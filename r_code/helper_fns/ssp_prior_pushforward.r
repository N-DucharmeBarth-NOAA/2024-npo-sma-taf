    

# ISC SHARKWG
# 2024/06/11
# Prior pushforward for stochastic surplus production model

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

ssp_prior_pushforward = function(ssp_summary,stan_data,settings){
      if(ssp_summary$exec == "bspm_F_est"){
      set.seed(settings$seed)
      chains = settings$chains
      n_samples = settings$iter_keep*chains

      logK = rnorm(n_samples,ssp_summary$PriorMean_logK,ssp_summary$PriorSD_logK)
      raw_logK = (logK - ssp_summary$PriorMean_logK)/ssp_summary$PriorSD_logK
      r = exp(rnorm(n_samples,ssp_summary$PriorMean_logr,ssp_summary$PriorSD_logr))
      raw_logr = (log(r) - ssp_summary$PriorMean_logr)/ssp_summary$PriorSD_logr
      x0 = exp(rnorm(n_samples,ssp_summary$PriorMean_logx0,ssp_summary$PriorSD_logx0))
      raw_logx0 = (log(x0) - ssp_summary$PriorMean_logx0)/ssp_summary$PriorSD_logx0

      sigmap = exp(rnorm(n_samples,stan_data[name=="logsigmap"&type=="PriorMean"]$value,stan_data[name=="logsigmap"&type=="PriorSD"]$value))
      raw_logsigmap = (log(sigmap) - stan_data[name=="logsigmap"&type=="PriorMean"]$value)/stan_data[name=="logsigmap"&type=="PriorSD"]$value

      shape = exp(rnorm(n_samples,stan_data[name=="logshape"&type=="PriorMean"]$value,stan_data[name=="logshape"&type=="PriorSD"]$value))
      raw_logshape = (log(shape) - stan_data[name=="logshape"&type=="PriorMean"]$value)/stan_data[name=="logshape"&type=="PriorSD"]$value

      raw_sigmao_add = abs(rnorm(n_samples))
      sigmao_add = raw_sigmao_add*stan_data[name=="sigmao_add"&type=="PriorSD"]$value
      sigmao_sc = stan_data[name=="sigmao_input"]$value + sigmao_add

      raw_sigmaf = abs(rnorm(n_samples))
      F = raw_F = matrix(NA,nrow=n_samples,ncol=stan_data[name=="T"]$value-1)
      sigmaf = raw_sigmaf*stan_data[name=="sigmaf"]$value


      raw_epsp = epsp = matrix(NA,nrow=n_samples,ncol=stan_data[name=="T"]$value-1)
      for(j in 1:ncol(epsp)){
            epsp[,j] = rlnorm(n_samples,log(1.0)-(sigmap^2)/2,sigmap)
            raw_F[,j] = abs(rnorm(n_samples))
            F[,j] = raw_F[,j]*sigmaf
      }

      raw_epsp = (log(epsp) - (sigmap^2)/2)/sigmap

      sigmap2 = sigmap^2
      n = shape
      dmsy = (1/n)^(1/(n-1))
      h = 2*dmsy
      m = r*h/4
      g = (n^(n/(n-1)))/(n-1)

      x = matrix(NA,nrow=n_samples,ncol=ncol(epsp)+1)
      x[,1] = x0
      T = ncol(x)
      removals = matrix(NA,nrow=n_samples,ncol=stan_data[name=="T"]$value-1)

      for(i in 1:nrow(x)){
            for(t in 2:T) {
                  if(x[i,t-1]<=dmsy[i]){
                  x[i,t] = (x[i,t-1] + r[i] * x[i,t-1] * (1 - x[i,t-1]/h[i]))*(exp(-F[i,t-1]))*epsp[i,t-1]
                  removals[i,t-1] = ((x[i,t-1] + r[i] * x[i,t-1] * (1 - x[i,t-1]/h[i])))*epsp[i,t-1]*(1-exp(-F[i,t-1]))*exp(logK[i])
                  } else {
                  x[i,t] = (x[i,t-1] + g[i] * m[i] * x[i,t-1] * (1 - x[i,t-1]^(n[i]-1)))*(exp(-F[i,t-1]))*epsp[i,t-1]
                  removals[i,t-1] = ((x[i,t-1] + g[i] * m[i] * x[i,t-1] * (1 - x[i,t-1]^(n[i]-1))))*epsp[i,t-1]*(1-exp(-F[i,t-1]))*exp(logK[i]);
                  }
            }
      }

      vector_var = c("raw_logK","raw_logr","raw_logx0","raw_logsigmap","raw_logshape","logK","r","x0","m","sigmap","sigmap2","sigmao_sc","shape","n","dmsy","h","g","sigmao_add","sigmaf")
      vector_dt.list = as.list(rep(NA,length(vector_var)))
      for(i in 1:length(vector_dt.list)){
            vector_dt.list[[i]] = data.table(value=get(vector_var[i])) %>%
                    .[,run_id:=ssp_summary$run_id] %>%
                    .[,iter:=1:n_samples] %>%
                    .[,chain:=sort(rep(1:chains,n_samples/chains))] %>%
                    .[,chain_iter:=rep(1:(n_samples/chains),chains)] %>%
                    .[,variable:=vector_var[i]] %>%
                    .[,name:=vector_var[i]] %>%
                    .[,row:=as.numeric(NA)] %>%
                    .[,col:=as.numeric(NA)] %>%
                    .[,.(run_id,iter,chain,chain_iter,variable,name,row,col,value)] %>%
                    .[order(chain,iter)]
      }
      vector_dt = rbindlist(vector_dt.list)

      matrix_var = c("raw_epsp","x","removals")
      matrix_dt.list = as.list(rep(NA,length(matrix_var)))
      for(i in 1:length(matrix_dt.list)){
            matrix_dt.list[[i]] = as.data.table(get(matrix_var[i])) %>%
                                  .[,iter:=1:n_samples] %>%
                                  .[,chain:=sort(rep(1:chains,n_samples/chains))] %>%
                                  .[,chain_iter:=rep(1:(n_samples/chains),chains)] %>%
                                  melt(.,id.vars=c("iter","chain","chain_iter")) %>%
                                  .[,row:=as.numeric(gsub("V","",variable))] %>%
                    .[,run_id:=ssp_summary$run_id] %>%
                    .[,variable:=paste0(matrix_var[i],"[",row,"]")] %>%
                    .[,name:=matrix_var[i]] %>%
                    .[,col:=as.numeric(NA)] %>%
                    .[,.(run_id,iter,chain,chain_iter,variable,name,row,col,value)] %>%
                    .[order(chain,iter)]
      }
      matrix_dt = rbindlist(matrix_dt.list)

      out = rbind(vector_dt,matrix_dt)

      return(out)      
    } else if(ssp_summary$exec == "bspm_F_llq"){
      set.seed(settings$seed)
      chains = settings$chains
      n_samples = settings$iter_keep*chains

      logK = rnorm(n_samples,ssp_summary$PriorMean_logK,ssp_summary$PriorSD_logK)
      raw_logK = (logK - ssp_summary$PriorMean_logK)/ssp_summary$PriorSD_logK
      r = exp(rnorm(n_samples,ssp_summary$PriorMean_logr,ssp_summary$PriorSD_logr))
      raw_logr = (log(r) - ssp_summary$PriorMean_logr)/ssp_summary$PriorSD_logr
      x0 = exp(rnorm(n_samples,ssp_summary$PriorMean_logx0,ssp_summary$PriorSD_logx0))
      raw_logx0 = (log(x0) - ssp_summary$PriorMean_logx0)/ssp_summary$PriorSD_logx0

      sigmap = exp(rnorm(n_samples,stan_data[name=="logsigmap"&type=="PriorMean"]$value,stan_data[name=="logsigmap"&type=="PriorSD"]$value))
      raw_logsigmap = (log(sigmap) - stan_data[name=="logsigmap"&type=="PriorMean"]$value)/stan_data[name=="logsigmap"&type=="PriorSD"]$value

      shape = exp(rnorm(n_samples,stan_data[name=="logshape"&type=="PriorMean"]$value,stan_data[name=="logshape"&type=="PriorSD"]$value))
      raw_logshape = (log(shape) - stan_data[name=="logshape"&type=="PriorMean"]$value)/stan_data[name=="logshape"&type=="PriorSD"]$value

      raw_sigmao_add = abs(rnorm(n_samples))
      sigmao_add = raw_sigmao_add*stan_data[name=="sigmao_add"&type=="PriorSD"]$value
      sigmao_sc = stan_data[name=="sigmao_input"]$value + sigmao_add

      raw_epsp = epsp = matrix(NA,nrow=n_samples,ncol=stan_data[name=="T"]$value-1)
      for(j in 1:ncol(epsp)){
            epsp[,j] = rlnorm(n_samples,log(1.0)-(sigmap^2)/2,sigmap)
      }

      raw_epsp = (log(epsp) - (sigmap^2)/2)/sigmap

      raw_logll_q = rnorm(n_samples)
      ll_q = exp(raw_logll_q*stan_data[name=="logll_q"&type=="PriorSD"]$value+stan_data[name=="logll_q"&type=="PriorMean"]$value)

      sigmap2 = sigmap^2
      n = shape
      dmsy = (1/n)^(1/(n-1))
      h = 2*dmsy
      m = r*h/4
      g = (n^(n/(n-1)))/(n-1)

      removals= x = matrix(NA,nrow=n_samples,ncol=ncol(epsp)+1)
      x[,1] = x0
      T = ncol(x)


      non_ll_removals = stan_data[name=="non_ll_removals"]$value
      ll_effort = stan_data[name=="ll_effort"]$value
      ll_effort_sc = ll_effort/max(ll_effort)

      for(i in 1:nrow(x)){
            total_u = rep(NA,T)
            total_u[1] = 1-exp(-((ll_q[i]*ll_effort_sc[1])+(non_ll_removals[1]/(x[i,1]*exp(logK[i])))))
            removals[i,1] = total_u[1]*x[i,1]*exp(logK[i])

            for(t in 2:T) {
                  C = min(c(exp(log(removals[i,t-1]) - logK[i]),x[i,t-1]))
                  if(x[i,t-1]<=dmsy[i]){
                  x[i,t] = (x[i,t-1] + r[i] * x[i,t-1] * (1 - x[i,t-1]/h[i]) - C)*epsp[i,t-1];
                  } else {
                  x[i,t] = (x[i,t-1] + g[i] * m[i] * x[i,t-1] * (1 - x[i,t-1]^(n[i]-1)) - C)*epsp[i,t-1];
                  }
                  total_u[t] = 1-exp(-((ll_q[i]*ll_effort_sc[t])+(non_ll_removals[t]/(x[i,t]*exp(logK[i])))));
                  removals[i,t] = total_u[t]*x[i,t]*exp(logK[i])
            }
      }

      vector_var = c("raw_logK","raw_logr","raw_logx0","raw_logsigmap","raw_logshape","logK","r","x0","m","sigmap","sigmap2","sigmao_sc","shape","n","dmsy","h","g","raw_logll_q","ll_q","sigmao_add")
      vector_dt.list = as.list(rep(NA,length(vector_var)))
      for(i in 1:length(vector_dt.list)){
            vector_dt.list[[i]] = data.table(value=get(vector_var[i])) %>%
                    .[,run_id:=ssp_summary$run_id] %>%
                    .[,iter:=1:n_samples] %>%
                    .[,chain:=sort(rep(1:chains,n_samples/chains))] %>%
                    .[,chain_iter:=rep(1:(n_samples/chains),chains)] %>%
                    .[,variable:=vector_var[i]] %>%
                    .[,name:=vector_var[i]] %>%
                    .[,row:=as.numeric(NA)] %>%
                    .[,col:=as.numeric(NA)] %>%
                    .[,.(run_id,iter,chain,chain_iter,variable,name,row,col,value)] %>%
                    .[order(chain,iter)]
      }
      vector_dt = rbindlist(vector_dt.list)

      matrix_var = c("raw_epsp","x","removals")
      matrix_dt.list = as.list(rep(NA,length(matrix_var)))
      for(i in 1:length(matrix_dt.list)){
            matrix_dt.list[[i]] = as.data.table(get(matrix_var[i])) %>%
                                  .[,iter:=1:n_samples] %>%
                                  .[,chain:=sort(rep(1:chains,n_samples/chains))] %>%
                                  .[,chain_iter:=rep(1:(n_samples/chains),chains)] %>%
                                  melt(.,id.vars=c("iter","chain","chain_iter")) %>%
                                  .[,row:=as.numeric(gsub("V","",variable))] %>%
                    .[,run_id:=ssp_summary$run_id] %>%
                    .[,variable:=paste0(matrix_var[i],"[",row,"]")] %>%
                    .[,name:=matrix_var[i]] %>%
                    .[,col:=as.numeric(NA)] %>%
                    .[,.(run_id,iter,chain,chain_iter,variable,name,row,col,value)] %>%
                    .[order(chain,iter)]
      }
      matrix_dt = rbindlist(matrix_dt.list)

      out = rbind(vector_dt,matrix_dt)

      return(out)
    } else {
      stop("Prior pushforward not developed for this version of the model.")
    }
}    
