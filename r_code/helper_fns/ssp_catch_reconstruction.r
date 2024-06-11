    

# ISC SHARKWG
# 2024/06/11
# catch reconstruction

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

ssp_catch_reconstruction = function(ssp_summary,samples_dt,stan_data,settings,effort_dt,sub_sample_prop=0.1,resample_raw_epsp="TRUE",catch_error=0.01){
      set.seed(settings$seed)
      tmp_samples = samples_dt[,.(run_id,iter,chain,chain_iter,variable,name,row,col,value)]
      sub_sample_n = round(max(tmp_samples$iter)*sub_sample_prop)
      if(sub_sample_n>max(tmp_samples$iter)){sub_sample_n=max(tmp_samples$iter)}
      if(sub_sample_n<1){sub_sample_n=1}
      sub_samples_dt = tmp_samples[iter %in% sample(1:max(tmp_samples$iter),sub_sample_n,replace=FALSE)]
      iter_sampled = sub_samples_dt[name=="x"&row==1]$iter

      logK = sub_samples_dt[name=="logK"]$value
      r = sub_samples_dt[name=="r"]$value
      x0 = sub_samples_dt[name=="x0"]$value
      x = dcast(sub_samples_dt[name=="x",.(iter,row,value)],iter~row) %>% .[,iter:=NULL] %>% as.matrix(.)
      raw_epsp = dcast(sub_samples_dt[name=="raw_epsp",.(iter,row,value)],iter~row) %>% .[,iter:=NULL] %>% as.matrix(.)
      sigmap2 = sub_samples_dt[name=="sigmap2"]$value
      sigmap = sqrt(sub_samples_dt[name=="sigmap2"]$value)
      n = sub_samples_dt[name=="n"]$value
      dmsy = (1/n)^(1/(n-1))
      h = 2*dmsy
      m = sub_samples_dt[name=="m"]$value
      g = sub_samples_dt[name=="g"]$value

    if(!("removals" %in% unique(samples_dt$name))){

      removals = stan_data[name=="removals"]$value

      catch_error_dfn = catch_error_ll = raw_hindcast_epsp = hindcast_epsp = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      if(resample_raw_epsp == "TRUE"){
        for(i in 1:nrow(raw_hindcast_epsp)){
                raw_hindcast_epsp[i,] = sample(raw_epsp[i,], size = nrow(effort_dt), replace = TRUE)
                hindcast_epsp[i,] = exp(raw_hindcast_epsp[i,]*sigmap[i]-(sigmap[i]^2)/2)
        }
      } else {
        for(j in 1:ncol(hindcast_epsp)){
                hindcast_epsp[,j] = rlnorm(length(iter_sampled),log(1.0)-(sigmap^2)/2,sigmap)
        }
        raw_hindcast_epsp = (log(hindcast_epsp) - (sigmap^2)/2)/sigmap
      }
      for(i in 1:nrow(catch_error_ll)){
            catch_error_ll[i,] = rev(exp(cumsum(rnorm(nrow(effort_dt),0,catch_error))))
            catch_error_dfn[i,which(!is.na(effort_dt$tans))] = rev(exp(cumsum(rnorm(length(which(!is.na(effort_dt$tans))),0,catch_error))))
            catch_error_dfn[i,which(is.na(catch_error_dfn[i,]))] = 1
      }
      
      # solve for q1 (longline) & q2 (dfn) needed to get depletion and catch in 1994
            optim_fn_a = function(par){
                  q1 = exp(par[1])

                  tmp_x = rep(NA,nrow(effort_dt))
                  tmp_x[1] = 1*tmp_epsp[1]
                  for(t in 2:nrow(effort_dt)) {
                        tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1]*tmp_cel[t-1])))*tmp_x[t-1]

                        if(tmp_x[t-1]<=tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_r * tmp_x[t-1] * (1 - tmp_x[t-1]/tmp_h) - tmp_C)*tmp_epsp[t]
                        if(tmp_x[t-1]> tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_g * tmp_m * tmp_x[t-1] * (1 - tmp_x[t-1]^(tmp_n-1)) - tmp_C)*tmp_epsp[t]
                  }
                  tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)]*tmp_cel[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)

                  obj_ssr = (tmp_x0-tmp_x[nrow(effort_dt)])^2
                  return(obj_ssr)
            }

            optim_fn_b = function(par){
                  q1 = exp(par[1])
                  q2 = exp(par[2])

                  tmp_x = rep(NA,nrow(effort_dt))
                  tmp_x[1] = 1*tmp_epsp[1]
                  for(t in 2:nrow(effort_dt)) {
                        tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1]*tmp_cel[t-1]+q1*q2*effort_dt$tans_sc[t-1]*tmp_ced[t-1])))*tmp_x[t-1]
                        # tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1])))*tmp_x[t-1]

                        if(tmp_x[t-1]<=tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_r * tmp_x[t-1] * (1 - tmp_x[t-1]/tmp_h) - tmp_C)*tmp_epsp[t]
                        if(tmp_x[t-1]> tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_g * tmp_m * tmp_x[t-1] * (1 - tmp_x[t-1]^(tmp_n-1)) - tmp_C)*tmp_epsp[t]
                  }
                  # tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)
                  tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)]*tmp_cel[nrow(effort_dt)]+q1*q2*effort_dt$tans_sc[nrow(effort_dt)]*tmp_ced[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)

                  obj_ssr = (1-(tmp_x[nrow(effort_dt)]/tmp_x0))^2 + (1-(tmp_C/removals[1]))^2
                  return(obj_ssr)
            }
      
      q1_vec = rep(NA,length(iter_sampled))
      q2_vec = rep(NA,length(iter_sampled))
      hindcast_x = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt)+length(removals))
      hindcast_removals = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt)+length(removals))
      catch_per_khooks = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      catch_per_ktan = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      catch_ll = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      catch_dfn = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      effort_ll = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      effort_dfn = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      iter=1
      for(i in 1:length(iter_sampled)){
            tmp_dmsy = dmsy[i]
            tmp_r = r[i]
            tmp_h = h[i]
            tmp_g = g[i]
            tmp_m = m[i]
            tmp_n = n[i]
            tmp_epsp = hindcast_epsp[i,]
            tmp_x0 = x0[i]
            tmp_logK = logK[i]
            tmp_cel = catch_error_ll[i,]
            tmp_ced = catch_error_dfn[i,]
            par_optim_a = optim(0,optim_fn_a,method="Brent",lower=-5,upper=5)
            par_optim_b = optim(c(par_optim_a$par,0),optim_fn_b)
            par_optim_c = optim(par_optim_b$par,optim_fn_b)

                  q1_vec[i] = q1 = exp(par_optim_c$par[1])
                  q2_vec[i] = q2 = exp(par_optim_c$par[2])


                  tmp_x = rep(NA,nrow(effort_dt))
                  tmp_x[1] = 1*tmp_epsp[1]
                  for(t in 2:nrow(effort_dt)) {
                        tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1]*tmp_cel[t-1]+q1*q2*effort_dt$tans_sc[t-1]*tmp_ced[t-1])))*tmp_x[t-1]
                        # tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1])))*tmp_x[t-1]
                        hindcast_removals[i,t-1] = tmp_C*exp(tmp_logK)
                        if(tmp_x[t-1]<=tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_r * tmp_x[t-1] * (1 - tmp_x[t-1]/tmp_h) - tmp_C)*tmp_epsp[t]
                        if(tmp_x[t-1]> tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_g * tmp_m * tmp_x[t-1] * (1 - tmp_x[t-1]^(tmp_n-1)) - tmp_C)*tmp_epsp[t]
                  }
                  # tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)
                  hindcast_removals[i,nrow(effort_dt)] = tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)]*tmp_cel[nrow(effort_dt)]+q1*q2*effort_dt$tans_sc[nrow(effort_dt)]*tmp_ced[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)
                  hindcast_x[i,1:nrow(effort_dt)] = tmp_x

                  catch_per_khooks[i,] = ((1-exp(-(q1*effort_dt$khooks_sc*tmp_cel)))*tmp_x*exp(tmp_logK))/(effort_dt$khooks*tmp_cel)
                  catch_per_ktan[i,] = ((1-exp(-(q1*q2*effort_dt$tans_sc*tmp_ced)))*tmp_x*exp(tmp_logK))/(effort_dt$tans*(1/1000)*tmp_ced)
                  catch_ll[i,] = ((1-exp(-(q1*effort_dt$khooks_sc*tmp_cel)))*tmp_x*exp(tmp_logK))
                  catch_dfn[i,] = ((1-exp(-(q1*q2*effort_dt$tans_sc*tmp_ced)))*tmp_x*exp(tmp_logK))
                  effort_ll[i,] = (effort_dt$khooks*tmp_cel)
                  effort_dfn[i,] = (effort_dt$tans*(1/1000)*tmp_ced)
                 
                  hindcast_removals[i,(nrow(effort_dt)+1):ncol(hindcast_removals)] = removals
                  hindcast_x[i,(nrow(effort_dt)+1):ncol(hindcast_x)] = x[i,]

                  if(hindcast_x[i,nrow(effort_dt)]<0.99*tmp_x0|hindcast_x[i,nrow(effort_dt)]>1.01*tmp_x0){
                        hindcast_removals[i,] = NA
                        hindcast_x[i,] = NA
                        catch_per_khooks[i,] = NA
                        catch_per_ktan[i,] = NA
                        catch_ll[i,] = NA
                        catch_dfn[i,] = NA 
                        iter=iter+1
                  }         
      }

    } else{
      removals = dcast(sub_samples_dt[name=="removals",.(iter,row,value)],iter~row) %>% .[,iter:=NULL] %>% as.matrix(.)
      data_T = stan_data[type=="Data"&name=="T"]$value
      if(ncol(removals)==data_T-1){
                  sigmac = stan_data[name=="sigmac"]$value
                  mu_catch = log(stan_data[name=="obs_removals"&row==data_T]$value) - 0.5*sigmac^2
                  removals = cbind(removals,rlnorm(nrow(removals),mu_catch,sigmac))
      }

      catch_error_dfn = catch_error_ll = raw_hindcast_epsp = hindcast_epsp = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      if(resample_raw_epsp == "TRUE"){
        for(i in 1:nrow(raw_hindcast_epsp)){
                raw_hindcast_epsp[i,] = sample(raw_epsp[i,], size = nrow(effort_dt), replace = TRUE)
                hindcast_epsp[i,] = exp(raw_hindcast_epsp[i,]*sigmap[i]-(sigmap[i]^2)/2)
        }
      } else {
        for(j in 1:ncol(hindcast_epsp)){
                hindcast_epsp[,j] = rlnorm(length(iter_sampled),log(1.0)-(sigmap^2)/2,sigmap)
        }
        raw_hindcast_epsp = (log(hindcast_epsp) - (sigmap^2)/2)/sigmap
      }
      for(i in 1:nrow(catch_error_ll)){
            catch_error_ll[i,] = rev(exp(cumsum(rnorm(nrow(effort_dt),0,catch_error))))
            catch_error_dfn[i,which(!is.na(effort_dt$tans))] = rev(exp(cumsum(rnorm(length(which(!is.na(effort_dt$tans))),0,catch_error))))
            catch_error_dfn[i,which(is.na(catch_error_dfn[i,]))] = 1
      }
      
      # solve for q1 (longline) & q2 (dfn) needed to get depletion and catch in 1994
            optim_fn_a = function(par){
                  q1 = exp(par[1])

                  tmp_x = rep(NA,nrow(effort_dt))
                  tmp_x[1] = 1*tmp_epsp[1]
                  for(t in 2:nrow(effort_dt)) {
                        tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1]*tmp_cel[t-1])))*tmp_x[t-1]

                        if(tmp_x[t-1]<=tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_r * tmp_x[t-1] * (1 - tmp_x[t-1]/tmp_h) - tmp_C)*tmp_epsp[t]
                        if(tmp_x[t-1]> tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_g * tmp_m * tmp_x[t-1] * (1 - tmp_x[t-1]^(tmp_n-1)) - tmp_C)*tmp_epsp[t]
                  }
                  tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)]*tmp_cel[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)

                  obj_ssr = (tmp_x0-tmp_x[nrow(effort_dt)])^2
                  return(obj_ssr)
            }

            optim_fn_b = function(par){
                  q1 = exp(par[1])
                  q2 = exp(par[2])

                  tmp_x = rep(NA,nrow(effort_dt))
                  tmp_x[1] = 1*tmp_epsp[1]
                  for(t in 2:nrow(effort_dt)) {
                        tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1]*tmp_cel[t-1]+q1*q2*effort_dt$tans_sc[t-1]*tmp_ced[t-1])))*tmp_x[t-1]
                        # tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1])))*tmp_x[t-1]

                        if(tmp_x[t-1]<=tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_r * tmp_x[t-1] * (1 - tmp_x[t-1]/tmp_h) - tmp_C)*tmp_epsp[t]
                        if(tmp_x[t-1]> tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_g * tmp_m * tmp_x[t-1] * (1 - tmp_x[t-1]^(tmp_n-1)) - tmp_C)*tmp_epsp[t]
                  }
                  # tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)
                  tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)]*tmp_cel[nrow(effort_dt)]+q1*q2*effort_dt$tans_sc[nrow(effort_dt)]*tmp_ced[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)

                  obj_ssr = (1-(tmp_x[nrow(effort_dt)]/tmp_x0))^2 + (1-(tmp_C/tmp_removals[1]))^2
                  return(obj_ssr)
            }
      
      q1_vec = rep(NA,length(iter_sampled))
      q2_vec = rep(NA,length(iter_sampled))
      hindcast_x = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt)+ncol(removals))
      hindcast_removals = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt)+ncol(removals))
      catch_per_khooks = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      catch_per_ktan = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      catch_ll = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      catch_dfn = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      effort_ll = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      effort_dfn = matrix(NA,nrow=length(iter_sampled),ncol=nrow(effort_dt))
      iter=1
      for(i in 1:length(iter_sampled)){
            tmp_dmsy = dmsy[i]
            tmp_r = r[i]
            tmp_h = h[i]
            tmp_g = g[i]
            tmp_m = m[i]
            tmp_n = n[i]
            tmp_epsp = hindcast_epsp[i,]
            tmp_x0 = x0[i]
            tmp_logK = logK[i]
            tmp_cel = catch_error_ll[i,]
            tmp_ced = catch_error_dfn[i,]
            tmp_removals = removals[i,]
            par_optim_a = optim(0,optim_fn_a,method="Brent",lower=-5,upper=5)
            par_optim_b = optim(c(par_optim_a$par,0),optim_fn_b)
            par_optim_c = optim(par_optim_b$par,optim_fn_b)

                  q1_vec[i] = q1 = exp(par_optim_c$par[1])
                  q2_vec[i] = q2 = exp(par_optim_c$par[2])


                  tmp_x = rep(NA,nrow(effort_dt))
                  tmp_x[1] = 1*tmp_epsp[1]
                  for(t in 2:nrow(effort_dt)) {
                        tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1]*tmp_cel[t-1]+q1*q2*effort_dt$tans_sc[t-1]*tmp_ced[t-1])))*tmp_x[t-1]
                        # tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[t-1])))*tmp_x[t-1]
                        hindcast_removals[i,t-1] = tmp_C*exp(tmp_logK)
                        if(tmp_x[t-1]<=tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_r * tmp_x[t-1] * (1 - tmp_x[t-1]/tmp_h) - tmp_C)*tmp_epsp[t]
                        if(tmp_x[t-1]> tmp_dmsy) tmp_x[t] = (tmp_x[t-1] + tmp_g * tmp_m * tmp_x[t-1] * (1 - tmp_x[t-1]^(tmp_n-1)) - tmp_C)*tmp_epsp[t]
                  }
                  # tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)
                  hindcast_removals[i,nrow(effort_dt)] = tmp_C = (1-exp(-(q1*effort_dt$khooks_sc[nrow(effort_dt)]*tmp_cel[nrow(effort_dt)]+q1*q2*effort_dt$tans_sc[nrow(effort_dt)]*tmp_ced[nrow(effort_dt)])))*tmp_x[nrow(effort_dt)]*exp(tmp_logK)
                  hindcast_x[i,1:nrow(effort_dt)] = tmp_x

                  catch_per_khooks[i,] = ((1-exp(-(q1*effort_dt$khooks_sc*tmp_cel)))*tmp_x*exp(tmp_logK))/(effort_dt$khooks*tmp_cel)
                  catch_per_ktan[i,] = ((1-exp(-(q1*q2*effort_dt$tans_sc*tmp_ced)))*tmp_x*exp(tmp_logK))/(effort_dt$tans*(1/1000)*tmp_ced)
                  catch_ll[i,] = ((1-exp(-(q1*effort_dt$khooks_sc*tmp_cel)))*tmp_x*exp(tmp_logK))
                  catch_dfn[i,] = ((1-exp(-(q1*q2*effort_dt$tans_sc*tmp_ced)))*tmp_x*exp(tmp_logK))
                  effort_ll[i,] = (effort_dt$khooks*tmp_cel)
                  effort_dfn[i,] = (effort_dt$tans*(1/1000)*tmp_ced)
                 
                  hindcast_removals[i,(nrow(effort_dt)+1):ncol(hindcast_removals)] = tmp_removals
                  hindcast_x[i,(nrow(effort_dt)+1):ncol(hindcast_x)] = x[i,]

                  if(hindcast_x[i,nrow(effort_dt)]<0.99*tmp_x0|hindcast_x[i,nrow(effort_dt)]>1.01*tmp_x0){
                        hindcast_removals[i,] = NA
                        hindcast_x[i,] = NA
                        catch_per_khooks[i,] = NA
                        catch_per_ktan[i,] = NA
                        catch_ll[i,] = NA
                        catch_dfn[i,] = NA 
                        iter=iter+1
                  }         
      }      

    }

      # report out
      matrix_var = c("catch_per_khooks",
            "catch_per_ktan",
            "catch_ll",
            "catch_dfn",
            "effort_ll",
            "effort_dfn", "catch_error_ll", "catch_error_dfn",
            "raw_hindcast_epsp",
            "hindcast_x", "hindcast_removals")

      catch_error_ll = log(catch_error_ll)
      catch_error_dfn = log(catch_error_dfn)
      matrix_dt.list = as.list(rep(NA,length(matrix_var)))
      for(i in 1:length(matrix_dt.list)){
            matrix_dt.list[[i]] = as.data.table(get(matrix_var[i])) %>%
                                  .[,iter:=iter_sampled] %>%
                                  melt(.,id.vars=c("iter")) %>%
                                  .[,row:=as.numeric(gsub("V","",variable))] %>%
                                  .[,row:=row-nrow(effort_dt)] %>%
                    .[,run_id:=ssp_summary$run_id] %>%
                    .[,variable:=paste0(matrix_var[i],"[",row,"]")] %>%
                    .[,name:=matrix_var[i]] %>%
                    .[,col:=as.numeric(NA)] %>%
                    merge(.,unique(samples_dt[,.(iter,chain,chain_iter)]),by="iter") %>%
                    .[,.(run_id,iter,chain,chain_iter,variable,name,row,col,value)] %>%
                    .[order(chain,iter)]
      }

      out = rbindlist(matrix_dt.list)

      return(out)
}    
