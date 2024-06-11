

# ISC SHARKWG
# 2024/06/06
# Run the model ensemble

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


#________________________________________________________________________________________________________________________________________________________________________________________________________
# load packages
    library(TAF)
    library(data.table)
    library(magrittr)
    library(rstan)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# source helper functions
    source_dir_stem = "./boot/software/"
    source_files = list.files(source_dir_stem)
    source_files = source_files[grep(".r",source_files,fixed=TRUE)]
    sapply(paste0(source_dir_stem,source_files),source)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# make directory for model outputs
    mkdir("model")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# load inputs
    index_mat = fread("./data/index_mat.csv") %>% as.data.frame(.) %>% as.matrix(.)
    se_mat = fread("./data/se_mat.csv") %>% as.data.frame(.) %>% as.matrix(.)
    prior_dt = fread("./data/prior_dt.csv")
    status_quo_catch = fread("./data/status_quo_catch.csv")
    recent_non_ll_catch = fread("./data/recent_non_ll_catch.csv")
    npo_ll_effort = fread("./data/npo_ll_effort.csv")
    ssp_scenarios = fread("./data/ssp_scenarios.csv")
    
#________________________________________________________________________________________________________________________________________________________________________________________________________
# compile executable
    exec_name_vec = c("bspm_F_llq","bspm_F_est")
    stan_c.list = as.list(rep(NA,length(exec_name_vec)))
    for(i in 1:length(exec_name_vec)){
        file.name = paste0("./boot/software/",exec_name_vec[i],".stan") # path to stan file
        stan_c.list[[i]] = stan_model(file=file.name, model_name = exec_name_vec[i])
    }

#________________________________________________________________________________________________________________________________________________________________________________________________________
# iterate over model configurations and run
    for(i in 1:nrow(ssp_scenarios)){
                run_label_stem = paste0(c(ssp_scenarios$index[i],ssp_scenarios$main_prior[i],ssp_scenarios$logK_prior[i],ssp_scenarios$sigmap_prior[i],ssp_scenarios$catch[i]),collapse=".")    
                if(i<10){
                    run_number = paste0("000",i)
                } else if(i<100) {
                    run_number = paste0("00",i)
                } 

                # define exec_name and stan_c
                    if(ssp_scenarios$catch[i]=="ELL"){
                        exec_name = exec_name_vec[1]
                        stan_c = stan_c.list[[1]]
                    } else {
                        exec_name = exec_name_vec[2]
                        stan_c = stan_c.list[[2]]
                    }

                
                # change priors
                        if(ssp_scenarios$main_prior[i]=="B"){
                                tmp.data.priors = list(PriorMean_logK = prior_dt[group=="Base"&type=="logK"&parameter=="mean"]$value,
                                PriorSD_logK = prior_dt[group=="Base"&type=="logK"&parameter=="log_sd"]$value,
                                PriorMean_logr = prior_dt[group=="Base"&type=="logr"&parameter=="mean"]$value,
                                PriorSD_logr = prior_dt[group=="Base"&type=="logr"&parameter=="log_sd"]$value,
                                PriorMean_logx0 = prior_dt[group=="Base"&type=="logx0"&parameter=="mean"]$value,
                                PriorSD_logx0 = prior_dt[group=="Base"&type=="logx0"&parameter=="log_sd"]$value,
                                PriorMean_logsigmap = prior_dt[group=="Base"&type=="logsigmap"&parameter=="mean"]$value,
                                PriorSD_logsigmap = prior_dt[group=="Base"&type=="logsigmap"&parameter=="log_sd"]$value,
                                PriorMean_logshape = prior_dt[group=="Base"&type=="logshape"&parameter=="mean"]$value,
                                PriorSD_logshape = prior_dt[group=="Base"&type=="logshape"&parameter=="log_sd"]$value)
                        } else {
                                tmp.data.priors = list(PriorMean_logK = prior_dt[group=="Base"&type=="logK"&parameter=="mean"]$value,
                                PriorSD_logK = prior_dt[group=="Base"&type=="logK"&parameter=="log_sd"]$value,
                                PriorMean_logr = prior_dt[group=="Extreme"&type=="logr"&parameter=="mean"]$value,
                                PriorSD_logr = prior_dt[group=="Extreme"&type=="logr"&parameter=="log_sd"]$value,
                                PriorMean_logx0 = prior_dt[group=="Extreme"&type=="logx0"&parameter=="mean"]$value,
                                PriorSD_logx0 = prior_dt[group=="Extreme"&type=="logx0"&parameter=="log_sd"]$value,
                                PriorMean_logsigmap = prior_dt[group=="Base"&type=="logsigmap"&parameter=="mean"]$value,
                                 PriorSD_logsigmap = prior_dt[group=="Base"&type=="logsigmap"&parameter=="log_sd"]$value,
                                PriorMean_logshape = prior_dt[group=="Extreme"&type=="logshape"&parameter=="mean"]$value,
                                PriorSD_logshape = prior_dt[group=="Extreme"&type=="logshape"&parameter=="log_sd"]$value)
                        }
                    
                    # change data - sigmao_input & catch
                        if(ssp_scenarios$catch[i]=="ELL"){
                            tmp_data = list(T=as.integer(nrow(index_mat)),
                            I = as.integer(ncol(index_mat)),
                            index=index_mat,
                            non_ll_removals=recent_non_ll_catch$catch_n,
                            ll_effort=npo_ll_effort$khooks,
                            sigmao_mat=se_mat,
                            PriorSD_sigmao_add = 0.2,
                            PriorMean_logll_q = prior_dt[group=="Base"&type=="logll_q"&parameter=="mean"]$value,
                            PriorSD_logll_q = prior_dt[group=="Base"&type=="logll_q"&parameter=="log_sd"]$value,
                            lambdas=as.integer(c(0,0,0,0,0,0,0)))

                            tmp_data$lambdas[ssp_scenarios$index[i]] = 1
                            if(ssp_scenarios$index[i]%in%c(4:5)){
                                tmp_data$sigmao_input =  0.2 # floor
                            } else {
                                tmp_data$sigmao_input =  0.33 # floor from model
                            }
                        } else if(ssp_scenarios$catch[i]=="EC"){
                            tmp_data = list(T=as.integer(nrow(index_mat)),
                            I = as.integer(ncol(index_mat)),
                            index=index_mat,
                            sigmao_mat=se_mat,
                            PriorSD_sigmao_add = 0.2,
                            lambdas=as.integer(c(0,0,0,0,0,0,0)))

                            tmp_data$lambdas[ssp_scenarios$index[i]] = 1
                            if(ssp_scenarios$index[i]%in%c(4:5)){
                                tmp_data$sigmao_input =  0.2 # floor
                            } else {
                                tmp_data$sigmao_input =  0.33 # floor from model
                            }
                            
                            tmp_removals=status_quo_catch$removals
                            tmp_data$obs_removals = tmp_removals
                            tmp_data$sigmac = 0.5
                            tmp_data$PriorSD_sigmaf = 0.05
                        } else if(ssp_scenarios$catch[i]=="EC2") {
                            tmp_data = list(T=as.integer(nrow(index_mat)),
                            I = as.integer(ncol(index_mat)),
                            index=index_mat,
                            sigmao_mat=se_mat,
                            PriorSD_sigmao_add = 0.2,
                            lambdas=as.integer(c(0,0,0,0,0,0,0)))

                            tmp_data$lambdas[ssp_scenarios$index[i]] = 1
                            if(ssp_scenarios$index[i]%in%c(4:5)){
                                tmp_data$sigmao_input =  0.2 # floor
                            } else {
                                tmp_data$sigmao_input =  0.33 # floor from model
                            }
                            
                            tmp_removals=status_quo_catch$removals
                            tmp_data$obs_removals = tmp_removals
                            tmp_data$sigmac = 0.5
                            tmp_data$PriorSD_sigmaf = 0.025                        
                        } else {
                            tmp_data = list(T=as.integer(nrow(index_mat)),
                            I = as.integer(ncol(index_mat)),
                            index=index_mat,
                            sigmao_mat=se_mat,
                            PriorSD_sigmao_add = 0.2,
                            lambdas=as.integer(c(0,0,0,0,0,0,0)))

                            tmp_data$lambdas[ssp_scenarios$index[i]] = 1
                            if(ssp_scenarios$index[i]%in%c(4:5)){
                                tmp_data$sigmao_input =  0.2 # floor
                            } else {
                                tmp_data$sigmao_input =  0.33 # floor from model
                            }
                            
                            tmp_removals=status_quo_catch$removals
                            tmp_data$obs_removals = tmp_removals
                            tmp_data$sigmac = 0.5
                            tmp_data$PriorSD_sigmaf = 0.0125
                        }
                        
                    tmp_data = c(tmp_data,tmp.data.priors)

                    fit = try(fit_rstan(tmp_data,
                            stan_c,
                            run_label = paste0(run_number,"_",run_label_stem,"_0"),
                            exec_name = exec_name,
                            seed  = 321,
                            chains = 5,
                            n_thin = 10,
                            iter_keep = 200,
                            burnin.prop = 0.5,
                            adapt_delta = 0.99,
                            max_treedepth = 15,
                            silent = FALSE,
                            stan_save_dir="./model/",
                            n_cores=1),silent=TRUE)
    }

