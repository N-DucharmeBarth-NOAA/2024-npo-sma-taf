

# ISC SHARKWG
# 2024/06/12
# Plot some results from the model ensemble

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


#________________________________________________________________________________________________________________________________________________________________________________________________________
# load packages
    library(TAF)
    library(data.table)
    library(magrittr)
    library(ggplot2)
    library(viridis)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# source helper functions
    source_dir_stem = "./boot/software/"
    source_files = list.files(source_dir_stem)
    source_files = source_files[grep(".r",source_files,fixed=TRUE)]
    sapply(paste0(source_dir_stem,source_files),source)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# make directory for plots
    mkdir("report")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# bring in data
    ssp_scenarios = fread("./data/ssp_scenarios.csv")


#________________________________________________________________________________________________________________________________________________________________________________________________________
# extract quantities from each model
    summary.list = as.list(rep(NA,nrow(ssp_scenarios)))
    samples.list = as.list(rep(NA,nrow(ssp_scenarios)))
    samples_ts.list = as.list(rep(NA,nrow(ssp_scenarios)))
    prior_posterior.list = as.list(rep(NA,nrow(ssp_scenarios)))
    fit_dt.list = as.list(rep(NA,nrow(ssp_scenarios)))
    for(i in 1:nrow(ssp_scenarios)){
        # identify target model
            run_label_stem = paste0(c(ssp_scenarios$index[i],ssp_scenarios$main_prior[i],ssp_scenarios$logK_prior[i],ssp_scenarios$sigmap_prior[i],ssp_scenarios$catch[i]),collapse=".")    
            if(i<10){
                run_number = paste0("000",i)
            } else if(i<100) {
                run_number = paste0("00",i)
            }
            run_label = paste0(run_number,"_",run_label_stem,"_0")

        # get model summary
            summary.list[[i]] = fread(paste0("output/",run_label,"/diagnostics.csv")) %>%
                                .[,model_number:=as.numeric(strsplit(run_label,"_",fixed=TRUE)[[1]][1])] %>%
                                .[,.(run_label,model_number,index,main_prior,catch,divergent,max_rhat,min_neff,index_rmse,index_loglik)]
        
        # get inputs
            tmp_summary = fread(paste0("model/",run_label,"/fit_summary.csv"))
            tmp_samples = fread(paste0("model/",run_label,"/hmc_samples.csv"))
            tmp_settings = fread(paste0("model/",run_label,"/settings.csv"))
            tmp_data = fread(paste0("model/",run_label,"/stan_data.csv"))

            leading_params = tmp_samples[name%in%c("logK","r","x0","n","sigmap","sigmao_add","dmsy","sigmao_sc","sigmaf","ll_q"),.(run_id,iter,name,value)]
        
        # get derived quantities
            samples.list[[i]] = ssp_derived_quants(hmc_sample = tmp_samples,
                                                        stan_data=tmp_data,
                                                        output="samples") %>%
                                    melt(.,id.vars=c("run_id","iter")) %>%
                                    setnames(.,"variable","name") %>%
                                    rbind(.,leading_params) %>%
                            .[name %in% c("msy","Umsy","latest_depletion","logK","r","x0","n","sigmap","dmsy","sigmao_sc","sigmao_add","sigmaf","ll_q"),.(iter,name,value)] %>%
                            .[,run_label:=run_label] %>%
                            merge(.,summary.list[[i]][,.(run_label,index,main_prior,catch,divergent,max_rhat,min_neff)],by="run_label")
        
        # get priors
            posterior = samples.list[[i]] %>%
                        .[name%in%c("logK","r","x0","n","sigmap","sigmao_add","dmsy","sigmao_sc","sigmaf","ll_q"),.(run_label,iter,name,value)] %>%
                        .[name=="logK",value:=exp(value)] %>%
                        .[name=="logK",name:="K"] %>%
                        .[,type:="posterior"]
            prior = ssp_prior_pushforward(ssp_summary=tmp_summary,stan_data=tmp_data,settings=tmp_settings) %>%
                    .[,run_label:=run_label] %>%
                    .[name%in%c("logK","r","x0","n","sigmap","sigmao_add","dmsy","sigmao_sc","sigmaf","ll_q"),.(run_label,iter,name,value)] %>%
                    .[name=="logK",value:=exp(value)] %>%
                    .[name=="logK",name:="K"] %>%
                    .[,type:="prior"]
            
            prior_posterior.list[[i]] = rbind(posterior,prior)

        # get time-series
            tmp_ts = ssp_derived_quants_ts(ssp_summary=tmp_summary,
                                    samples_dt=tmp_samples,
                                    settings=tmp_settings,
                                    stan_data=tmp_data,
                                    sub_sample_prop=1)
            samples_ts.list[[i]] = tmp_ts %>%
                                    .[name%in%c("removals","U","D","D_Dmsy","U_Umsy"),.(iter,name,row,value)] %>%
                                    .[,run_label:=run_label] %>%
                                    merge(.,summary.list[[i]][,.(run_label,index,main_prior,catch,divergent,max_rhat,min_neff)],by="run_label")
            
        # get management quantities
            tmp_f = ssp_derived_quants_init_recent(tmp_summary,tmp_ts,years_avg=5,exclude_last=1)[variable%in%c("recent_U","recent_U_Umsy")] %>%
                        setnames(.,"variable","name") %>%
                        .[,.(iter,name,value)] %>%
                        .[,run_label:=run_label]
            tmp_dep = ssp_derived_quants_init_recent(tmp_summary,tmp_ts,years_avg=4,exclude_last=0)[variable%in%c("recent_D","recent_D_Dmsy")] %>%
                        setnames(.,"variable","name") %>%
                        .[,.(iter,name,value)] %>%
                        .[,run_label:=run_label]
            msy_dt = merge(summary.list[[i]][,.(run_label,index,main_prior,catch,divergent,max_rhat,min_neff)],rbind(tmp_f,tmp_dep),by="run_label") %>%
                        .[,.(run_label,iter,name,value,index,main_prior,catch,divergent,max_rhat,min_neff)]
            samples.list[[i]] = rbind(samples.list[[i]],msy_dt)

        # get fits
            fit_dt.list[[i]] = ssp_extract_cpue_fit(ssp_summary=tmp_summary,
                                    samples_dt=tmp_samples,
                                    stan_data=tmp_data,
                                    settings=tmp_settings,
                                    sub_sample_prop=1,
                                    active="TRUE",
                                    calc_std = "FALSE") %>%
                                    .[,run_label:=run_label] %>%
                                merge(.,summary.list[[i]][,.(run_label,main_prior,catch,divergent,max_rhat,min_neff)],by="run_label") %>%
                                .[index==summary.list[[i]]$index]   
    }

    summary_dt = rbindlist(summary.list)
    samples = rbindlist(samples.list)
    samples_ts = rbindlist(samples_ts.list)
    prior_posterior = rbindlist(prior_posterior.list)
    fit_dt = rbindlist(fit_dt.list)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# apply convergence criteria and model weighting 


#________________________________________________________________________________________________________________________________________________________________________________________________________
# save output


#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot fit

#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot management distributions

#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot time series

#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot kobe
