

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
                                    .[name%in%c("P","removals","U","D","D_Dmsy","U_Umsy"),.(iter,name,row,value)] %>%
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
# Convergence criteria: max_rhat<=1.01&min_neff>=500&divergent==0
# Models run for stock assessment used R version 4.3.1 (and associated libraries)
# Models 5, 8, 12, and 30 failed convergence criteria under that R version
# R version 4.4.0 was used with this repository
# Under this version, model 16 also failed convergence criteria
    summary_dt.original = summary_dt[!(model_number %in% c(5,8,12,30))] # to use converged models from assessment report
    summary_dt.alternative = summary_dt[max_rhat<=1.01&min_neff>=500&divergent==0] # to apply convergence criteria to models run with R 4.4.0

    ensemble = rbind(samples[run_label%in%summary_dt.original$run_label&catch=="ELL"&index%in%4:5&iter%in%1:50],
                            samples[run_label%in%summary_dt.original$run_label&catch=="ELL"&index%in%1:2&iter%in%1:25],
                            samples[run_label%in%summary_dt.original$run_label&catch!="ELL"&index%in%4:5&iter%in%1:317],
                            samples[run_label%in%summary_dt.original$run_label&catch!="ELL"&index%in%1&iter%in%1:158],
                            samples[run_label%in%summary_dt.original$run_label&catch!="ELL"&index%in%2&iter%in%1:158]) %>%
                            .[,ensemble:="Original"]
    ensemble_alternative = rbind(samples[run_label%in%summary_dt.alternative$run_label&catch=="ELL"&index%in%4:5&iter%in%1:50],
                            samples[run_label%in%summary_dt.alternative$run_label&catch=="ELL"&index%in%1:2&iter%in%1:25],
                            samples[run_label%in%summary_dt.alternative$run_label&catch!="ELL"&index%in%4:5&iter%in%1:317],
                            samples[run_label%in%summary_dt.alternative$run_label&catch!="ELL"&index%in%1&iter%in%1:158],
                            samples[run_label%in%summary_dt.alternative$run_label&catch!="ELL"&index%in%2&iter%in%1:158]) %>%
                            .[,ensemble:="Alternative"]

    ensembles = rbind(ensemble,ensemble_alternative)

    samples_ts = rbind(samples_ts[run_label%in%summary_dt.original$run_label&catch=="ELL"&index%in%4:5&iter%in%1:50],
                            samples_ts[run_label%in%summary_dt.original$run_label&catch=="ELL"&index%in%1:2&iter%in%1:25],
                            samples_ts[run_label%in%summary_dt.original$run_label&catch!="ELL"&index%in%4:5&iter%in%1:317],
                            samples_ts[run_label%in%summary_dt.original$run_label&catch!="ELL"&index%in%1&iter%in%1:158],
                            samples_ts[run_label%in%summary_dt.original$run_label&catch!="ELL"&index%in%2&iter%in%1:158])
    
    prior_posterior = prior_posterior[run_label%in%summary_dt.original$run_label]        
    fit_dt = fit_dt[run_label%in%summary_dt.original$run_label]                
#________________________________________________________________________________________________________________________________________________________________________________________________________
# save output
    fwrite(ensemble,file=paste0("report/ensemble.csv"))
    fwrite(samples_ts,file=paste0("report/samples_ts.csv"))
    fwrite(prior_posterior,file=paste0("report/prior_posterior.csv"))
    fwrite(fit_dt,file=paste0("report/fit_dt.csv"))

#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot fit
        first_year = 1994
        plot_dt = copy(fit_dt) %>%
                merge(.,summary_dt[,.(run_label,model_number)],by="run_label") %>%
                .[row>=1,year:=first_year+(row-1)] %>%
                .[row<1,year:=first_year+(row-1)]

        obs_se_dt = plot_dt[metric=="sigmao"] %>%
                    .[,.(model_number,index,row,year,value)] %>%
                    .[,se:=median(value),by=.(model_number,index,row,year)] %>%
                    .[,se:=round(se,digits=3)] %>%
                    .[,.(model_number,index,row,year,se)] %>%
                  unique(.)

        obs_cpue_dt = plot_dt[metric=="obs_cpue"] %>%
                    .[,.(model_number,index,row,year,value)] %>%
                    merge(.,obs_se_dt[,.(model_number,index,row,year,se)],by=c("model_number","index","row","year")) %>%
                    .[,obs:=round(value,digits=3)] 
    
        obs_quant = 0.5*(1-95/100)

        obs_cpue_dt = obs_cpue_dt[,.(model_number,index,row,year,obs,se)] %>%
                    .[,upper:=qlnorm(1-obs_quant,meanlog=log(obs),sdlog=se)] %>%
                    .[,lower:=qlnorm(obs_quant,meanlog=log(obs),sdlog=se)]
        pred_cpue_dt = plot_dt[metric=="pred_cpue",.(model_number,index,row,year,iter,value)] %>%
                    .[,.(median=median(value),upper=quantile(value,probs=1-obs_quant),lower=quantile(value,probs=obs_quant)),by=.(model_number,index,row,year)]

        pred_cpue_dt_2 = plot_dt[metric=="ppd_cpue",.(model_number,index,row,year,iter,value)] %>%
                    .[,.(median=median(value),upper=quantile(value,probs=1-obs_quant),lower=quantile(value,probs=obs_quant)),by=.(model_number,index,row,year)]

     p = pred_cpue_dt[model_number%in%1:40] %>%
            ggplot() +
			ylab("Index") +
            xlab("Year") +
            geom_hline(yintercept=1,linetype="dashed") +
            facet_wrap(~model_number,ncol=4)
 
    p = p + geom_segment(data=obs_cpue_dt[model_number%in%1:40],aes(x=year,xend=year,y=lower,yend=upper),linewidth=0.5)
    p = p + geom_ribbon(data=pred_cpue_dt_2,aes(x=year,ymin=lower,ymax=upper,fill=as.character(index)),alpha=0.2)
    p = p + geom_ribbon(aes(x=year,ymin=lower,ymax=upper,fill=as.character(index)),alpha=0.2) +
                geom_line(aes(x=year,y=median,color=as.character(index)),linewidth=1)
      
    p = p + geom_point(data=obs_cpue_dt[model_number%in%1:40],aes(x=year,y=obs),color="white",fill="black",shape=21,size=2)

    p = p + geom_hline(yintercept=0) +
			        scale_color_viridis("Index",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        scale_fill_viridis("Index",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
    ggsave(filename=paste0("ensemble.index_fit.png"), plot = p, device = "png", path = "./report/",
	  			scale = 0.75, width =9, height = 12, units = c("in"),
	  			dpi = 300, limitsize = TRUE)    
#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot management distributions
        p=ensembles %>%
        .[ensemble=="Original"] %>%
        .[name %in% c("msy","Umsy","dmsy","recent_U","recent_U_Umsy","recent_D","recent_D_Dmsy")] %>%
        .[,name:=factor(name,levels=c("msy","Umsy","dmsy","recent_U","recent_U_Umsy","recent_D","recent_D_Dmsy"),labels=c("MSY",expression("U"["MSY"]),expression("D"["MSY"]),expression("U"["2018-2021"]),expression("U"["2018-2021"]/"U"["MSY"]),expression("D"["2019-2022"]),expression("D"["2019-2022"]/"D"["MSY"])))] %>%
        ggplot() +
        ylab("Density") +
        xlab("Metric") +
        xlim(0,NA) +
        facet_wrap(~name,scales="free_x",labeller = label_parsed) +
        geom_density(aes(x=value,y=after_stat(scaled),color=ensemble,fill=ensemble),linewidth=1.15,alpha=0.2) +
        viridis::scale_color_viridis("Ensemble",begin = 0.1,end = 0.8,direction = 1,option = "H",discrete=TRUE) +
            viridis::scale_fill_viridis("Ensemble",begin = 0.1,end = 0.8,direction = 1,option = "H",discrete=TRUE) +
            theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
                                    panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
                                    panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
                                    strip.background =element_rect(fill="white"),
                                    legend.key = element_rect(fill = "white"))
    ggsave(filename=paste0("ensemble.mgmt_dist.png"), plot = p, device = "png", path = "./report/",
	  			scale = 0.75, width =9, height = 9, units = c("in"),
	  			dpi = 300, limitsize = TRUE)  
#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot effect of excluding model 16 as well
        p=ensembles %>%
        .[name %in% c("msy","Umsy","dmsy","recent_U","recent_U_Umsy","recent_D","recent_D_Dmsy")] %>%
        .[,name:=factor(name,levels=c("msy","Umsy","dmsy","recent_U","recent_U_Umsy","recent_D","recent_D_Dmsy"),labels=c("MSY",expression("U"["MSY"]),expression("D"["MSY"]),expression("U"["2018-2021"]),expression("U"["2018-2021"]/"U"["MSY"]),expression("D"["2019-2022"]),expression("D"["2019-2022"]/"D"["MSY"])))] %>%
        ggplot() +
        ylab("Density") +
        xlab("Metric") +
        xlim(0,NA) +
        facet_wrap(~name,scales="free_x",labeller = label_parsed) +
        geom_density(aes(x=value,y=after_stat(scaled),color=ensemble,fill=ensemble),linewidth=1.15,alpha=0.2) +
        viridis::scale_color_viridis("Ensemble",begin = 0.1,end = 0.8,direction = 1,option = "H",discrete=TRUE) +
            viridis::scale_fill_viridis("Ensemble",begin = 0.1,end = 0.8,direction = 1,option = "H",discrete=TRUE) +
            theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
                                    panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
                                    panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
                                    strip.background =element_rect(fill="white"),
                                    legend.key = element_rect(fill = "white"))
    ggsave(filename=paste0("ensemble.mgmt_dist_comp.png"), plot = p, device = "png", path = "./report/",
	  			scale = 0.75, width =9, height = 9, units = c("in"),
	  			dpi = 300, limitsize = TRUE)  
#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot time series
       year_one = 1994 
       plot_dt = samples_ts %>%
         .[!is.na(value)] %>%
        .[,.(med=median(value),avg=mean(value),lp=quantile(value,probs=0.5-(0.5*((as.numeric(95)-1e-1)/100))),up=quantile(value,probs=0.5+(0.5*((as.numeric(95)-1e-1)/100))),lp50=quantile(value,probs=0.5-(0.5*((as.numeric(50)-1e-1)/100))),up50=quantile(value,probs=0.5+(0.5*((as.numeric(50)-1e-1)/100)))),by=.(name,row)] %>%
        .[row>=1,year:=year_one+(row-1)] %>%
        .[row<1,year:=year_one+(row-1)] %>%
        .[name %in% c("Process error","Process error (raw)")&row>0,year:=year+1] %>%
        .[,name:=factor(name,levels=c("P","D","U","D_Dmsy","U_Umsy","removals"),labels=c("Numbers",expression("Depletion (D)"),expression("Exploitation (U)"),expression("D"/"D"["MSY"]),expression("U"/"U"["MSY"]),expression("Total"~"removals")))]
    
    p = plot_dt %>% ggplot() +
			ylab("Metric") +
      xlab("Year") +
      facet_wrap(~name,scales="free_y",labeller = label_parsed,ncol=1)
    p = p + geom_ribbon(aes(x=year,ymin=lp,ymax=up,fill=name),alpha=0.3,linewidth=1.15)
    p = p + geom_ribbon(aes(x=year,ymin=lp50,ymax=up50,fill=name),alpha=0.6,linewidth=1.15)
    p = p + geom_path(aes(x=year,y=med),color="black",linewidth=1.15)
    p = p + geom_hline(yintercept=0) +
			        viridis::scale_color_viridis("Metric",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE,labels = scales::parse_format()) +
			        viridis::scale_fill_viridis("Metric",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE,labels = scales::parse_format()) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
    ggsave(filename=paste0("ensemble.mgmt_ts.png"), plot = p, device = "png", path = "./report/",
	  			scale = 0.75, width =9, height = 12, units = c("in"),
	  			dpi = 300, limitsize = TRUE)     
#________________________________________________________________________________________________________________________________________________________________________________________________________
# plot kobe
    tmp_contour = ensembles %>%
                .[ensemble=="Original"] %>%
                    .[name %in% c("recent_D_Dmsy","recent_U_Umsy")] %>%
                    .[,.(run_label,iter,name,value)] %>%
                    .[,iter_id:=paste0(run_label,".",iter)] %>%
                    .[,iter_id:=as.numeric(as.factor(iter_id))] %>%
                    .[,.(iter_id,name,value)] %>%
                    dcast(.,iter_id~name)


        

        mv.kde = MASS::kde2d(tmp_contour$recent_D_Dmsy, tmp_contour$recent_U_Umsy,n = as.numeric(200),lims=c(range(tmp_contour$recent_D_Dmsy)*c(0.75,1.25),range(tmp_contour$recent_U_Umsy)*c(0.75,1.25)))
        dx = diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
        dy = diff(mv.kde$y[1:2])
        sz = sort(mv.kde$z)
        c1 = cumsum(sz) * dx * dy
        dimnames(mv.kde$z) = list(mv.kde$x,mv.kde$y)
        dc = reshape2::melt(mv.kde$z)
        dc$prob = approx(sz,1-c1,dc$value)$y
        dc_plot = dc
        dc = as.data.table(dc[,c("Var1","Var2","prob")]) %>%
            dcast(.,Var1~Var2)
        kd = MASS::kde2d(tmp_contour$recent_D_Dmsy, tmp_contour$recent_U_Umsy,n = as.numeric(200))
        kd$x = dc$Var1
        kd$y = as.numeric(colnames(dc)[-1])
        kd$z = as.matrix(dc)[,-1]
        cntr = contourLines(kd,levels=as.numeric(95)/100)
        cntr_ln = length(cntr)
        if(length(cntr)>1){
          cntr_dt.list = as.list(rep(NA,cntr_ln))
          for(j in 1:cntr_ln){
            cntr_dt.list[[j]] = as.data.table(cntr[[j]]) %>%
            .[,cntr_id:=j] %>%
            .[,.(cntr_id,x,y)] %>%
            setnames(.,c("x","y"),c("recent_D_Dmsy","recent_U_Umsy"))
            cntr_dt.list[[j]] = rbind(cntr_dt.list[[j]],cntr_dt.list[[j]][1])
          }
          cntr_dt = rbindlist(cntr_dt.list)
        } else {
          cntr_dt = as.data.table(cntr[[1]]) %>%
            .[,cntr_id:=1] %>%
            .[,.(cntr_id,x,y)] %>%
            setnames(.,c("x","y"),c("recent_D_Dmsy","recent_U_Umsy"))
            cntr_dt = rbind(cntr_dt,cntr_dt[1])
        }

    ts_msy = samples_ts[,.(value=median(value)),by=.(name,row)] %>%
        dcast(.,row~name)
        
p = tmp_contour[,.(recent_D_Dmsy=median(recent_D_Dmsy),recent_U_Umsy=median(recent_U_Umsy))] %>%
    ggplot() +
			xlab(expression(D["2019-2022"]/D["MSY"])) +
		  ylab(expression(U["2018-2021"]/U["MSY"])) +
      coord_fixed(ylim=c(0,2.25),xlim=c(0,2.25)) +
      scale_x_continuous(expand = expansion(mult=c(0,0.05))) +
      scale_y_continuous(expand = expansion(mult=c(0,0.05)))
    #   geom_polygon(data=data.table(P_Pmsy=c(0,1,1,0,0),F_Fmsy=c(0,0,1,1,0)),aes(x=P_Pmsy,y=F_Fmsy),fill="yellow",alpha=0.2) +
    #   geom_polygon(data=data.table(P_Pmsy=c(0,1,1,0,0),F_Fmsy=c(1,1,5,5,1)),aes(x=P_Pmsy,y=F_Fmsy),fill="red",alpha=0.2) +
    #   geom_polygon(data=data.table(P_Pmsy=c(1,5,5,1,1),F_Fmsy=c(1,1,5,5,1)),aes(x=P_Pmsy,y=F_Fmsy),fill="orange",alpha=0.2) +
    #   geom_polygon(data=data.table(P_Pmsy=c(1,5,5,1,1),F_Fmsy=c(0,0,1,1,0)),aes(x=P_Pmsy,y=F_Fmsy),fill="green",alpha=0.2) +
      p = p + geom_contour_filled(data=dc_plot,aes(x=Var1,y=Var2,z=prob),alpha=0.85,breaks=seq(from=0,to=0.95,by=0.05))
      p = p + geom_hline(yintercept=0,color="black") +
		  geom_vline(xintercept=0,color="black") +
      geom_hline(yintercept=1,linewidth=1.15,color="black") +
		  geom_vline(xintercept=1,linewidth=1.15,color="black") 
      
      p = p + geom_path(data=ts_msy,aes(x=D_Dmsy,U_Umsy))
      p = p + geom_point(data=ts_msy[row==min(row)],aes(x=D_Dmsy,y=U_Umsy),color="black",fill="white",shape=24,size=5)
      p = p + geom_point(data=ts_msy[row==max(row)],aes(x=D_Dmsy,y=U_Umsy),color="black",fill="gray70",shape=24,size=5)
      p = p + geom_label(data=ts_msy[row==min(row)],aes(x=D_Dmsy,y=U_Umsy),label=1994, position = position_dodge(0.9),size=5)
      p = p + geom_label(data=ts_msy[row==max(row)],aes(x=D_Dmsy,y=U_Umsy),label=2022,position= position_dodge(0.9),size=5)
      # p = p + geom_polygon(data=cntr_dt,aes(x=recent_D_Dmsy,y=recent_U_Umsy),color="black",fill="black",alpha=0.1)
     

      p = p +  
        geom_point(data=tmp_contour[,.(recent_D_Dmsy=median(recent_D_Dmsy),recent_U_Umsy=median(recent_U_Umsy))],aes(x=recent_D_Dmsy,y=recent_U_Umsy),color="white",fill="black",shape=21,size=5) +
        viridis::scale_fill_viridis("Percentile",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
    ggsave(filename=paste0("ensemble.kobe.png"), plot = p, device = "png", path = "./report/",
	  			scale = 0.75, width =9, height = 9, units = c("in"),
	  			dpi = 300, limitsize = TRUE)    
