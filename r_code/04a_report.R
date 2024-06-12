

# ISC SHARKWG
# 2024/06/11
# Plot some results from a representative model from the model ensemble
# Model 20: Index 5 (JP); Baseline priors for init depletion, shape and Rmax; Medium variability for sigmaF prior

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
# bring in data
    ssp_scenarios = fread("./data/ssp_scenarios.csv")
    
#________________________________________________________________________________________________________________________________________________________________________________________________________
# identify target model
    i = 20
    run_label_stem = paste0(c(ssp_scenarios$index[i],ssp_scenarios$main_prior[i],ssp_scenarios$logK_prior[i],ssp_scenarios$sigmap_prior[i],ssp_scenarios$catch[i]),collapse=".")    
    if(i<10){
        run_number = paste0("000",i)
    } else if(i<100) {
        run_number = paste0("00",i)
    }
    run_label = paste0(run_number,"_",run_label_stem,"_0")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# make directory for plots
    mkdir("report")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# make time series plots
    first_year = 1994 
    plot_dt = ssp_derived_quants_ts(ssp_summary=fread(paste0("model/",run_label,"/fit_summary.csv")),
                            samples_dt=fread(paste0("model/",run_label,"/hmc_samples.csv")),
                            settings=fread(paste0("model/",run_label,"/settings.csv")),
                            stan_data=fread(paste0("model/",run_label,"/stan_data.csv")),
                            sub_sample_prop=1) %>%
         .[!is.na(value)&name%in%c("P","D","U","D_Dmsy","U_Umsy","removals")] %>%
        .[,.(med=median(value),avg=mean(value),lp=quantile(value,probs=0.5-(0.5*((as.numeric(95)-1e-1)/100))),up=quantile(value,probs=0.5+(0.5*((as.numeric(95)-1e-1)/100))),lp50=quantile(value,probs=0.5-(0.5*((as.numeric(50)-1e-1)/100))),up50=quantile(value,probs=0.5+(0.5*((as.numeric(50)-1e-1)/100)))),by=.(name,row)] %>%
        .[row>=1,year:=first_year+(row-1)] %>%
        .[row<1,year:=first_year+(row-1)] %>%
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
    ggsave(filename=paste0("example_model.mgmt_ts.png"), plot = p, device = "png", path = "./report/",
	  			scale = 0.75, width =9, height = 12, units = c("in"),
	  			dpi = 300, limitsize = TRUE)        
                           
#________________________________________________________________________________________________________________________________________________________________________________________________________
# make plot of posterior predictive fit to the index
        first_year = 1994
        samples_ts = ssp_extract_cpue_fit(ssp_summary=fread(paste0("model/",run_label,"/fit_summary.csv")),
                            samples_dt=fread(paste0("model/",run_label,"/hmc_samples.csv")),
                            stan_data=fread(paste0("model/",run_label,"/stan_data.csv")),
                            settings=fread(paste0("model/",run_label,"/settings.csv")),
                            sub_sample_prop=1,
                            active="TRUE",
                            calc_std = "FALSE") %>%
                .[,model_number:=i]%>%
                .[row>=1,year:=first_year+(row-1)] %>%
                .[row<1,year:=first_year+(row-1)]

        obs_se_dt = samples_ts[metric=="sigmao"] %>%
                    .[,.(model_number,index,row,year,value)] %>%
                    .[,se:=median(value),by=.(model_number,index,row,year)] %>%
                    .[,se:=round(se,digits=3)] %>%
                    .[,.(model_number,index,row,year,se)] %>%
                  unique(.)

        obs_cpue_dt = samples_ts[metric=="obs_cpue"] %>%
                    .[,.(model_number,index,row,year,value)] %>%
                    merge(.,obs_se_dt[,.(model_number,index,row,year,se)],by=c("model_number","index","row","year")) %>%
                    .[,obs:=round(value,digits=3)] 
    
        obs_quant = 0.5*(1-95/100)

        obs_cpue_dt = obs_cpue_dt[,.(model_number,index,row,year,obs,se)] %>%
                    .[,upper:=qlnorm(1-obs_quant,meanlog=log(obs),sdlog=se)] %>%
                    .[,lower:=qlnorm(obs_quant,meanlog=log(obs),sdlog=se)]
        pred_cpue_dt = samples_ts[metric=="pred_cpue",.(model_number,index,row,year,iter,value)] %>%
                    .[,.(median=median(value),upper=quantile(value,probs=1-obs_quant),lower=quantile(value,probs=obs_quant)),by=.(model_number,index,row,year)]

        pred_cpue_dt_2 = samples_ts[metric=="ppd_cpue",.(model_number,index,row,year,iter,value)] %>%
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
    ggsave(filename=paste0("example_model.index_fit.png"), plot = p, device = "png", path = "./report/",
	  			scale = 0.75, width =9, height = 9, units = c("in"),
	  			dpi = 300, limitsize = TRUE)        

                            