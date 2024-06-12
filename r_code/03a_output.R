

# ISC SHARKWG
# 2024/06/11
# Extract results from a representative model from the model ensemble
# Model 20: Index 5 (JP); Baseline priors for init depletion, shape and Rmax; Medium variability for sigmaF prior

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


#________________________________________________________________________________________________________________________________________________________________________________________________________
# load packages
    library(TAF)
    library(data.table)
    library(magrittr)

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
# make directory for model outputs
    mkdir("output")
    mkdir(paste0("output/",run_label))

#________________________________________________________________________________________________________________________________________________________________________________________________________
# get relevant fit metrics  
    summary_dt = fread(paste0("model/",run_label,"/fit_summary.csv"))

    base_diags_dt = summary_dt[,c("run_label","divergent","max_rhat","min_neff")]
    base_diags_dt$divergent = base_diags_dt$divergent*1000
    base_diags_dt$min_neff = base_diags_dt$min_neff*1000
    base_diags_dt$index_rmse = as.data.frame(summary_dt)[,c("index_rmse_1","index_rmse_2","index_rmse_3","index_rmse_4","index_rmse_5")][1,ssp_scenarios$index[i]]
    base_diags_dt$index_loglik = as.data.frame(summary_dt)[,c("index_ll_1","index_ll_2","index_ll_3","index_ll_4","index_ll_5")][1,ssp_scenarios$index[i]]
    base_diags_dt$index = ssp_scenarios$index[i]
    base_diags_dt$main_prior = ssp_scenarios$main_prior[i]
    base_diags_dt$catch = ssp_scenarios$catch[i]
    base_diags_dt = base_diags_dt[,.(run_label,index,main_prior,catch,divergent,min_neff,index_rmse,index_loglik)]

#________________________________________________________________________________________________________________________________________________________________________________________________________
# extract time series (medians): removals, N, D, U, D_Dmsy, U_Umsy
        first_year = 1994
        derived_quants_ts = ssp_derived_quants_ts(ssp_summary=fread(paste0("model/",run_label,"/fit_summary.csv")),
                            samples_dt=fread(paste0("model/",run_label,"/hmc_samples.csv")),
                            settings=fread(paste0("model/",run_label,"/settings.csv")),
                            stan_data=fread(paste0("model/",run_label,"/stan_data.csv")),
                            sub_sample_prop=1) %>%
                .[name%in%c("removals","P","D","U","D_Dmsy","U_Umsy")] %>%
                .[,name:=factor(name,levels=c("removals","P","D","U","D_Dmsy","U_Umsy"),labels=c("Removals","Numbers","Depletion","Exploitation","D_Dmsy","U_Umsy"))] %>%
                .[,row:=(row-1)+first_year] %>%
                .[,.(value=median(value)),by=.(name,row)] %>%
                .[,run_label:=run_label] %>%
                setnames(.,"row","Year") %>%
                dcast(.,run_label+Year~name)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# extract observed cpue, predicted cpue and residual
        fit_dt = ssp_extract_cpue_fit(ssp_summary=fread(paste0("model/",run_label,"/fit_summary.csv")),
                            samples_dt=fread(paste0("model/",run_label,"/hmc_samples.csv")),
                            stan_data=fread(paste0("model/",run_label,"/stan_data.csv")),
                            settings=fread(paste0("model/",run_label,"/settings.csv")),
                            sub_sample_prop=1,
                            active="TRUE",
                            calc_std = "TRUE") %>%
                .[metric %in% c("obs_cpue","pred_cpue","std_residual")] %>%
                .[,metric:=factor(metric,levels=c("obs_cpue","pred_cpue","std_residual"),labels=c("Obs_CPUE","Pred_CPUE","Std_Residual"))] %>%
                .[,row:=(row-1)+first_year] %>%
                .[,.(value=median(value)),by=.(metric,row)] %>%
                .[,run_label:=run_label] %>%
                setnames(.,"row","Year") %>%
                dcast(.,run_label+Year~metric)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# write-out data files
    fwrite(base_diags_dt,file=paste0("output/",run_label,"/diagnostics.csv"))
    fwrite(derived_quants_ts,file=paste0("output/",run_label,"/derived_quantities.csv"))  
    fwrite(fit_dt,file=paste0("output/",run_label,"/fit.csv"))
    
                                 