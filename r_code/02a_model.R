

# ISC SHARKWG
# 2024/06/06
# Run a representative model from the model ensemble
# Model 20: Index 5 (JP); Baseline priors for init depletion, shape and Rmax; Medium variability for sigmaF prior

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

#________________________________________________________________________________________________________________________________________________________________________________________________________
# make directory for processed data
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
    exec_name = "bspm_F_est"
    stan_c = stan_model(file=paste0("./boot/software/",exec_name,".stan"), model_name =exec_name)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# set-up model inputs
    i = 20
    run_label_stem = paste0(c(ssp_scenarios$index[i],ssp_scenarios$main_prior[i],ssp_scenarios$logK_prior[i],ssp_scenarios$sigmap_prior[i],ssp_scenarios$catch[i]),collapse=".")    
    if(i<10){
        run_number = paste0("000",i)
    } else if(i<100) {
        run_number = paste0("00",i)
    }
            
    # specify data and priors
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
                                    
        tmp_data = c(tmp_data,tmp.data.priors)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# run model
    fit = fit_rstan(tmp_data,
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
                    n_cores=1)
                