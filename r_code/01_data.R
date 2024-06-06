

# ISC SHARKWG
# 2024/06/05
# Format the data for the Bayesian State-Space Surplus Production model (BSPM)

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.


#________________________________________________________________________________________________________________________________________________________________________________________________________
# load packages
    library(TAF)
    library(data.table)
    library(magrittr)
#________________________________________________________________________________________________________________________________________________________________________________________________________
# make directory for processed data
    mkdir("data")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# bring in data - status quo catch
            # bring in catch in numbers (labelled the SS3 08 – 2022simple model in the report)
            catch_dt = fread("./boot/data/0020-ASPMchgMX.catch.csv") %>%
                       .[Time >= 1994,.(catch_n=sum(sel_num)*1000,catch_mt=sum(sel_bio)),by=Time] %>%
                       setnames(.,"Time","time") %>%
                       .[,time:=floor(time)]
            # add chinese catch from IATTC
            iattc_cn = fread("./boot/data/IATTC_CN_LL.csv")

            status_quo_catch = merge(catch_dt[,.(time,catch_n)],iattc_cn,by='time',all=TRUE) %>%
                           .[,removals:=catch_n] %>%
                           .[!is.na(IATTC_CN_LL_n),removals:=catch_n+IATTC_CN_LL_n] %>%
                           .[,.(time,removals)] %>%
                           as.data.frame(.)

            write.taf(status_quo_catch, dir="data")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# bring in catch data (non-longline) & adjust mexican artisanal catch
            # bring in catch in numbers (labelled the SS3 06 – 2022data model in the report)
            non_ll_catch_dt = fread("./boot/data/0018-2024dataASPMv2.catch.csv") %>%
                        setnames(.,"Fleet","fleet") %>%
                        merge(.,fread("./boot/data/0018-2024dataASPMv2.fleet_summary.csv")[,.(fleet,fleetname)]) %>%
                        .[fleet%in%c(5,6,9,12,13,16,17,22,23,27,30,32)] %>%
                       .[Time >= 1994,.(catch_n=sum(sel_num)*1000,catch_mt=sum(sel_bio)),by=Time] %>%
                       setnames(.,"Time","time") %>%
                       .[,time:=floor(time)]
            mex_art_dt = fread("./boot/data/0020-ASPMchgMX.catch.csv") %>%
                       .[Fleet %in% 20] %>%
                       .[Time >= 1994,.(catch_n=sum(sel_num)*1000,catch_mt=sum(sel_bio)),by=Time] %>%
                       setnames(.,"Time","time") %>%
                       .[,time:=floor(time)]
        
            recent_non_ll_catch = rbind(non_ll_catch_dt,mex_art_dt) %>%
                 .[,.(catch_n=sum(catch_n)),by=time] %>%
                           as.data.frame(.)
            
            write.taf(recent_non_ll_catch, dir="data")


#________________________________________________________________________________________________________________________________________________________________________________________________________
# bring in catch data (non-longline) & adjust mexican artisanal catch
        wcpfc_ll_effort = fread("./boot/data/WCPFC_L_PUBLIC_BY_FLAG_YR.csv") %>%
                          .[,.(yy,flag_id,lon_short,lat_short,hhooks)] %>%
                          .[,lat_num:=as.numeric(sapply(lat_short,substr,1,2))] %>%
                          .[,lat_chr:=sapply(lat_short,substr,3,3)] %>%
                          .[,lon_num:=as.numeric(sapply(lon_short,substr,1,3))] %>%
                          .[,lon_chr:=sapply(lon_short,substr,4,4)] %>%
                          .[lat_chr=="S",lat_num:=-lat_num] %>%
                          .[lon_chr=="W",lon_num:=180+(180-lon_num)] %>%
                          .[lat_num>10,.(hhooks=sum(hhooks)),by=yy] %>%
                          .[,rfmo:="wcpfc"]
        
        iattc_ll_effort = fread("./boot/data/IATTCPublicLLTunaBillfishNum.csv") %>%
                          .[LatC5>10,.(hhooks=sum(Hooks)/100),by=Year] %>%
                          setnames(.,"Year","yy") %>%
                          .[,rfmo:="iattc"]
        npo_ll_effort = rbind(wcpfc_ll_effort,iattc_ll_effort) %>%
                    .[,.(khooks=sum(hhooks)/10),by=yy] %>%
                    .[yy%in%1994:2022]
        # replace 2022 value which looks under-reported; use average of previous 5 years
        npo_ll_effort = npo_ll_effort %>%
                        .[yy==2022,khooks:=mean(npo_ll_effort[yy%in%2017:2021]$khooks)] %>%
                        .[,khooks_sc:=khooks/max(khooks)] %>%
                        as.data.frame(.)
        write.taf(npo_ll_effort, dir="data")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# bring in index data
    # read and prepare cpue
            index_mat = fread("./boot/data/0020-ASPMchgMX.cpue.csv") %>%
                      .[,Time:=floor(Time)] %>%
                      .[,.(Time,Fleet_name,Obs)] %>%
                      .[,Obs:=Obs/mean(Obs),by=Fleet_name] %>%
                      dcast(.,Time~Fleet_name) %>%
                      .[,Time:=NULL] %>%
                      as.data.frame(.) %>%
                      as.matrix(.)

            index_mat[is.na(index_mat)] = -999
            write.taf(index_mat, dir="data")
        
            se_mat = fread("./boot/data/0020-ASPMchgMX.cpue.csv") %>%
                      .[,Time:=floor(Time)] %>%
                      .[,.(Time,Fleet_name,SE)] %>%
                      .[,SE:=SE/mean(SE),by=Fleet_name] %>%
                      dcast(.,Time~Fleet_name) %>%
                      .[,Time:=NULL] %>%
                      as.data.frame(.) %>%
                      as.matrix(.)
            se_mat[is.na(se_mat)] = -999
            write.taf(se_mat, dir="data")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# bring in priors
        pr1 = fread(file="./boot/data/shape_pars.csv") %>%
                melt(.,id.vars=c("V1")) %>%
                setnames(.,c("V1","variable"),c("parameter","type")) %>%
                .[,group:="Base"] %>%
                .[,type:=as.character(type)] %>%
                .[,type:="logshape"] %>%
                .[,parameter:=c("mean","log_sd")] %>%
                .[,.(group,type,parameter,value)]
        pr2 = fread(file="./boot/data/shape_pars_ex.csv") %>%
                melt(.,id.vars=c("V1")) %>%
                setnames(.,c("V1","variable"),c("parameter","type")) %>%
                .[,group:="Extreme"] %>%
                .[,type:=as.character(type)] %>%
                .[,type:="logshape"] %>%
                .[,parameter:=c("mean","log_sd")] %>%
                .[,.(group,type,parameter,value)]
        pr3 = fread(file="./boot/data/init_dep_pars.csv") %>%
                melt(.,id.vars=c("V1")) %>%
                setnames(.,c("V1","variable"),c("parameter","type")) %>%
                .[,group:="Base"] %>%
                .[,type:=as.character(type)] %>%
                .[,type:="logx0"] %>%
                .[,parameter:=c("mean","log_sd")] %>%
                .[,.(group,type,parameter,value)]
        pr4 = fread(file="./boot/data/init_dep_pars_ex.csv") %>%
                melt(.,id.vars=c("V1")) %>%
                setnames(.,c("V1","variable"),c("parameter","type")) %>%
                .[,group:="Extreme"] %>%
                .[,type:=as.character(type)] %>%
                .[,type:="logx0"] %>%
                .[,parameter:=c("mean","log_sd")] %>%
                .[,.(group,type,parameter,value)]
        pr5 = fread(file="./boot/data/rmax_pars.csv") %>%
                melt(.,id.vars=c("V1")) %>%
                setnames(.,c("V1","variable"),c("parameter","type")) %>%
                .[,group:="Base"] %>%
                .[,type:=as.character(type)] %>%
                .[,type:="logr"] %>%
                .[,parameter:=c("mean","log_sd")] %>%
                .[,.(group,type,parameter,value)]
        pr6 = fread(file="./boot/data/rmax_pars_ex.csv") %>%
                melt(.,id.vars=c("V1")) %>%
                setnames(.,c("V1","variable"),c("parameter","type")) %>%
                .[,group:="Extreme"] %>%
                .[,type:=as.character(type)] %>%
                .[,type:="logr"] %>%
                .[,parameter:=c("mean","log_sd")] %>%
                .[,.(group,type,parameter,value)]
        pr7 = fread(file="./boot/data/q_pars.csv") %>%
                melt(.) %>%
                setnames(.,c("variable"),c("type")) %>%
                .[,group:="Base"] %>%
                .[,type:=as.character(type)] %>%
                .[,type:="logll_q"] %>%
                .[,parameter:=c("mean","log_sd")] %>%
                .[,.(group,type,parameter,value)]

        pr_logK = data.table(group=rep("Base",2),type=rep("logK",2),parameter=c("mean","log_sd"),value=c(16,1))
        # specify lognormal prior for process error based on JABBA default
        set.seed(123)
        sigmap_sim = (sqrt(1/rgamma(100000,shape=4,rate=0.01))) # use JABBA prior for precision

        sigmap_fn = function(par){-sum(dnorm(log(sigmap_sim), mean = par[1], sd = par[2], log = TRUE))}
        sigmap_pars = nlminb(c(log(0.05), 0.2), sigmap_fn)$par   
        pr_sigmap = data.table(group=rep("Base",2),type=rep("logsigmap",2),parameter=c("mean","log_sd"),value=sigmap_pars)


        prior_dt = rbind(pr1,pr2,pr3,pr4,pr5,pr6,pr_logK,pr7,pr_sigmap)%>%
                   as.data.frame(.)
        write.taf(prior_dt, dir="data")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# define scenarios
        ssp_scenarios = expand.grid(index=c(1,2,4,5),main_prior=c("B","E"),logK_prior=c("B"),sigmap_prior=c("J"),catch=c("ELL","EC","EC2","EC3"),stringsAsFactors=FALSE)
        ssp_scenarios$run_label = rep(NA,nrow(ssp_scenarios))
        ssp_scenarios$run_number = rep(NA,nrow(ssp_scenarios))        
        for(i in 1:nrow(ssp_scenarios)){
            ssp_scenarios$run_label[i] = paste0(paste0(c(ssp_scenarios$index[i],ssp_scenarios$main_prior[i],ssp_scenarios$logK_prior[i],ssp_scenarios$sigmap_prior[i],ssp_scenarios$catch[i]),collapse="."),"_0")    
            if(i<10){
                ssp_scenarios$run_number[i] = paste0("000",i)
            } else if(i<100) {
                ssp_scenarios$run_number[i] = paste0("00",i)
            } else {
                ssp_scenarios$run_number[i] = paste0("0",i)
            }
        }
        write.taf(ssp_scenarios, dir="data")
