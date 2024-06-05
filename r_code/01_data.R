

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
            catch_dt = fread("./boot/data/0020-ASPMchgMX.catch.csv") %>%
                       .[Time >= 1994,.(catch_n=sum(sel_num)*1000,catch_mt=sum(sel_bio)),by=Time] %>%
                       setnames(.,"Time","time") %>%
                       .[,time:=floor(time)]
            iattc_cn = fread("./boot/data/IATTC_CN_LL.csv")

            status_quo_catch = merge(catch_dt[,.(time,catch_n)],iattc_cn,by='time',all=TRUE) %>%
                           .[,removals:=catch_n] %>%
                           .[!is.na(IATTC_CN_LL_n),removals:=catch_n+IATTC_CN_LL_n] %>%
                           .[,.(time,removals)] %>%
                           setnames(.,c("time","removals"),c("year","total_removals_n")) %>%
                           as.data.frame(.)

            write.taf(status_quo_catch, dir="data")
