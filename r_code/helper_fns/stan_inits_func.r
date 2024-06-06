    

# ISC SHARKWG
# 2024/06/06
# inits function for stan model

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.
        
    stan_inits_func = function(version,Tm1 = (stan.data$T-1))
    {   
        
        if(version == "bspm_F_est"){
            inits = list(raw_logK=rnorm(1,0,0.25),
            raw_logr=rnorm(1,0,0.25),
            raw_logx0=rnorm(1,0,0.25),
            raw_epsp=rnorm(Tm1,0,0.25),
            raw_logsigmap=rnorm(1,0,0.25),
            raw_sigmao_add=abs(rnorm(1,0,0.25)),
            raw_logshape=rnorm(1,0,0.25),
            raw_sigmaf=abs(rnorm(1,0,0.25)),
            raw_F=abs(rnorm(Tm1,0,0.25)))
        }
        if(version == "bspm_F_llq"){
            inits = list(raw_logK=rnorm(1,0,0.25),
            raw_logr=rnorm(1,0,0.25),
            raw_logx0=rnorm(1,0,0.25),
            raw_epsp=rnorm(Tm1,0,0.25),
            raw_logsigmap=rnorm(1,0,0.25),
            raw_sigmao_add=abs(rnorm(1,0,0.25)),
            raw_logshape=rnorm(1,0,0.25),
            raw_logll_q=rnorm(1,0,0.25))
        }
        return(inits)
    }
