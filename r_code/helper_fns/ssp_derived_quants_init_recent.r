    

# ISC SHARKWG
# 2024/06/11
# Time series of derived quantities

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

ssp_derived_quants_init_recent = function(ssp_summary,derived_quants_ts,years_avg=1,exclude_last=0){
    if(exclude_last >= years_avg){
      stop("`exclude_last` must be less than `years_avg`")
    }
      dt_init = derived_quants_ts %>%
           .[name%in%c("D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","D","P","F","U")] %>%
           .[row==1] %>%
           .[,type:="init"] %>%
           .[,.(run_id,type,name,iter,chain,chain_iter,value)] %>%
           .[,.(value=mean(value)),by=.(run_id,type,name,iter,chain,chain_iter)] %>%
           dcast(.,run_id+iter+chain+chain_iter~type+name) %>%
           melt(.,id.vars=c("run_id","iter","chain","chain_iter"))
      
      dt_terminal = derived_quants_ts %>%
           .[name%in%c("D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","D","P","F","U")] %>%
           .[row%in%((max(derived_quants_ts$row)-(years_avg-1)):(max(derived_quants_ts$row)-exclude_last))] %>%
           .[,type:="recent"] %>%
           .[,.(run_id,type,name,iter,chain,chain_iter,value)] %>%
           .[,.(value=mean(value)),by=.(run_id,type,name,iter,chain,chain_iter)] %>%
           dcast(.,run_id+iter+chain+chain_iter~type+name) %>%
           melt(.,id.vars=c("run_id","iter","chain","chain_iter"))
      
      out = rbind(dt_init,dt_terminal)

      return(out)
}    
