

# Nicholas Ducharme-Barth
# 2023/02/04
# script to launch shiny app

# Copyright (c) 2024 Nicholas Ducharme-Barth
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#_____________________________________________________________________________________________________________________________
# load packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(markdown)
library(magrittr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(GGally)
# library(DT)

#________________________________________________________________________________________________________________________________________________________________________________________________________
# recalc run summaries
    recalc_summaries = FALSE
    if(recalc_summaries){
        source("./r-code/gather_shiny_viewer_ssp.r")
    }

#________________________________________________________________________________________________________________________________________________________________________________________________________
# load data 
    summary_dt = fread(file="./model-runs/ensemble_v4_ssp_summaries_retro0.csv")
#________________________________________________________________________________________________________________________________________________________________________________________________________
# set relative paths
    model_stem = "./model-runs/surplus-production/ensemble_runs_v4/"

#_____________________________________________________________________________________________________________________________
# get directories of available model runs
    all_dirs = list.files(model_stem,recursive = TRUE)
    all_dirs = all_dirs[grep("fit_summary.csv",all_dirs,fixed=TRUE)]
    all_dirs = gsub("fit_summary.csv","",all_dirs,fixed=TRUE)

    # get index names
    index_names = unique(fread("./model-runs/stepwise/0020-ASPMchgMX/cpue.csv")[,.(Fleet_name)]$Fleet_name)
    year_one = 1994
#_____________________________________________________________________________________________________________________________
# app options
  start_collapsed = FALSE

#_____________________________________________________________________________________________________________________________
# source ui/server
  source("./r-code/shiny/surplus-production/css.r")
  source("./r-code/shiny/surplus-production/ui.R")
  source("./r-code/shiny/surplus-production/server.R")

  source_dir_stem = "./r-code/helper-fns/stan/"
  sapply(paste0(source_dir_stem,(list.files(source_dir_stem))),source)

#_____________________________________________________________________________________________________________________________
# Run the app
  app = shinyApp(ui=ui,server=server)
  runApp(app, port = 8888)
