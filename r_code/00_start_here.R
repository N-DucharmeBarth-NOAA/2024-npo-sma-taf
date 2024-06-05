

# ISC SHARKWG
# 2024/06/05
# R code to re-produce the 2024 ISC North Pacific Ocean shortfin mako shark stock assessment
# using the Transparent Assessment Framework (TAF) <https://www.ices.dk/data/assessment-tools/Pages/transparent-assessment-framework.aspx>

# Copyright (c) 2024 ISC SHARKWG
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#________________________________________________________________________________________________________________________________________________________________________________________________________
# load packages
    library("TAF")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# change working directory to ./TAF/
    setwd("./TAF/")

#________________________________________________________________________________________________________________________________________________________________________________________________________
# initialize the TAF analysis
# Process the SOFTWARE.bib & DATA.bib  metadata files to set up the files required for the analysis.
# SOFTWARE.bib contains the Stan model code which will be compiled into executables used to fit the model
# DATA.bib contains the input data needed to fit the models
    taf.boot()

#________________________________________________________________________________________________________________________________________________________________________________________________________
# run the TAF analysis
    source.taf("../r_code/01_data.R") # format the data for the Bayesian State-Space Surplus Production model (BSPM)


