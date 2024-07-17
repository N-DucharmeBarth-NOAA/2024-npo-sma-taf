# 2024-npo-sma-taf

Input data, model code, and executables needed to reproduce the 2024 [ISC](https://isc.fra.go.jp/index.html) stock assessment of North Pacific Ocean shortfin mako shark. The full assessment report can be found [here](https://meetings.wcpfc.int/node/22828), and is a product of the ISC Shark Working Group ([SHARKWG](https://isc.fra.go.jp/working_groups/shark.html)).

[R](https://www.r-project.org/) code to reproduce the assessment results are provided in the Transparent Assessment Framework ([TAF](https://www.ices.dk/data/assessment-tools/Pages/transparent-assessment-framework.aspx)). R code in the TAF style is structured
to take the user through:
- input data preparation,
- running the model,
- extracting output,
- and summarizing output & making plots of model estimates.

Two workflows are provided:
- Users seeking to reproduce a single example model should run the R script [`00a_start_here.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/00a_start_here.R).
- Users seeking to reproduce the entire model ensemble should run the R script [`00b_start_here.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/00b_start_here.R). Note that running the full model ensemble will take ~70 minutes.

### Expected output

Output from either workflow will be created within the `TAF/` folder. The [`01_data.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/01_data.R) script will create the `TAF/data/` folder and populate it with the input files needed to run either work flow.

#### Single example model

In the single example model case, the [`02a_model.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/02a_model.R) script fits the example model using Stan and writes output files (in *csv* format) to the `TAF/model/0020_5.B.B.J.EC2_0/` directory.

The [`03a_output.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/03a_output.R) script post-processes the output and places processed *csv* files in the `TAF/output/0020_5.B.B.J.EC2_0/` directory.

Lastly, [`04a_report.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/04a_report.R) script makes two plots:
- `example_model.mgmt_ts.png` which shows time series (median, solid lines) of numbers, depletion, exploitation rate, density relative to density at MSY, exploitation rate, relative to exploitation rate at MSY, and total removals. The 50th (dark shading) and 95th (light shading) credible intervals are also shown. 
- `example_model.index_fit.png` which shows the Posterior Predictive Fit (median, solid line) to the observed index (black points with vertical bars showing estimated observation error). The 50th (dark shading) and 95th (light shading) posterior predictive intervals are also shown. 

#### Entire model ensemble

In the entire model ensemble case, the [`02b_model.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/02b_model.R) script fits the 32 models in the ensemble using Stan and writes output files (in *csv* format) to the following directories in the `TAF/model/` folder:
```
2024-npo-sma-taf  
│   ...
└───TAF/
│   │   ...
│   └───model/
│       └───0001_1.B.B.J.ELL_0/
│       │   ...
│       └───0032_5.E.B.J.EC3_0/
│   ...
```

The [`03b_output.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/03b_output.R) script post-processes the output from each model run and places processed *csv* files in the corresponding `TAF/output/` directories.

Lastly, [`04b_report.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/04b_report.R) script saves a number of *csv* files in the `TAF/report/` directory and five plots: 
- `ensemble.index_fit.png` which shows the Posterior Predictive Fit (median, solid line) to the observed index (black points with vertical bars showing estimated observation error). The 50th (dark shading) and 95th (light shading) posterior predictive intervals are also shown.
- `ensemble.mgmt_dist.png` which shows the posterior distribution of key management quantities.
- `ensemble.mgmt_dist_comp.png` which shows the effect of excluding the additional model that failed to meet convergence criteria when running with R version 4.4.0 (see **Note** below).
- `ensemble.mgmt_ts.png` which shows time series (median, solid lines) of numbers, depletion, exploitation rate, density relative to density at MSY, exploitation rate, relative to exploitation rate at MSY, and total removals. The 50th (dark shading) and 95th (light shading) credible intervals are also shown.
- `ensemble.kobe.png` which shows the relative stock status in terms of a Kobe plot (and associated bivariate posterior distribution).

The `summary.csv` in the `TAF/report/` directory contains the convergence status for each model in terms of number of divergent posterior samples, Rhat, and effective sample size.

### Running the models locally

Users should [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) this repository on their local machine.

#### Using base R

Users should open up an R terminal ([version 4.4.0](https://cloud.r-project.org/); with RTools 4.4 already installed) and change the working directory to the directory that they cloned the repository into:
```
setwd("path/to/2024-npo-sma-taf/")
```
Next they should source the [`.Rprofile`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/.Rprofile):
```
source(".Rprofile")
```
This should prompt the `renv` package to bootstrap itself. [renv](https://rstudio.github.io/renv/index.html) is used for R package management to ensure a consistent work environment is set-up. Follow the in terminal prompts to 
install all packages. This should take a few minutes as there are a number of packages to load. If `renv` does not bootstrap automatically then run:
```
renv::restore()
```

Once all packages have been installed the user can run either `00a_start_here.R` or `00b_start_here.R` to initiate the TAF workflows to re-create the assessment output.

#### Using Rstudio

Users should use [Rstudio](https://posit.co/download/rstudio-desktop/) with R version 4.4.0 and RTools 4.4 installed, and open the `2024-npo-sma-taf` [project](https://bookdown.org/ndphillips/YaRrr/projects-in-rstudio.html). The `renv` package to bootstrap itself as described above and once all packages have been installed the user can initiate either the `00a_start_here.R` or `00b_start_here.R` TAF workflows. If `renv` does not bootstrap automatically then run:
```
renv::restore()
```

#### Using Visual Studio Code

Users should use [Visual Studio Code](https://code.visualstudio.com/download) with R version 4.4.0 and RTools 4.4 installed (set-up instructions [here](https://github.com/REditorSupport/vscode-R)). In order to configure Visual Studio Code to work with `renv` the user should follow the configuration steps located [here](https://github.com/REditorSupport/vscode-R/wiki/Working-with-renv-enabled-projects). Once Visual Studio Code has been configured properly, open the `2024-npo-sma-taf` folder using Visual Studio Code. [Opening an R terminal](https://code.visualstudio.com/docs/languages/r#_running-r-code) should prompt the `renv` package to bootstrap itself as described above. Once all packages have been installed the user can initiate either the `00a_start_here.R` or `00b_start_here.R` TAF workflows. If `renv` does not bootstrap automatically then run:
```
renv::restore()
```
### Running the models remotely

Alternatively, models can be run in the cloud using [GitHub Codespaces](https://github.com/features/codespaces). A virtual Linux machine has already been [configured](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/.devcontainer/devcontainer.json) so users can simply [open a Codespace](https://docs.github.com/en/codespaces/developing-in-a-codespace/creating-a-codespace-for-a-repository#creating-a-codespace-for-a-repository) using default options. Initial creation of the Codespace can take 15-20 minutes. Once the Codespace is created, open an R terminal. This should prompt the `renv` package to bootstrap itself as described above. Once all packages have been installed the user can initiate either the `00a_start_here.R` or `00b_start_here.R` TAF workflows. If `renv` does not bootstrap automatically then run:
```
renv::restore()
```

### Expected warnings
The following warning messages are expected, and do not indicate that models ran incorrectly.
```
Warning messages:
1: In melt.data.table(.) :
  id.vars and measure.vars are internally guessed when both are 'NULL'. All non-numeric/integer/logical type columns are considered id.vars, which in this case are columns []. Consider providing at least one of 'id' or 'measure' vars in future.
2: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
```

### Note
The following convergence criteria was used to determine models retained for the final ensemble:
- $\hat{R} < 1.01$
- Bulk effective sample size (ESS) greater than 100 samples per chain. Five chains were used so $ESS > 500$.
- No divergent transitions in posterior samples.

Criteria were based on [Monnahan 2024](https://doi.org/10.1016/j.fishres.2024.107024).

The original model runs to produce management advice used R version 4.3.1 and identified that models 5, 8, 12, and 30 failed to meet these conversion criteria. Models within this repository were run with R version 4.4.0 to address an identified security risk in earlier versions of R. Doing so resulted in estimates that were virtually identical. However, using R version 4.4.0 resulted in model 16 having $\hat{R}=1.012$ which is marginally higher than the convergence criteria. All other convergence criteria for all other models were unchanged between the two versions. Including/excluding model 16 from the ensemble does not change the management advice as seen in figure `TAF/report/ensemble.mgmt_dist_comp.png`.


### License

The code contained in this repository is licensed under the GNU GENERAL PUBLIC LICENSE version 3 ([GPLv3](https://www.gnu.org/licenses/gpl-3.0.html)).

### Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
