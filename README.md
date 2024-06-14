# 2024-npo-sma-taf

Input data, model code, and executables needed to reproduce the 2024 [ISC](https://isc.fra.go.jp/index.html) stock assessment of North Pacific Ocean shortfin mako shark. 

[R](https://www.r-project.org/) code to reproduce the assessment results are provided in the Transparent Assessment Framework ([TAF](https://www.ices.dk/data/assessment-tools/Pages/transparent-assessment-framework.aspx)). R code in the TAF style is structured
to take the user through:
- input data preparation,
- running the model,
- extracting output,
- and summarizing output & making plots of model estimates.

Two workflows are provided:
- Users seeking to reproduce a single example model should run the R script [`00a_start_here.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/00a_start_here.R).
- Users seeking to reproduce the entire model ensemble should run the R script [`00b_start_here.R`](https://github.com/N-DucharmeBarth-NOAA/2024-npo-sma-taf/blob/main/r_code/00b_start_here.R). Note that running the full model ensemble will take ~70 minutes.

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

### License

The code contained in this repository is licensed under the GNU GENERAL PUBLIC LICENSE version 3 ([GPLv3](https://www.gnu.org/licenses/gpl-3.0.html)).

### Disclaimer

“The United States Department of Commerce (DOC) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.”
