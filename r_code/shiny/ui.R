
ui = dashboardPage(
  header = dashboardHeader(title="2024-npo-mako"),
  sidebar = dashboardSidebar(
    br(),
    br(),
    sidebarMenu(id="sidebarmenu",
      menuItem("Introduction", tabName="introduction"),
      menuItem("Summary table", tabName="table"),
      menuItem("Bayesian diags: Convergence", tabName="plots_hmc"),
      menuItem("Bayesian diags: PPC", tabName="plots_tab_ppc"),
      menuItem("Model fits", tabName="plots_model_fits"),
      menuItem("Pr. & Post: params", tabName="plots_tab_ppp"),
      menuItem("Pr. & Post: time-series", tabName="plots_tab_ppts"),
      menuItem("Pr. & Post: mgmt. quants.", tabName="plots_tab_ppmq"),
      menuItem("Kobe & Majuro", tabName="plots_tab_kbmj"),
      menuItem("Forecasts", tabName="plots_tab_forecasts"),
      menuItem("Catch reconstruction", tabName="plots_tab_cr")
    ),
    # Only show these on the plotting tabs - not Introduction and Summary table tabs
    conditionalPanel(condition="input.sidebarmenu == 'plots_hmc'",
      # leading_params
      awesomeCheckboxGroup(
      inputId = "hmc.leading_params",
      label = "Parameter", 
        choices = c("logK","x0","r","sigmao","sigmap","shape","lp__"),
      selected = c("logK","x0","r","lp__")),
      # raw
      switchInput(
      inputId = "hmc.raw",  
      label = "Transform parameter?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # diag.color
      awesomeRadio(
        inputId = "hmc.diag",
        label = "Error type", 
        choices = c("None", "Divergences", "Max. treedepth"),
        selected = "None"
      ),
      # eps
      switchInput(
      inputId = "hmc.eps",  
      label = "Include eps?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # acf lags
      sliderTextInput(
      inputId = "hmc.lags",  
      label = "ACF lags (choose a value)",
      choices = seq(from=5,to=50,by=5),
      selected = "30",
      grid = TRUE),
      #bayesplot color scheme
      pickerInput(
        inputId = "hmc.scheme",
        label = "Select bayesplot color scheme", 
          choices = c("blue","brightblue","gray", "darkgray","green","pink","purple","red","teal","yellow","viridis", "viridisA", "viridisB", "viridisC", "viridisD", "viridisE"),
          selected = c("brightblue"),
          multiple = FALSE
      )
    ),
    conditionalPanel(condition="input.sidebarmenu == 'plots_tab_ppc'",
      #bayesplot color scheme
      pickerInput(
        inputId = "ppc.scheme",
        label = "Select bayesplot color scheme", 
          choices = c("blue","brightblue","gray", "darkgray","green","pink","purple","red","teal","yellow","viridis", "viridisA", "viridisB", "viridisC", "viridisD", "viridisE"),
          selected = c("brightblue"),
          multiple = FALSE
      ),
      # sample prop
      sliderTextInput(
      inputId = "ppc.prop",  
      label = "Sub-sample proportion",
      choices = c(0.01,seq(from=0.05,to=1,by=0.05)),
      selected = "0.25",
      grid = TRUE),
      # active
      switchInput(
      inputId = "ppc.active",  
      label = "Only fitted indices?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # group
      switchInput(
      inputId = "ppc.group",  
      label = "Aggregate observations for PPC?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # ppc stat
      awesomeCheckboxGroup(
      inputId = "ppc.stat",
      label = "PPC statistic\n(choose 1 or 2)", 
        choices = c("mean","median","sd","mad"),
      selected = c("median")),
      # ppc qq
      awesomeRadio(
        inputId = "ppc.qqdist",
        label = "QQ distribution", 
        choices = c("uniform", "normal"),
        selected = "uniform"
      )
    ),
    conditionalPanel(condition="input.sidebarmenu == 'plots_model_fits'",
      
      # sample prop
      sliderTextInput(
      inputId = "fits.prop",  
      label = "Sub-sample proportion",
      choices = c(0.01,seq(from=0.05,to=1,by=0.05)),
      selected = "0.25",
      grid = TRUE),
      # active
      switchInput(
      inputId = "fits.active",  
      label = "Only fitted indices?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # obs error
      switchInput(
      inputId = "fits.obs",  
      label = "Show obs. error?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # fit type
      awesomeRadio(
        inputId = "fits.type",
        label = "Show", 
        choices = c("Median", "Spaghetti", "Quantile"),
        selected = "Median"
      ),
      # credible interval
      sliderTextInput(
      inputId = "fits.quants",  
      label = "Credible interval (%)",
      choices = c(1,seq(from=5,to=100,by=5)),
      selected = "95",
      grid = TRUE),
      # residual type
      awesomeRadio(
        inputId = "fits.resid",
        label = "Show", 
        choices = c("Ordinary", "Standardized", "PIT"),
        selected = "PIT"
      )),
    conditionalPanel(condition="input.sidebarmenu == 'plots_tab_ppp'",
      # leading_params
      awesomeCheckboxGroup(
      inputId = "ppp.leading_params",
      label = "Parameter", 
        choices = c("logK","x0","r","sigmao","sigmap","shape"),
      selected = c("logK","x0","r")),
      # raw
      switchInput(
      inputId = "ppp.raw",  
      label = "Transform parameter?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # show distribution
      awesomeRadio(
        inputId = "ppp.show",
        label = "Show", 
        choices = c("Prior", "Posterior", "Both"),
        selected = "Both"
      ),
      # combine posterior
      switchInput(
      inputId = "ppp.combine",  
      label = "Combine posterior?",
      value=FALSE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger")
    ),
    conditionalPanel(condition="input.sidebarmenu == 'plots_tab_ppts'",
      pickerInput(
        inputId = "ppts.var",
        label = "Select metric(s)", 
          choices = c("Depletion (D)","Population (P)","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","Removals","Process error","Process error (raw)","Process error (mult.)","Surplus production"),
          selected = c("Depletion (D)","F_Fmsy","Removals","Process error (mult.)"),
        options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
      ),
      # show distribution
      awesomeRadio(
        inputId = "ppts.show",
        label = "Show", 
        choices = c("Prior", "Posterior", "Both"),
        selected = "Both"
      ),
      # combine posterior
      switchInput(
      inputId = "ppts.combine",  
      label = "Combine posterior?",
      value=FALSE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # sample prop
      sliderTextInput(
      inputId = "ppts.prop",  
      label = "Sub-sample proportion",
      choices = c(0.01,seq(from=0.05,to=1,by=0.05)),
      selected = "0.25",
      grid = TRUE),
      # credible interval
      sliderTextInput(
      inputId = "ppts.quants",  
      label = "Credible interval (%)",
      choices = c(1,seq(from=5,to=100,by=5)),
      selected = "95",
      grid = TRUE)),
    conditionalPanel(condition="input.sidebarmenu == 'plots_tab_ppmq'",
      pickerInput(
        inputId = "ppmq.var",
        label = "Select metric(s)", 
          choices = c("init_D_Dmsy","init_P_Pmsy","init_U_Umsy","init_F_Fmsy","init_D","init_P","init_F","init_U","recent_D_Dmsy","recent_P_Pmsy","recent_U_Umsy","recent_F_Fmsy","recent_D","recent_P","recent_F","recent_U"),
          selected = c("init_D_Dmsy","init_F_Fmsy","recent_D_Dmsy","recent_F_Fmsy"),
        options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
      ),
      # show distribution
      awesomeRadio(
        inputId = "ppmq.show",
        label = "Show", 
        choices = c("Prior", "Posterior", "Both"),
        selected = "Both"
      ),
      # combine posterior
      switchInput(
      inputId = "ppmq.combine",  
      label = "Combine posterior?",
      value=FALSE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # sample prop
      sliderTextInput(
      inputId = "ppmq.prop",  
      label = "Sub-sample proportion",
      choices = c(0.01,seq(from=0.05,to=1,by=0.05)),
      selected = "0.25",
      grid = TRUE),
      # average over x years
      sliderTextInput(
      inputId = "ppmq.avg_year",  
      label = "Average final n yrs.",
      choices = 1:10,
      selected = "5",
      grid = TRUE),
      # omit last n yrs from avg.
      sliderTextInput(
      inputId = "ppmq.last_year",  
      label = "Omit final n yrs.",
      choices = 0:9,
      selected = "1",
      grid = TRUE)),
    conditionalPanel(condition="input.sidebarmenu == 'plots_tab_kbmj'",
      # show distribution
      awesomeRadio(
        inputId = "kbmj.show",
        label = "Show", 
        choices = c("Prior", "Posterior", "Both"),
        selected = "Both"
      ),
      # combine posterior
      switchInput(
      inputId = "kbmj.combine",  
      label = "Combine posterior?",
      value=FALSE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # sample prop
      sliderTextInput(
      inputId = "kbmj.prop",  
      label = "Sub-sample proportion",
      choices = c(0.01,seq(from=0.05,to=1,by=0.05)),
      selected = "0.25",
      grid = TRUE),
      # show distribution
      switchInput(
      inputId = "kbmj.uncertainty",  
      label = "Show terminal year uncertainty?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # credible interval
      sliderTextInput(
      inputId = "kbmj.quants",  
      label = "Credible interval (%)",
      choices = c(1,seq(from=5,to=95,by=5),99),
      selected = "95",
      grid = TRUE),
      # credible interval
      sliderTextInput(
      inputId = "kbmj.resolution",  
      label = "Contour resolution",
      choices = seq(from=50,to=500,by=25),
      selected = "100",
      grid = TRUE)),
conditionalPanel(condition="input.sidebarmenu == 'plots_tab_forecasts'",
      pickerInput(
        inputId = "forecasts.var",
        label = "Select metric(s)", 
          choices = c("Depletion (D)","Population (P)","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","Removals","Process error","Process error (raw)","Surplus production"),
          selected = c("Depletion (D)","F_Fmsy","Removals","Process error"),
        options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
      ),
      # combine posterior
      switchInput(
      inputId = "forecasts.combine",  
      label = "Combine posterior?",
      value=FALSE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # sample prop
      sliderTextInput(
      inputId = "forecasts.prop",  
      label = "Sub-sample proportion",
      choices = c(0.01,seq(from=0.05,to=1,by=0.05)),
      selected = "0.25",
      grid = TRUE),
      # credible interval
      sliderTextInput(
      inputId = "forecasts.quants",  
      label = "Credible interval (%)",
      choices = c(1,seq(from=5,to=100,by=5)),
      selected = "95",
      grid = TRUE),
      # forecast years
      sliderTextInput(
      inputId = "forecasts.nyears",  
      label = "Years in forecast period",
      choices = c(1:20),
      selected = "5",
      grid = TRUE),
      # re-sample historical process
      switchInput(
      inputId = "forecasts.resample_epsp",  
      label = "Re-sample historical process?",
      value=TRUE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # forecast type
      awesomeRadio(
        inputId = "forecasts.type",
        label = "Forecast type", 
        choices = c("Catch", "U", "MSY","Umsy"),
        selected = "Catch"
      ),
      # average over x years
      sliderTextInput(
      inputId = "forecasts.avg_year",  
      label = "Average (Catch/U) for final n yrs.",
      choices = 1:10,
      selected = "3",
      grid = TRUE),
      # scalar
      sliderTextInput(
      inputId = "forecasts.scalar",  
      label = "Catch/U multiplier",
      choices = c(0.01,seq(from=0.1,to=5,by=0.1)),
      selected = "1",
      grid = TRUE)),
conditionalPanel(condition="input.sidebarmenu == 'plots_tab_cr'",
      pickerInput(
        inputId = "cr.var",
        label = "Select metric(s)", 
          choices = c("CPUE (LL)",
            "CPUE (DFN)",
            "Removals (LL)",
            "Removals (DFN)",
            "Effort (LL)",
            "Effort (DFN)", "Catch error (LL; raw)", "Catch error (DFN; raw)",
            "Process error (raw)",
            "Depletion (D)", "Removals"),
          selected = c("Depletion (D)", "Removals"),
        options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
      ),
      # combine posterior
      switchInput(
      inputId = "cr.combine",  
      label = "Combine posterior?",
      value=FALSE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # sample prop
      sliderTextInput(
      inputId = "cr.prop",  
      label = "Sub-sample proportion",
      choices = c(0.01,seq(from=0.05,to=1,by=0.05)),
      selected = "0.25",
      grid = TRUE),
      # credible interval
      sliderTextInput(
      inputId = "cr.quants",  
      label = "Credible interval (%)",
      choices = c(1,seq(from=5,to=100,by=5)),
      selected = "95",
      grid = TRUE),
      # re-sample historical process
      switchInput(
      inputId = "cr.resample_epsp",  
      label = "Re-sample historical process?",
      value=FALSE,
      onLabel = "TRUE",
      offLabel = "FALSE",
      onStatus = "success", 
      offStatus = "danger"),
      # effort error
      sliderTextInput(
      inputId = "cr.effort_error",  
      label = "CV (Effort error)",
      choices = seq(from=0.01,to=0.2,by=0.01),
      selected = "0.01",
      grid = TRUE)),
    br(),
    br(),
    tags$footer(
      div(style="text-align:center",
        tags$p("version 0.0.1"),
        tags$p(paste("Copyright", format(Sys.time(),"%Y"), "NOAA Fisheries, PIFSC OSAP"))
      )
    )
  ), # End of sidebar

  body = dashboardBody(
    tags$head(tags$style(HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}') )),
    tags$head(tags$style(css)),
    # Start of main tab stuff
    tabItems(
      # **** Introduction ****
      tabItem(tabName="introduction", h2("Introduction"),
        fluidRow(column(12, includeMarkdown(paste0("./r-code/shiny/surplus-production/introduction_index.md"))))
      ), # End of introduction tab

      # **** Summary table ****
      tabItem(tabName="table", h2("Summary table"),
        fluidRow(box(title="Model metrics", collapsed=start_collapsed, solidHeader=TRUE, collapsible=TRUE, status="primary", width=12,
         DT::dataTableOutput("summarytable")))
      ), # End of table tab

      # **** Bayesian diagnostics plots ****
      tabItem(tabName="plots_hmc", h2("Bayesian diagnostics: Convergence"),
        fluidRow(
          box(title="Parcoord", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_hmc_parcoord", height="auto")),
          box(title="Pairs", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_hmc_pairs", height="auto")),
          box(title="Trace", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_hmc_trace", height="auto")),
          box(title="Rhat", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_hmc_rhat", height="auto")),
          box(title="Effective sample size", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_hmc_neff", height="auto")),
          box(title="Autocorrelation plots", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_hmc_acf", height="auto"))
        )
      ), # End of plots.hmc tab
      # **** Bayesian diagnostics plots: PPC ****
      tabItem(tabName="plots_tab_ppc", h2("Bayesian diagnostics: Posterior Predictive Checking (PPC)"),
        fluidRow(
          box(title="Density overlay", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_ppc_dens", height="auto")),
          box(title="ECDF", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_ppc_ecdf", height="auto")),
          box(title="ECDF (PIT)", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_ppc_pit_ecdf", height="auto")),
          box(title="Test statistics", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_ppc_stat", height="auto")),
          box(title="LOO-PIT", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_ppc_loo_pit", height="auto")),
          box(title="LOO-PIT QQ", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_ppc_loo_qq", height="auto")),            
          box(title="LOO Posterior Predicted Interval", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select only one model."),
            plotOutput("plots_ppc_loo_interval", height="auto"))
        )
      ), # End of plots_tab_ppc tab
      # **** Model fits ****
      tabItem(tabName="plots_model_fits", h2("Model fits"),
        fluidRow(
          box(title="Index fit", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_index_fit", height="auto")),
          box(title="Index fit: posterior predicted", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_index_fit_ppd", height="auto")),
          box(title="Index fit: residuals", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_index_fit_residuals", height="auto"))
        )
      ), # End of plots_model_fits tab
      # **** Prior & Posterior: Parameter plots ****
      tabItem(tabName="plots_tab_ppp", h2("Prior & Posterior: Leading parameters"),
        fluidRow(
          box(title="Distributions", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_ppp", height="auto"))
        )
      ), # End of plots.ppts tab
      # **** Prior & Posterior: Time-series plots ****
      tabItem(tabName="plots_tab_ppts", h2("Prior & Posterior: Time-series quantities"),
        fluidRow(
          box(title="Time-series plots", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_ppts", height="auto"))
        )
      ), # End of plots_ppts tab,
      # **** Prior & Posterior: management quantities plots ****
      tabItem(tabName="plots_tab_ppmq", h2("Prior & Posterior: Management quantities"),
        fluidRow(
          box(title="Distributions", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_ppmq", height="auto"))
        )
      ), # End of plots_kbmj tab
      # **** Kobe & Majuro plots ****
      tabItem(tabName="plots_tab_kbmj", h2("Kobe & Majuro plots"),
        fluidRow(
          box(title="Kobe plot", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_kb", height="auto")),
        box(title="Majuro plot", solidHeader=TRUE, collapsible=TRUE, collapsed=TRUE, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_mj", height="auto"))
        )
      ), # End of plots_ppmq tab
      # **** Prior & Posterior: Time-series plots ****
      tabItem(tabName="plots_tab_forecasts", h2("Forecasts"),
        fluidRow(
          box(title="Forecast plots", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_fcast", height="auto"))
        )
      ), # End of plots_forecasts tab
      # **** Catch reconstruction ****
      tabItem(tabName="plots_tab_cr", h2("Catch reconstruction"),
        fluidRow(
          box(title="Time-series plots", solidHeader=TRUE, collapsible=TRUE, collapsed=start_collapsed, status="primary", width=12,
            p("Select at least one model."),
            plotOutput("plots_cr", height="auto"))
        )
      ) # End of plots_forecasts tab
    ) # End of tabItems
  ) # End of dashboardBody
)
