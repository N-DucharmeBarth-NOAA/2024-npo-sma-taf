

server = function(input, output){
  # pixel height for each panel. i.e row height when plotting by species
  height_per_panel = 350

  ref_table_reduced = summary_dt %>%
                as.data.frame(.)

  output$summarytable = DT::renderDataTable({
    summary_df = summary_dt %>%
                 as.data.frame(.,stringsAsFactors=FALSE)
    summary_DT = DT::datatable(summary_df, filter = 'top',rownames=FALSE,
    options = list(scrollX = TRUE, search = list(regex = TRUE, caseInsensitive = FALSE),pageLength = 25))
    return(summary_DT)
  })
  outputOptions(output, "summarytable", suspendWhenHidden = FALSE)

#   filtered_table = reactive({
#     req(input$summarytable_rows_selected)
#     keep_models = c(ref_table_reduced[input$summarytable_rows_selected, ]$id)
#     return(as.data.frame(summary_dt[id%in%keep_models],stringsAsFactors=FALSE))  
#   })

  filtered_id = reactive({
    req(input$summarytable_rows_selected)
    keep_models = c(ref_table_reduced[input$summarytable_rows_selected, ]$run_id)
    return(keep_models)  
  })

  # plots_hmc_parcoord
  output$plots_hmc_parcoord = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1|length(input$hmc.leading_params)<2){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    # map parameters
    parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","lp__"),c("logK","x0","r","sigmao","sigmap","n","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","lp__"))
    colnames(parameter_map) = c("input","transformed","raw")
    plot_dt = rbindlist(lapply(selected_models,function(x)fread(paste0(model_stem,x,"hmc_samples.csv"))))

    if(input$hmc.raw == "TRUE"){
      grab_parameters = parameter_map[match(input$hmc.leading_params,parameter_map[,"input"]),"transformed"]
      plot_title = "Parcoord (transformed)"
      plot_dt = plot_dt %>%
                .[,.(iter,name,value,treedepth,divergent)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"transformed"])]
    } else {
      grab_parameters = parameter_map[match(input$hmc.leading_params,parameter_map[,"input"]),"raw"]
      plot_title = "Parcoord (original)"
      plot_dt = plot_dt %>%
                .[,.(iter,name,value,treedepth,divergent)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"raw"])]
    }

    if(input$hmc.diag=="None"){
      plot_dt = plot_dt[,.(iter,name,value)] %>%
                .[,type:="None"]
    } else if(input$hmc.diag=="Max. treedepth"){  
      if(tmp_summary$treedepth>0){
        max_tree = max(plot_dt$treedepth)
        plot_dt = plot_dt[,.(iter,name,value,treedepth)] %>%
                .[,type:="None"] %>%
                .[treedepth==max_tree,type:="Max treedepth"] %>%
                .[,type:=factor(type,levels=c("None","Max treedepth"))]
      } else {
        plot_dt = plot_dt[,.(iter,name,value,treedepth)] %>%
                .[,type:="None"]
      }
    } else {
      if(tmp_summary$divergent>0){
        plot_dt = plot_dt[,.(iter,name,value,divergent)] %>%
                .[,type:="None"] %>%
                .[divergent==1,type:="Divergent transition"] %>%
                .[,type:=factor(type,levels=c("None","Divergent transition"))]
      } else {
        plot_dt = plot_dt[,.(iter,name,value,divergent)] %>%
                .[,type:="None"]
      }
    }

    if(nrow(plot_dt) == 0){
      return()
    }
        
    p = plot_dt %>%
      ggplot() +
      ggtitle(plot_title) +
			ylab("Value") +
      xlab("Variable") +
      geom_line(aes(x=name,y=value,group=iter,color=type),alpha=0.25,linewidth=1.15) + 
      viridis::scale_color_viridis("Error type",begin = 0.1,end = 0.8,direction = 1,option = "H",discrete=TRUE) +
			viridis::scale_fill_viridis("Error type",begin = 0.1,end = 0.8,direction = 1,option = "H",discrete=TRUE) +
			theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })
  
  # plots_hmc_pairs
  output$plots_hmc_pairs = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1|length(input$hmc.leading_params)<1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    # map parameters
    parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","lp__"),c("logK","x0","r","sigmao_sc","sigmap","n","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","lp__"))
    colnames(parameter_map) = c("input","transformed","raw")
    plot_dt = rbindlist(lapply(selected_models,function(x)fread(paste0(model_stem,x,"hmc_samples.csv"))))

    if(input$hmc.raw == "TRUE"){
      grab_parameters = parameter_map[match(input$hmc.leading_params,parameter_map[,"input"]),"transformed"]
      plot_title = "Pairs (transformed)"
      plot_dt = plot_dt %>%
                .[,.(iter,name,value,treedepth,divergent)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"transformed"])]
    } else {
      grab_parameters = parameter_map[match(input$hmc.leading_params,parameter_map[,"input"]),"raw"]
      plot_title = "Pairs (original)"
      plot_dt = plot_dt %>%
                .[,.(iter,name,value,treedepth,divergent)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"raw"])]
    }

    if(input$hmc.diag=="None"){
      plot_dt = plot_dt[,.(iter,name,value)] %>%
                .[,type:="None"]
    } else if(input$hmc.diag=="Max. treedepth"){  
      if(tmp_summary$treedepth>0){
        max_tree = max(plot_dt$treedepth)
        plot_dt = plot_dt[,.(iter,name,value,treedepth)] %>%
                .[,type:="None"] %>%
                .[treedepth==max_tree,type:="Max treedepth"] %>%
                .[,type:=factor(type,levels=c("None","Max treedepth"))]
      } else {
        plot_dt = plot_dt[,.(iter,name,value,treedepth)] %>%
                .[,type:="None"]
      }
    } else {
      if(tmp_summary$divergent>0){
        plot_dt = plot_dt[,.(iter,name,value,divergent)] %>%
                .[,type:="None"] %>%
                .[divergent==1,type:="Divergent transition"] %>%
                .[,type:=factor(type,levels=c("None","Divergent transition"))]
      } else {
        plot_dt = plot_dt[,.(iter,name,value,divergent)] %>%
                .[,type:="None"]
      }
    }

    if(nrow(plot_dt) == 0){
      return()
    }

            
    plot_dt = plot_dt %>%
        .[,.(type,iter,name,value)] %>%
        dcast(.,type+iter~name)
    p = plot_dt %>%
      ggpairs(., columns = 3:ncol(plot_dt), aes(color = type, alpha = 0.4)) + 
      viridis::scale_color_viridis("Error type",begin = 0.1,end = 0.8,direction = 1,option = "H",discrete=TRUE) +
			viridis::scale_fill_viridis("Error type",begin = 0.1,end = 0.8,direction = 1,option = "H",discrete=TRUE) +
			theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })
  
  # plots_hmc_trace
  output$plots_hmc_trace = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1|length(input$hmc.leading_params)<1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    # map parameters
    parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","lp__"),c("logK","x0","r","sigmao_sc","sigmap","n","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","lp__"))
    colnames(parameter_map) = c("input","transformed","raw")
    plot_dt = rbindlist(lapply(selected_models,function(x)fread(paste0(model_stem,x,"hmc_samples.csv"))))

    if(input$hmc.raw == "TRUE"){
      grab_parameters = parameter_map[match(input$hmc.leading_params,parameter_map[,"input"]),"transformed"]
      plot_title = "Trace (transformed)"
      plot_dt = plot_dt %>%
                .[,.(chain,chain_iter,name,value,treedepth,divergent)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"transformed"])]
    } else {
      grab_parameters = parameter_map[match(input$hmc.leading_params,parameter_map[,"input"]),"raw"]
      plot_title = "Trace (original)"
      plot_dt = plot_dt %>%
                .[,.(chain,chain_iter,name,value,treedepth,divergent)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"raw"])]
    }
    
    if(input$hmc.diag=="None"){
      plot_dt = plot_dt[,.(chain,chain_iter,name,value)] %>%
                .[,type:="None"]
    } else if(input$hmc.diag=="Max. treedepth"){  
      if(tmp_summary$treedepth>0){
        max_tree = max(plot_dt$treedepth)
        plot_dt = plot_dt[,.(chain,chain_iter,name,value,treedepth)] %>%
                .[,type:="None"] %>%
                .[treedepth==max_tree,type:="Max treedepth"] %>%
                .[,type:=factor(type,levels=c("None","Max treedepth"))]
      } else {
        plot_dt = plot_dt[,.(chain,chain_iter,name,value)] %>%
                .[,type:="None"]
      }
    } else {
      if(tmp_summary$divergent>0){
        plot_dt = plot_dt[,.(chain,chain_iter,name,value,divergent)] %>%
                .[,type:="None"] %>%
                .[divergent==1,type:="Divergent transition"] %>%
                .[,type:=factor(type,levels=c("None","Divergent transition"))]
      } else {
        plot_dt = plot_dt[,.(chain,chain_iter,name,value)] %>%
                .[,type:="None"]
      }
    }

    if(nrow(plot_dt) == 0){
      return()
    }

    p = plot_dt %>%
      ggplot() +
      ggtitle(plot_title) +
			ylab("Value") +
      xlab("Iteration") +
      facet_wrap(~name,scales="free_y",ncol=min(c(3,uniqueN(plot_dt$name)))) +
      geom_path(aes(x=chain_iter,y=value,color=as.character(chain),group=paste0(chain,".",name)),linewidth=1.15)
    if(input$hmc.diag!="None"&uniqueN(plot_dt$type)>1){
      p = p + geom_point(data=plot_dt[type!="None"],aes(x=chain_iter,y=value,fill=type),shape=21,size=2)
    }

      p = p + scale_color_brewer("Chain",palette="Blues") +
			        viridis::scale_fill_viridis("Error type",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_hmc_rhat
  output$plots_hmc_rhat = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1|(length(input$hmc.leading_params)<1&input$hmc.eps == "FALSE")){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    # map parameters
    if(input$hmc.eps == "FALSE"){
      parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","lp__"),c("logK","x0","r","sigmao_sc","sigmap","n","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","lp__"))
      colnames(parameter_map) = c("input","transformed","raw")
      target_par = input$hmc.leading_params
    } else {
      parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","epsp","lp__"),c("logK","x0","r","sigmao_sc","sigmap","n","epsp","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","raw_epsp","lp__"))
      colnames(parameter_map) = c("input","transformed","raw")
      target_par = c(input$hmc.leading_params,"epsp")
    }
    
    plot_dt = rbindlist(lapply(selected_models,function(x)fread(paste0(model_stem,x,"rhat.csv"))))

    if(input$hmc.raw == "TRUE"){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"transformed"]
      plot_dt = plot_dt %>%
                .[,.(variable,name,row,rhat)] %>%
                .[name %in% grab_parameters] %>%
                .[!is.na(row),name:=paste0(name,"[",row,"]")]
    } else {
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"raw"]
      plot_dt = plot_dt %>%
                .[,.(variable,name,row,rhat)] %>%
                .[name %in% grab_parameters] %>%
                .[!is.na(row),name:=paste0(name,"[",row,"]")]
    }
    rhats = plot_dt$rhat
    names(rhats) = plot_dt$name
   
    if(nrow(plot_dt) == 0){
      return()
    }

    bayesplot::color_scheme_set(input$hmc.scheme)
    p = bayesplot::mcmc_rhat(rhats, size = 2) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.major.y = element_blank(), 
							panel.grid.minor = element_blank(),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_hmc_neff
  output$plots_hmc_neff = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1|(length(input$hmc.leading_params)<1&input$hmc.eps == "FALSE")){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    # map parameters
    if(input$hmc.eps == "FALSE"){
      parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","lp__"),c("logK","x0","r","sigmao_sc","sigmap","n","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","lp__"))
      colnames(parameter_map) = c("input","transformed","raw")
      target_par = input$hmc.leading_params
    } else {
      parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","epsp","lp__"),c("logK","x0","r","sigmao_sc","sigmap","n","epsp","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","raw_epsp","lp__"))
      colnames(parameter_map) = c("input","transformed","raw")
      target_par = c(input$hmc.leading_params,"epsp")
    }
    
    plot_dt = rbindlist(lapply(selected_models,function(x)fread(paste0(model_stem,x,"neff.csv"))))

    if(input$hmc.raw == "TRUE"){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"transformed"]
      plot_dt = plot_dt %>%
                .[,.(variable,name,row,neff)] %>%
                .[name %in% grab_parameters] %>%
                .[!is.na(row),name:=paste0(name,"[",row,"]")]
    } else {
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"raw"]
      plot_dt = plot_dt %>%
                .[,.(variable,name,row,neff)] %>%
                .[name %in% grab_parameters] %>%
                .[!is.na(row),name:=paste0(name,"[",row,"]")]
    }
    neff = plot_dt$neff
    names(neff) = plot_dt$name
   
    if(nrow(plot_dt) == 0){
      return()
    }

    bayesplot::color_scheme_set(input$hmc.scheme)
    p = bayesplot::mcmc_neff(neff, size = 2) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.major.y = element_blank(),  
							panel.grid.minor = element_blank(),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_hmc_acf
  output$plots_hmc_acf = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1|(length(input$hmc.leading_params)<1&input$hmc.eps == "FALSE")){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    # map parameters
      parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","lp__"),c("logK","x0","r","sigmao_sc","sigmap","n","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","lp__"))
      colnames(parameter_map) = c("input","transformed","raw")
      target_par = input$hmc.leading_params

    plot_dt = rbindlist(lapply(selected_models,function(x)fread(paste0(model_stem,x,"hmc_samples.csv"))))

    if(input$hmc.raw == "TRUE"){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"transformed"]
      plot_df = plot_dt %>%
                .[,.(chain,chain_iter,name,value)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"transformed"])] %>%
                dcast(.,chain+chain_iter~name) %>%
                .[,chain_iter:=NULL] %>%
                setnames(.,"chain","Chain") %>%
                as.data.frame(.)
    } else {
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"raw"]
      plot_df = plot_dt %>%
                .[,.(chain,chain_iter,name,value)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"raw"])] %>%
                dcast(.,chain+chain_iter~name) %>%
                .[,chain_iter:=NULL] %>%
                setnames(.,"chain","Chain") %>%
                as.data.frame(.)
    }

   
    if(nrow(plot_df) == 0){
      return()
    }

    bayesplot::color_scheme_set(input$hmc.scheme)
    p = bayesplot::mcmc_acf_bar(plot_df, lags = as.numeric(as.character(input$hmc.lags))) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.minor.y = element_blank(),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white")) +
			        geom_hline(yintercept=0.5, linetype = 2, color = "gray70")
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_ppc_dens
  output$plots_ppc_dens = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])

    tmp_ssp_summary=fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    tmp_stan_data=fread(paste0(model_stem,selected_models,"stan_data.csv"))
    tmp_settings=fread(paste0(model_stem,selected_models,"settings.csv"))
    tmp_samples=fread(paste0(model_stem,selected_models,"hmc_samples.csv"))
    fit_dt = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$ppc.prop),
                          active=input$ppc.active,
                          calc_std = "FALSE")
    
    obs_cpue_dt = fit_dt[metric=="obs_cpue",.(row,index,value)] %>%
                  setnames(.,"value","y")

    ppd_dt = fit_dt[metric=="ppd_cpue",.(iter,row,index,value)] %>%
             dcast(.,row+index~iter)
    
    yrep_dt = merge(obs_cpue_dt,ppd_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.)
    
    y_vec = yrep_dt$y
    if(input$ppc.group!="TRUE"){
      ygroup_vec = yrep_dt$index
    }

    yrep_mat = yrep_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)

    bayesplot::color_scheme_set(input$ppc.scheme)
    if(input$ppc.group!="TRUE"){
      p = bayesplot::ppc_dens_overlay_grouped(y=y_vec,yrep=yrep_mat,group=index_names[ygroup_vec])
    } else {
      p = bayesplot::ppc_dens_overlay(y=y_vec,yrep=yrep_mat)
    }
    p = p +   theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.minor.y = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white")) 
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


  # plots_ppc_ecdf
  output$plots_ppc_ecdf = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])

    tmp_ssp_summary=fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    tmp_stan_data=fread(paste0(model_stem,selected_models,"stan_data.csv"))
    tmp_settings=fread(paste0(model_stem,selected_models,"settings.csv"))
    tmp_samples=fread(paste0(model_stem,selected_models,"hmc_samples.csv"))
    fit_dt = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$ppc.prop),
                          active=input$ppc.active,
                          calc_std = "FALSE")
    
    obs_cpue_dt = fit_dt[metric=="obs_cpue",.(row,index,value)] %>%
                  setnames(.,"value","y")

    ppd_dt = fit_dt[metric=="ppd_cpue",.(iter,row,index,value)] %>%
             dcast(.,row+index~iter)
    
    yrep_dt = merge(obs_cpue_dt,ppd_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.)
    
    y_vec = yrep_dt$y
    if(input$ppc.group!="TRUE"){
      ygroup_vec = yrep_dt$index
    }

    yrep_mat = yrep_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)

    bayesplot::color_scheme_set(input$ppc.scheme)
    if(input$ppc.group!="TRUE"){
      p = bayesplot::ppc_ecdf_overlay_grouped(y=y_vec,yrep=yrep_mat,group=index_names[ygroup_vec])
    } else {
      p = bayesplot::ppc_ecdf_overlay(y=y_vec,yrep=yrep_mat)
    }
    p = p +   theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.minor.y = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white")) 
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_ppc_pit_ecdf
  output$plots_ppc_pit_ecdf = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])

    tmp_ssp_summary=fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    tmp_stan_data=fread(paste0(model_stem,selected_models,"stan_data.csv"))
    tmp_settings=fread(paste0(model_stem,selected_models,"settings.csv"))
    tmp_samples=fread(paste0(model_stem,selected_models,"hmc_samples.csv"))
    fit_dt = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$ppc.prop),
                          active=input$ppc.active,
                          calc_std = "FALSE")
    
    obs_cpue_dt = fit_dt[metric=="obs_cpue",.(row,index,value)] %>%
                  setnames(.,"value","y")

    ppd_dt = fit_dt[metric=="ppd_cpue",.(iter,row,index,value)] %>%
             dcast(.,row+index~iter)
    
    yrep_dt = merge(obs_cpue_dt,ppd_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.)
    
    y_vec = yrep_dt$y
    if(input$ppc.group!="TRUE"){
      ygroup_vec = yrep_dt$index
    }

    yrep_mat = yrep_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)

    bayesplot::color_scheme_set(input$ppc.scheme)
    if(input$ppc.group!="TRUE"){
      p = bayesplot::ppc_pit_ecdf_grouped(y=y_vec,yrep=yrep_mat,group=index_names[ygroup_vec])
    } else {
      p = bayesplot::ppc_pit_ecdf(y=y_vec,yrep=yrep_mat)
    }
    p = p +   theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.minor.y = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white")) 
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_ppc_stat
  output$plots_ppc_stat = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1){
      return()
    }
    if(!(length(input$ppc.stat)%in%c(1,2))){
      return(warning("Must select at least 1 or at most 2 PPC statistics."))
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])

    tmp_ssp_summary=fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    tmp_stan_data=fread(paste0(model_stem,selected_models,"stan_data.csv"))
    tmp_settings=fread(paste0(model_stem,selected_models,"settings.csv"))
    tmp_samples=fread(paste0(model_stem,selected_models,"hmc_samples.csv"))
    fit_dt = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$ppc.prop),
                          active=input$ppc.active,
                          calc_std = "FALSE")
    
    obs_cpue_dt = fit_dt[metric=="obs_cpue",.(row,index,value)] %>%
                  setnames(.,"value","y")

    ppd_dt = fit_dt[metric=="ppd_cpue",.(iter,row,index,value)] %>%
             dcast(.,row+index~iter)
    
    yrep_dt = merge(obs_cpue_dt,ppd_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.)
    
    y_vec = yrep_dt$y
    if(input$ppc.group!="TRUE"){
      ygroup_vec = yrep_dt$index
    }

    yrep_mat = yrep_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)

    bayesplot::color_scheme_set(input$ppc.scheme)
    if(length(input$ppc.stat)==1){
      if(input$ppc.group!="TRUE"){
        p = bayesplot::ppc_stat_grouped(y=y_vec,yrep=yrep_mat,group=index_names[ygroup_vec],stat=input$ppc.stat)
      } else {
        p = bayesplot::ppc_stat(y=y_vec,yrep=yrep_mat,stat=input$ppc.stat)
      }
    } else {
        p = bayesplot::ppc_stat_2d(y=y_vec,yrep=yrep_mat,stat=input$ppc.stat)
    }
    
    p = p +   theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.minor.y = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white")) 
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_ppc_loo_pit
  output$plots_ppc_loo_pit = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1){
      return()
    }
    require(rstantools)
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])

    tmp_ssp_summary=fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    tmp_stan_data=fread(paste0(model_stem,selected_models,"stan_data.csv"))
    tmp_settings=fread(paste0(model_stem,selected_models,"settings.csv"))
    tmp_samples=fread(paste0(model_stem,selected_models,"hmc_samples.csv"))
    fit_dt = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$ppc.prop),
                          active="TRUE",
                          calc_std = "FALSE")
    
    obs_cpue_dt = fit_dt[metric=="obs_cpue",.(row,index,value)] %>%
                  setnames(.,"value","y")

    ppd_dt = fit_dt[metric=="ppd_cpue",.(iter,row,index,value)] %>%
             dcast(.,row+index~iter)
    
    lik1_dt = ssp_calc_likelihood(tmp_samples,tmp_stan_data) %>%
             setnames(.,c("T","I","value"),c("row","index","ll")) %>%
             .[iter %in% unique(fit_dt[metric=="ppd_cpue"]$iter)] %>%
             .[,.(iter,row,index,ll)] %>%
             dcast(.,row+index~iter)

    
    yrep_dt = merge(obs_cpue_dt,ppd_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.) 
    lik_dt = merge(obs_cpue_dt,lik1_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.)
    
    y_vec = yrep_dt$y
    if(input$ppc.group!="TRUE"){
      ygroup_vec = yrep_dt$index
    }

    yrep_mat = yrep_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)
    log_lik_mat = lik_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)
    
    r_eff = loo::relative_eff(exp(log_lik_mat), unique(tmp_samples[,.(iter,chain)])$chain, cores = 1)
    loo = loo::loo(log_lik_mat,r_eff=r_eff,cores=1,save_psis =TRUE)
    psis = loo$psis_object
    lw = weights(psis)

    bayesplot::color_scheme_set(input$ppc.scheme)
    p = bayesplot::ppc_loo_pit_overlay(y=y_vec,yrep=yrep_mat,lw=lw)
    p = p +   theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.minor.y = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_ppc_loo_qq
  output$plots_ppc_loo_qq = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1){
      return()
    }
    require(rstantools)
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])

    tmp_ssp_summary=fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    tmp_stan_data=fread(paste0(model_stem,selected_models,"stan_data.csv"))
    tmp_settings=fread(paste0(model_stem,selected_models,"settings.csv"))
    tmp_samples=fread(paste0(model_stem,selected_models,"hmc_samples.csv"))
    fit_dt = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$ppc.prop),
                          active="TRUE",
                          calc_std = "FALSE")
    
    obs_cpue_dt = fit_dt[metric=="obs_cpue",.(row,index,value)] %>%
                  setnames(.,"value","y")

    ppd_dt = fit_dt[metric=="ppd_cpue",.(iter,row,index,value)] %>%
             dcast(.,row+index~iter)
    
    lik1_dt = ssp_calc_likelihood(tmp_samples,tmp_stan_data) %>%
             setnames(.,c("T","I","value"),c("row","index","ll")) %>%
             .[iter %in% unique(fit_dt[metric=="ppd_cpue"]$iter)] %>%
             .[,.(iter,row,index,ll)] %>%
             dcast(.,row+index~iter)

    
    yrep_dt = merge(obs_cpue_dt,ppd_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.) 
    lik_dt = merge(obs_cpue_dt,lik1_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.)
    
    y_vec = yrep_dt$y
    if(input$ppc.group!="TRUE"){
      ygroup_vec = yrep_dt$index
    }

    yrep_mat = yrep_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)
    log_lik_mat = lik_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)
    
    r_eff = loo::relative_eff(exp(log_lik_mat), unique(tmp_samples[,.(iter,chain)])$chain, cores = 1)
    loo = loo::loo(log_lik_mat,r_eff=r_eff,cores=1,save_psis =TRUE)
    psis = loo$psis_object
    lw = weights(psis)

    bayesplot::color_scheme_set(input$ppc.scheme)
    p = bayesplot::ppc_loo_pit_qq(y=y_vec,yrep=yrep_mat,lw=lw, compare = input$ppc.qqdist)
    p = p +   theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.minor.y = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


  # plots_ppc_loo_interval
  output$plots_ppc_loo_interval = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) != 1){
      return()
    }
    require(rstantools)
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])

    tmp_ssp_summary=fread(paste0(model_stem,selected_models,"fit_summary.csv"))
    tmp_stan_data=fread(paste0(model_stem,selected_models,"stan_data.csv"))
    tmp_settings=fread(paste0(model_stem,selected_models,"settings.csv"))
    tmp_samples=fread(paste0(model_stem,selected_models,"hmc_samples.csv"))
    fit_dt = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$ppc.prop),
                          active="TRUE",
                          calc_std = "FALSE")
    
    obs_cpue_dt = fit_dt[metric=="obs_cpue",.(row,index,value)] %>%
                  setnames(.,"value","y")

    ppd_dt = fit_dt[metric=="ppd_cpue",.(iter,row,index,value)] %>%
             dcast(.,row+index~iter)
    
    lik1_dt = ssp_calc_likelihood(tmp_samples,tmp_stan_data) %>%
             setnames(.,c("T","I","value"),c("row","index","ll")) %>%
             .[iter %in% unique(fit_dt[metric=="ppd_cpue"]$iter)] %>%
             .[,.(iter,row,index,ll)] %>%
             dcast(.,row+index~iter)

    
    yrep_dt = merge(obs_cpue_dt,ppd_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.) 
    lik_dt = merge(obs_cpue_dt,lik1_dt,by=c("row","index"),all.x=TRUE) %>%
              na.omit(.)
    
    y_vec = yrep_dt$y
    if(input$ppc.group!="TRUE"){
      ygroup_vec = yrep_dt$index
    }

    yrep_mat = yrep_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)
    log_lik_mat = lik_dt %>%
              .[,row:=NULL] %>%
              .[,index:=NULL] %>%
              .[,y:=NULL] %>%
              as.matrix(.) %>%
              t(.)
    
    r_eff = loo::relative_eff(exp(log_lik_mat), unique(tmp_samples[,.(iter,chain)])$chain, cores = 1)
    loo = loo::loo(log_lik_mat,r_eff=r_eff,cores=1,save_psis =TRUE)
    psis = loo$psis_object
    lw = weights(psis)

    bayesplot::color_scheme_set(input$ppc.scheme)
    p = bayesplot::ppc_loo_intervals(y=y_vec,yrep=yrep_mat,psis_object=psis)
    p = p +   theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor.x = element_line(color = 'gray70',linetype = "dotted"),
              panel.grid.minor.y = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


# plots_index_fit
  output$plots_index_fit = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)

    fit_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(fit_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))
      tmp_samples=fread(paste0(model_stem,selected_models[i],"hmc_samples.csv"))
      fit_dt.list[[i]] = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$fits.prop),
                          active=input$fits.active,
                          calc_std = "FALSE")
                          # calc_std=input$fits.calc_std)
    }
    plot_dt_a = rbindlist(fit_dt.list) %>%
              merge(.,tmp_summary[,.(run_id,run_label)],by="run_id") %>%
              .[,group_id:=paste0(run_id,"-",index)] %>%
              .[metric%in%c("obs_cpue","pred_cpue")] %>%
              na.omit(.) %>%
              .[,index:=factor(index,levels=sort(unique(index)),labels=index_names[as.numeric(as.character(sort(unique(index))))])] %>%
              .[row>=1,year:=year_one+(row-1)] %>%
              .[row<1,year:=year_one+(row-1)]

    plot_dt_b = rbindlist(fit_dt.list) %>%
              merge(.,tmp_summary[,.(run_id,run_label)],by="run_id") %>%
              .[,group_id:=paste0(run_id,"-",index)] %>%
              .[metric%in%c("sigmao")] %>%
              na.omit(.) %>%
              .[,index:=factor(index,levels=sort(unique(index)),labels=index_names[as.numeric(as.character(sort(unique(index))))])] %>%
              .[row>=1,year:=year_one+(row-1)] %>%
              .[row<1,year:=year_one+(row-1)] %>%
              .[,.(value=median(value)),by=.(run_id,metric,row,index,run_label,group_id,year)] %>%
              .[,iter:=0] %>%
              .[,.(run_id,metric,iter,row,index,value,run_label,group_id,year)]
    plot_dt = rbind(plot_dt_a,plot_dt_b)
    
    obs_se_dt = plot_dt[metric=="sigmao"] %>%
                  .[,.(run_id,run_label,group_id,index,row,year,value)] %>%
                  .[,se:=round(value,digits=3)]

    obs_cpue_dt = plot_dt[metric=="obs_cpue"] %>%
                  .[,.(run_id,run_label,group_id,index,row,year,value)] %>%
                  merge(.,obs_se_dt[,.(run_id,run_label,group_id,index,row,year,se)],by=c("run_id","run_label","group_id","index","row","year")) %>%
                  .[,obs:=round(value,digits=3)] %>%
                  .[,id2:=as.numeric(as.factor(paste0(index,"_",obs)))] %>%
                  .[,id3:=as.numeric(as.factor(paste0(index,"_",obs,"_",se)))]
    
    pred_cpue_dt = plot_dt[metric=="pred_cpue"] %>%
                  .[,.(run_id,run_label,group_id,index,row,year,iter,value)]
    
    if(input$fits.type=="Median"){
      pred_cpue_dt = pred_cpue_dt[,.(run_label,index,row,year,iter,value)] %>%
                    .[,.(median=median(value)),by=.(run_label,index,row,year)]
    } else if(input$fits.type=="Spaghetti"){
      pred_cpue_dt = pred_cpue_dt[,.(run_label,index,row,year,iter,value)] %>%
                    .[,group_id:=paste0(run_label,"-",index,"-",iter)]
    } else {
      obs_quant = 0.5*(1-as.numeric(input$fits.quants)/100)
      pred_cpue_dt = pred_cpue_dt[,.(run_label,index,row,year,iter,value)] %>%
                    .[,.(median=median(value),upper=quantile(value,probs=1-obs_quant),lower=quantile(value,probs=obs_quant)),by=.(run_label,index,row,year)]
    }

    # check if obs are identical across models 
    # consider interaction with plotting of observation error
    if(length(input_models)>1&mean(table(obs_cpue_dt$id2) %% uniqueN(obs_cpue_dt$run_id) == 0)!= 1&input$fits.obs=="FALSE"){
      stop("Observed CPUE not identical between models. Choose models with the same CPUE or only a single model.")
    } else if(length(input_models)>1&mean(table(obs_cpue_dt$id2) %% uniqueN(obs_cpue_dt$run_id) == 0)!= 1&input$fits.obs=="TRUE"){
      if(mean(table(obs_cpue_dt$id3) %% uniqueN(obs_cpue_dt$run_id) == 0)!= 1){
        stop("Observation error is selected to be plotted however it is not identical. Turn off observation error plotting, choose models with the same observation error or choose a single model.")
      }
    }

    if(input$fits.obs=="TRUE"){
      obs_quant = 0.5*(1-as.numeric(input$fits.quants)/100)
      obs_cpue_dt = obs_cpue_dt[,.(index,row,year,obs,se)] %>%
                    .[,upper:=qlnorm(1-obs_quant,meanlog=log(obs),sdlog=se)] %>%
                    .[,lower:=qlnorm(obs_quant,meanlog=log(obs),sdlog=se)]
    } else {
      obs_cpue_dt = obs_cpue_dt[,.(index,row,year,obs)]
    }

    if(nrow(plot_dt) == 0|nrow(obs_cpue_dt)==0|nrow(pred_cpue_dt)==0){
      return()
    }
    
    p = pred_cpue_dt %>%
      ggplot() +
			ylab("Index") +
      xlab("Year") +
      geom_hline(yintercept=1,linetype="dashed") +
      facet_wrap(~index,ncol=min(c(3,uniqueN(obs_cpue_dt$index))))
      if(input$fits.obs=="TRUE"){
        p = p + geom_segment(data=obs_cpue_dt,aes(x=year,xend=year,y=lower,yend=upper),linewidth=1.05)
      }
      if(input$fits.type=="Median"){
        p = p + geom_line(aes(x=year,y=median,color=run_label),linewidth=1.1)
      } else if(input$fits.type=="Spaghetti"){
        p = p + geom_line(aes(x=year,y=value,color=run_label,group=group_id),alpha=0.1)
      } else {
        p = p + geom_ribbon(aes(x=year,ymin=lower,ymax=upper,fill=run_label),alpha=0.4) +
                geom_line(aes(x=year,y=median,color=run_label),linewidth=1.1)
      }
      p = p + geom_point(data=obs_cpue_dt,aes(x=year,y=obs),color="white",fill="black",shape=21,size=3)

      p = p + geom_hline(yintercept=0) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			        
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


# plots_index_fit_ppd
  output$plots_index_fit_ppd = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)

    fit_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(fit_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))
      tmp_samples=fread(paste0(model_stem,selected_models[i],"hmc_samples.csv"))
      fit_dt.list[[i]] = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$fits.prop),
                          active=input$fits.active,
                          calc_std = "FALSE")
                          # calc_std=input$fits.calc_std)
    }
    plot_dt = rbindlist(fit_dt.list) %>%
              merge(.,tmp_summary[,.(run_id,run_label)],by="run_id") %>%
              .[,group_id:=paste0(run_id,"-",index)] %>%
              .[metric%in%c("obs_cpue","sigmao","ppd_cpue")] %>%
              na.omit(.) %>%
              .[,index:=factor(index,levels=sort(unique(index)),labels=index_names[as.numeric(as.character(sort(unique(index))))])] %>%
              .[row>=1,year:=year_one+(row-1)] %>%
              .[row<1,year:=year_one+(row-1)]
    
    obs_se_dt = plot_dt[metric=="sigmao"] %>%
                  .[,.(run_id,run_label,group_id,index,row,year,value)] %>%
                  .[,se:=round(value,digits=3)]

    obs_cpue_dt = plot_dt[metric=="obs_cpue"] %>%
                  .[,.(run_id,run_label,group_id,index,row,year,value)] %>%
                  merge(.,obs_se_dt[,.(run_id,run_label,group_id,index,row,year,se)],by=c("run_id","run_label","group_id","index","row","year")) %>%
                  .[,obs:=round(value,digits=3)] %>%
                  .[,id2:=as.numeric(as.factor(paste0(index,"_",obs)))] %>%
                  .[,id3:=as.numeric(as.factor(paste0(index,"_",obs,"_",se)))]
    
    pred_cpue_dt = plot_dt[metric=="ppd_cpue"] %>%
                  .[,.(run_id,run_label,group_id,index,row,year,iter,value)]
    
    if(input$fits.type=="Median"){
      pred_cpue_dt = pred_cpue_dt[,.(run_label,index,row,year,iter,value)] %>%
                    .[,.(median=median(value)),by=.(run_label,index,row,year)]
    } else if(input$fits.type=="Spaghetti"){
      pred_cpue_dt = pred_cpue_dt[,.(run_label,index,row,year,iter,value)] %>%
                    .[,group_id:=paste0(run_label,"-",index,"-",iter)]
    } else {
      obs_quant = 0.5*(1-as.numeric(input$fits.quants)/100)
      pred_cpue_dt = pred_cpue_dt[,.(run_label,index,row,year,iter,value)] %>%
                    .[,.(median=median(value),upper=quantile(value,probs=1-obs_quant),lower=quantile(value,probs=obs_quant)),by=.(run_label,index,row,year)]
    }

    # check if obs are identical across models 
    # consider interaction with plotting of observation error
    if(length(input_models)>1&mean(table(obs_cpue_dt$id2) %% uniqueN(obs_cpue_dt$run_id) == 0)!= 1&input$fits.obs=="FALSE"){
      stop("Observed CPUE not identical between models. Choose models with the same CPUE or only a single model.")
    } else if(length(input_models)>1&mean(table(obs_cpue_dt$id2) %% uniqueN(obs_cpue_dt$run_id) == 0)!= 1&input$fits.obs=="TRUE"){
      if(mean(table(obs_cpue_dt$id3) %% uniqueN(obs_cpue_dt$run_id) == 0)!= 1){
        stop("Observation error is selected to be plotted however it is not identical. Turn off observation error plotting, choose models with the same observation error or choose a single model.")
      }
    }

    if(input$fits.obs=="TRUE"){
      obs_quant = 0.5*(1-as.numeric(input$fits.quants)/100)
      obs_cpue_dt = obs_cpue_dt[,.(index,row,year,obs,se)] %>%
                    .[,upper:=qlnorm(1-obs_quant,meanlog=log(obs),sdlog=se)] %>%
                    .[,lower:=qlnorm(obs_quant,meanlog=log(obs),sdlog=se)]
    } else {
      obs_cpue_dt = obs_cpue_dt[,.(index,row,year,obs)]
    }

    if(nrow(plot_dt) == 0|nrow(obs_cpue_dt)==0|nrow(pred_cpue_dt)==0){
      return()
    }
    
    p = pred_cpue_dt %>%
      ggplot() +
			ylab("Index") +
      xlab("Year") +
      geom_hline(yintercept=1,linetype="dashed") +
      facet_wrap(~index,ncol=min(c(3,uniqueN(obs_cpue_dt$index))))
      if(input$fits.obs=="TRUE"){
        p = p + geom_segment(data=obs_cpue_dt,aes(x=year,xend=year,y=lower,yend=upper),linewidth=1.05)
      }
      if(input$fits.type=="Median"){
        p = p + geom_line(aes(x=year,y=median,color=run_label),linewidth=1.1)
      } else if(input$fits.type=="Spaghetti"){
        p = p + geom_line(aes(x=year,y=value,color=run_label,group=group_id),alpha=0.1)
      } else {
        p = p + geom_ribbon(aes(x=year,ymin=lower,ymax=upper,fill=run_label),alpha=0.4) +
                geom_line(aes(x=year,y=median,color=run_label),linewidth=1.1)
      }
      p = p + geom_point(data=obs_cpue_dt,aes(x=year,y=obs),color="white",fill="black",shape=21,size=3)

      p = p + geom_hline(yintercept=0) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			        
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


# plots_index_fit_residuals
  output$plots_index_fit_residuals = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)

    fit_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(fit_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))
      tmp_samples=fread(paste0(model_stem,selected_models[i],"hmc_samples.csv"))
      if(input$fits.resid=="Standardized"){
        fit_dt.list[[i]] = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$fits.prop),
                          active=input$fits.active,
                          calc_std="TRUE")
      } else {
        fit_dt.list[[i]] = ssp_extract_cpue_fit(ssp_summary=tmp_ssp_summary,
                          samples_dt=tmp_samples,
                          stan_data=tmp_stan_data,
                          settings=tmp_settings,
                          sub_sample_prop=as.numeric(input$fits.prop),
                          active=input$fits.active,
                          calc_std="FALSE")
      }
      
    }
    
    if(input$fits.resid=="Ordinary"){
      plot_dt = rbindlist(fit_dt.list) %>%
              merge(.,tmp_summary[,.(run_id,run_label)],by="run_id") %>%
              .[,group_id:=paste0(run_label,"-",index)] %>%
              .[metric%in%c("residual")] %>%
              .[,.(value=median(value)),by=.(run_id,run_label,group_id,metric,row,index)]
      ylab_txt = "Ordinary residual"
    } else if(input$fits.resid=="Standardized"){
      plot_dt = rbindlist(fit_dt.list) %>%
              merge(.,tmp_summary[,.(run_id,run_label)],by="run_id") %>%
              .[,group_id:=paste0(run_label,"-",index)] %>%
              .[metric%in%c("std_residual")] %>%
              .[,.(value=median(value)),by=.(run_id,run_label,group_id,metric,row,index)]
      ylab_txt = "Standardized residual"
    } else {
      plot_dt = rbindlist(fit_dt.list) %>%
              merge(.,tmp_summary[,.(run_id,run_label)],by="run_id") %>%
              .[,group_id:=paste0(run_label,"-",index)] %>%
              .[metric%in%c("pit_residual")] %>%
              .[,.(run_id,run_label,group_id,metric,row,index,value)]
      ylab_txt = "PIT residual"
    }

    if(nrow(plot_dt)==0){
      return()
    }
    # jitter year
    step = 1/(length(input_models) + 1)
    jitter_seq = seq(from=-0.5,to=0.5,by=step)[-1]

    plot_dt = plot_dt %>%
              na.omit(.) %>%
              .[,index:=factor(index,levels=sort(unique(index)),labels=index_names[as.numeric(as.character(sort(unique(index))))])] %>%
              .[,run_label:=factor(run_label,levels=sort(unique(run_label)))] %>%
              .[row>=1,year:=year_one+(row-1)] %>%
              .[row<1,year:=year_one+(row-1)] %>%
              .[,year:=year+jitter_seq[as.numeric(run_label)]]
    
    # calculate runs test
    if(length(input_models) == 1){
      u_index = unique(plot_dt$index)
      runs_vec = rep(NA,length(u_index))
      if(input$fits.resid=="PIT"){
        for(i in 1:length(u_index)){
          runs_vec[i] = randtests::runs.test(plot_dt[index==u_index[i]]$value-0.5, threshold = 0, alternative = "left.sided")[["p.value"]]
        }
        
        runs_vec = ifelse(runs_vec<0.05,"Runs test: Fail","Runs test: Pass")
        runs_dt = data.table(index=u_index,runs_test=runs_vec) %>%
                .[,x:=min(plot_dt$year)+0.05*diff(range(plot_dt$year))] %>%
                .[,y:=0.9]
      } else {
        for(i in 1:length(u_index)){
          runs_vec[i] = randtests::runs.test(plot_dt[index==u_index[i]]$value, threshold = 0, alternative = "left.sided")[["p.value"]]
        }
        
        runs_vec = ifelse(runs_vec>0.05,"Runs test: Fail","Runs test: Pass")
        runs_dt = data.table(index=u_index,runs_test=runs_vec) %>%
                .[,x:=min(plot_dt$year)+0.05*diff(range(plot_dt$year))] %>%
                .[,y:=0.9*max(plot_dt$value)]
      }


    }

    # # get leverage
    # if(input$fits.calc_std){
    #   leverage_dt = rbindlist(fit_dt.list) %>%
    #           .[,index:=factor(index,levels=sort(unique(index)),labels=index_names[as.numeric(as.character(sort(unique(index))))])] %>%
    #           merge(.,tmp_summary[,.(run_id,run_label)],by="run_id") %>%
    #           .[,group_id:=paste0(run_id,"-",index)] %>%
    #           .[metric%in%c("leverage")] %>%
    #           .[,.(leverage=median(value)),by=.(run_id,run_label,group_id,row,index)] %>%
    #           .[,mean_leverage:=mean(leverage),by=.(run_id,run_label,group_id,row,index)] %>%
    #           .[,std_leverage:=leverage/mean_leverage] %>%
    #           .[,outlier:="FALSE"] %>%
    #           .[std_leverage>2,outlier:="TRUE"]
    #   plot_dt = merge(plot_dt,leverage_dt[,.(run_label,row,index,outlier)],by=c("run_label","row","index"))
    # }
    
    if(input$fits.resid!="PIT"){
        p = plot_dt %>%
        ggplot() +
        ylab(ylab_txt) +
        xlab("Year") +
        geom_hline(yintercept=0) +
        facet_wrap(~index,ncol=min(c(3,uniqueN(plot_dt$index)))) +
        geom_segment(aes(x=year,xend=year,y=0,yend=value,color=run_label,group=group_id),linewidth=1.05) +
        geom_point(aes(x=year,y=value,fill=run_label,group=group_id),color="black",shape=21,size=3,alpha=0.5) +
         viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
    } else {
        p = plot_dt %>%
        ggplot() +
        ylab(ylab_txt) +
        xlab("Year") +
        ylim(0,1) +
        geom_hline(yintercept=0.5) +
        facet_wrap(~index,ncol=min(c(3,uniqueN(plot_dt$index)))) +
        geom_segment(aes(x=year,xend=year,y=0.5,yend=value,color=run_label,group=group_id),linewidth=1.05) +
        geom_point(aes(x=year,y=value,fill=run_label,group=group_id),color="black",shape=21,size=3,alpha=0.5) +
         viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
     }

     if(length(input_models) == 1){
      p = p + geom_label(data=runs_dt,aes(x=x,y=y,label=runs_test),hjust="inward")
     }
    
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

  # plots_ppp
  output$plots_ppp = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1|(length(input$ppp.leading_params)<1)){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)
    # map parameters
      parameter_map = cbind(c("logK","x0","r","sigmao","sigmap","shape","lp__"),c("logK","x0","r","sigmao_sc","sigmap","n","lp__"),c("raw_logK","raw_logx0","raw_logr","raw_logsigmao","raw_logsigmap","raw_logn","lp__"))
      colnames(parameter_map) = c("input","transformed","raw")
      target_par = input$ppp.leading_params

    posterior_dt = rbindlist(lapply(selected_models,function(x)fread(paste0(model_stem,x,"hmc_samples.csv")))) %>%
                   setnames(.,"value","Posterior")

    prior_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(prior_dt.list)){
      prior_dt.list[[i]] = ssp_prior_pushforward(ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv")),
                                                stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv")),
                                                settings=fread(paste0(model_stem,selected_models[i],"settings.csv")))
    }
    prior_dt = rbindlist(prior_dt.list) %>%
                   setnames(.,"value","Prior")
    
    plot_dt = merge(prior_dt,posterior_dt[,.(run_id,iter,chain,chain_iter,variable,name,row,col,Posterior)],by=c("run_id","iter","chain","chain_iter","variable","name","row","col"))

    if(input$ppp.raw == "TRUE"){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"transformed"]
      plot_dt = plot_dt %>%
                .[,.(run_id,iter,name,Prior,Posterior)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"transformed"])]
    } else {
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"raw"]
      plot_dt = plot_dt %>%
                .[,.(run_id,iter,name,Prior,Posterior)] %>%
                .[name %in% grab_parameters] %>%
                .[,name:=factor(name,levels=parameter_map[,"raw"])] 
    }

    
    if(input$ppp.combine=="TRUE"){
      plot_dt = merge(plot_dt,tmp_summary[,.(run_id,run_label)]) %>%
              .[,.(run_label,iter,name,Prior,Posterior)] %>%
              melt(.,id.vars=c("run_label","iter","name")) %>%
              .[,variable:=factor(variable,levels=c("Prior","Posterior"))] %>%
                .[variable=="Posterior",run_label:="Combined posterior"] %>%
                .[variable=="Prior",run_label:="Combined prior"] %>%
                .[,run_label:=factor(run_label,levels=c("Combined prior","Combined posterior"))]
    } else {
      plot_dt = merge(plot_dt,tmp_summary[,.(run_id,run_label)]) %>%
              .[,.(run_label,iter,name,Prior,Posterior)] %>%
              melt(.,id.vars=c("run_label","iter","name")) %>%
              .[,variable:=factor(variable,levels=c("Prior","Posterior"))]
    }
    if(input$ppp.show=="Prior"){
      plot_dt = plot_dt[variable=="Prior"]
    }
    if(input$ppp.show=="Posterior"){
      plot_dt = plot_dt[variable=="Posterior"]
    }

   
    if(nrow(plot_dt) == 0){
      return()
    }

    p = plot_dt %>%
      ggplot() +
			ylab("Density") +
      xlab("Parameter") +
      facet_wrap(~name,scales="free_x",ncol=min(c(3,uniqueN(plot_dt$name))))
      if(input$ppp.show=="Prior"|input$ppp.show=="Both"){
        p = p + geom_density(data=plot_dt[variable=="Prior"],aes(x=value,y=after_stat(scaled),color=run_label,linetype=variable),linewidth=1.15)
      }
      if(input$ppp.show=="Posterior"|input$ppp.show=="Both"){
        p = p + geom_density(data=plot_dt[variable=="Posterior"],aes(x=value,y=after_stat(scaled),color=run_label,fill=run_label,linetype=variable),alpha=0.4,linewidth=1.15)
      }
      
      p = p + scale_linetype_manual("Distribution type", values=c("dotted","solid"),drop=FALSE) +
              geom_hline(yintercept=0) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

    # plots_ppts
  output$plots_ppts = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1|(length(input$ppts.var)<1)){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)
    # map parameters
      parameter_map = cbind(c("Depletion (D)","Population (P)","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","Removals","Process error","Process error (raw)","Process error (mult.)","Surplus production"),
                            c("D","P","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","removals","dev","raw_epsp","epsilon_p","surplus_production"))
      colnames(parameter_map) = c("input","grab")
      target_par = input$ppts.var

    posterior_dt.list = prior_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(prior_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))
      if(input$ppts.show%in% c("Both","Prior")){
        tmp_samples = ssp_prior_pushforward(ssp_summary=tmp_ssp_summary,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings)
                                    
        prior_dt.list[[i]] = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = tmp_samples,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$ppts.prop))
      }
      if(input$ppts.show%in% c("Both","Posterior")){
        posterior_dt.list[[i]] = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = fread(paste0(model_stem,selected_models[i],"hmc_samples.csv")),
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$ppts.prop))
      }
    }

    if(input$ppts.show%in% c("Both","Prior")){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      prior_dt = rbindlist(prior_dt.list) %>% .[name %in% grab_parameters] %>% .[,name:=factor(name,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"],labels=parameter_map[match(target_par,parameter_map[,"input"]),"input"])] %>%
                .[,type:="Prior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    }
 
    if(input$ppts.show%in% c("Both","Posterior")){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      posterior_dt = rbindlist(posterior_dt.list) %>% .[name %in% grab_parameters] %>% .[,name:=factor(name,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"],labels=parameter_map[match(target_par,parameter_map[,"input"]),"input"])] %>%
                .[,type:="Posterior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    }   

    if(input$ppts.show=="Both"){
      plot_dt = rbind(prior_dt,posterior_dt) %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    } else if(input$ppts.show=="Posterior"){
      plot_dt = posterior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    } else {
      plot_dt = prior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    }

    if(input$ppts.combine=="TRUE"){
      plot_dt = plot_dt %>%
                .[type=="Posterior",run_label:="Combined posterior"] %>%
                .[type=="Prior",run_label:="Combined prior"] %>%
                .[,run_label:=factor(run_label,levels=c("Combined prior","Combined posterior"))]
    } 

    if(nrow(plot_dt) == 0){
      return()
    }

    plot_dt = plot_dt %>%
         .[!is.na(value)] %>%
        .[,.(med=median(value),avg=mean(value),lp=quantile(value,probs=0.5-(0.5*((as.numeric(input$ppts.quants)-1e-1)/100))),up=quantile(value,probs=0.5+(0.5*((as.numeric(input$ppts.quants)-1e-1)/100)))),by=.(run_label,type,name,row)] %>%
        .[row>=1,year:=year_one+(row-1)] %>%
        .[row<1,year:=year_one+(row-1)] %>%
        .[name %in% c("Process error","Process error (raw)")&row>0,year:=year+1]
    
    p = plot_dt %>% ggplot() +
			ylab("Metric") +
      xlab("Year") +
      facet_wrap(~name,scales="free_y",ncol=min(c(4,uniqueN(plot_dt$name))))
      if(input$ppts.show=="Prior"|input$ppts.show=="Both"){
        p = p + geom_ribbon(data=plot_dt[type=="Prior"],aes(x=year,ymin=lp,ymax=up,fill=run_label),alpha=0.2,linewidth=1.15)
      }
      if(input$ppts.show=="Posterior"|input$ppts.show=="Both"){
        p = p + geom_ribbon(data=plot_dt[type=="Posterior"],aes(x=year,ymin=lp,ymax=up,fill=run_label),alpha=0.4,linewidth=1.15)
      }
      if(input$ppts.show=="Prior"|input$ppts.show=="Both"){
        p = p + geom_path(data=plot_dt[type=="Prior"],aes(x=year,y=med,color=run_label,linetype=type),alpha=0.8,linewidth=1.15)
      }
      if(input$ppts.show=="Posterior"|input$ppts.show=="Both"){
        p = p + geom_path(data=plot_dt[type=="Posterior"],aes(x=year,y=med,color=run_label,linetype=type),linewidth=1.15)
      }
      
      p = p + scale_linetype_manual("Distribution type", values=c("dotted","solid"),drop=FALSE) +
              geom_hline(yintercept=0) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


    # plots_ppmq
  output$plots_ppmq = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1|(length(input$ppmq.var)<1)){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)
    # map parameters
      parameter_map = cbind(c("init_D_Dmsy","init_P_Pmsy","init_U_Umsy","init_F_Fmsy","init_D","init_P","init_F","init_U","recent_D_Dmsy","recent_P_Pmsy","recent_U_Umsy","recent_F_Fmsy","recent_D","recent_P","recent_F","recent_U"),
                            c("init_D_Dmsy","init_P_Pmsy","init_U_Umsy","init_F_Fmsy","init_D","init_P","init_F","init_U","recent_D_Dmsy","recent_P_Pmsy","recent_U_Umsy","recent_F_Fmsy","recent_D","recent_P","recent_F","recent_U"))
      colnames(parameter_map) = c("input","grab")
      target_par = input$ppmq.var

    posterior_dt.list = prior_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(prior_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))
      if(input$ppmq.show%in% c("Both","Prior")){
        tmp_samples = ssp_prior_pushforward(ssp_summary=tmp_ssp_summary,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings)
                                    
        tmp_ts = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = tmp_samples,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$ppmq.prop))
        prior_dt.list[[i]] = ssp_derived_quants_init_recent(ssp_summary=tmp_ssp_summary,derived_quants_ts=tmp_ts,years_avg=as.numeric(input$ppmq.avg_year),exclude_last=as.numeric(input$ppmq.last_year))
      }
      if(input$ppmq.show%in% c("Both","Posterior")){
        tmp_ts = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = fread(paste0(model_stem,selected_models[i],"hmc_samples.csv")),
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$ppmq.prop))
        posterior_dt.list[[i]] = ssp_derived_quants_init_recent(ssp_summary=tmp_ssp_summary,derived_quants_ts=tmp_ts,years_avg=as.numeric(input$ppmq.avg_year),exclude_last=as.numeric(input$ppmq.last_year))
      }
    }

    if(input$ppmq.show%in% c("Both","Prior")){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      prior_dt = rbindlist(prior_dt.list) %>% .[variable %in% grab_parameters] %>% .[,variable:=factor(variable,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"],labels=parameter_map[match(target_par,parameter_map[,"input"]),"input"])] %>%
                .[,type:="Prior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    }
 
    if(input$ppmq.show%in% c("Both","Posterior")){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      posterior_dt = rbindlist(posterior_dt.list) %>% .[variable %in% grab_parameters] %>% .[,variable:=factor(variable,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"],labels=parameter_map[match(target_par,parameter_map[,"input"]),"input"])] %>%
                .[,type:="Posterior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    }   

    if(input$ppmq.show=="Both"){
      plot_dt = rbind(prior_dt,posterior_dt) %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    } else if(input$ppmq.show=="Posterior"){
      plot_dt = posterior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    } else {
      plot_dt = prior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    }

    if(input$ppmq.combine=="TRUE"){
      plot_dt = plot_dt %>%
                .[type=="Posterior",run_label:="Combined posterior"] %>%
                .[type=="Prior",run_label:="Combined prior"] %>%
                .[,run_label:=factor(run_label,levels=c("Combined prior","Combined posterior"))]
    } 

    if(nrow(plot_dt) == 0){
      return()
    }

    p = plot_dt %>%
      ggplot() +
			ylab("Density") +
      xlab("Parameter") +
      facet_wrap(~variable,scales="free_x",ncol=min(c(4,uniqueN(plot_dt$variable))))
      if(input$ppmq.show=="Prior"|input$ppmq.show=="Both"){
        p = p + geom_density(data=plot_dt[type=="Prior"],aes(x=value,y=after_stat(scaled),color=run_label,linetype=type),linewidth=1.15)
      }
      if(input$ppmq.show=="Posterior"|input$ppmq.show=="Both"){
        p = p + geom_density(data=plot_dt[type=="Posterior"],aes(x=value,y=after_stat(scaled),color=run_label,fill=run_label,linetype=type),alpha=0.4,linewidth=1.15)
      }
      
      p = p + scale_linetype_manual("Distribution type", values=c("dotted","solid"),drop=FALSE) +
              geom_hline(yintercept=0) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


    # plots_kb
  output$plots_kb = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)
    # map parameters
      parameter_map = cbind(c("Depletion (D)","Population (P)","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","Removals","Process error","Process error (raw)","Surplus production"),
                            c("D","P","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","removals","dev","raw_epsp","surplus_production"))
      colnames(parameter_map) = c("input","grab")
      target_par = c("P_Pmsy","F_Fmsy")

    posterior_dt.list = prior_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(prior_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))
      if(input$kbmj.show%in% c("Both","Prior")){
        tmp_samples = ssp_prior_pushforward(ssp_summary=tmp_ssp_summary,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings)
                                    
        prior_dt.list[[i]] = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = tmp_samples,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$kbmj.prop))
      }
      if(input$kbmj.show%in% c("Both","Posterior")){
        posterior_dt.list[[i]] = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = fread(paste0(model_stem,selected_models[i],"hmc_samples.csv")),
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$kbmj.prop))
      }
    }

    if(input$kbmj.show%in% c("Both","Prior")){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      prior_dt = rbindlist(prior_dt.list) %>% .[name %in% grab_parameters] %>% .[,name:=factor(name,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"],labels=parameter_map[match(target_par,parameter_map[,"input"]),"input"])] %>%
                .[,type:="Prior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    }
 
    if(input$kbmj.show%in% c("Both","Posterior")){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      posterior_dt = rbindlist(posterior_dt.list) %>% .[name %in% grab_parameters] %>% .[,name:=factor(name,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"],labels=parameter_map[match(target_par,parameter_map[,"input"]),"input"])] %>%
                .[,type:="Posterior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    }   

    if(input$kbmj.show=="Both"){
      plot_dt = rbind(prior_dt,posterior_dt) %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    } else if(input$kbmj.show=="Posterior"){
      plot_dt = posterior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    } else {
      plot_dt = prior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    }

    if(input$kbmj.combine=="TRUE"){
      plot_dt = plot_dt %>%
                .[type=="Posterior",run_label:="Combined posterior"] %>%
                .[type=="Prior",run_label:="Combined prior"] %>%
                .[,run_label:=factor(run_label,levels=c("Combined prior","Combined posterior"))]
    } 

    if(nrow(plot_dt) == 0){
      return()
    }

    if(input$kbmj.uncertainty=="TRUE"){
      contour_points_dt = plot_dt[row==max(plot_dt$row)] %>%
                   .[,.(type,run_label,name,iter,value)] %>%
                   dcast(.,type+run_label+iter~name) %>%
                   .[,group_id:=paste0(type,"-",run_label)]
      unique_id = unique(contour_points_dt$group_id)
      contour_dt.list = as.list(rep(NA,length(unique_id)))
      for(i in 1:length(unique_id)){
        tmp_contour = contour_points_dt[group_id == unique_id[i]]

        mv.kde = MASS::kde2d(tmp_contour$P_Pmsy, tmp_contour$F_Fmsy,n = as.numeric(input$kbmj.resolution),lims=c(range(tmp_contour$P_Pmsy)*c(0.75,1.25),range(tmp_contour$F_Fmsy)*c(0.75,1.25)))
        dx = diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
        dy = diff(mv.kde$y[1:2])
        sz = sort(mv.kde$z)
        c1 = cumsum(sz) * dx * dy
        dimnames(mv.kde$z) = list(mv.kde$x,mv.kde$y)
        dc = reshape2::melt(mv.kde$z)
        dc$prob = approx(sz,1-c1,dc$value)$y
        dc = as.data.table(dc[,c("Var1","Var2","prob")]) %>%
            dcast(.,Var1~Var2)
        kd = MASS::kde2d(tmp_contour$P_Pmsy, tmp_contour$F_Fmsy,n = as.numeric(input$kbmj.resolution))
        kd$x = dc$Var1
        kd$y = as.numeric(colnames(dc)[-1])
        kd$z = as.matrix(dc)[,-1]
        cntr = contourLines(kd,levels=as.numeric(input$kbmj.quants)/100)
        cntr_ln = length(cntr)
        if(length(cntr)>1){
          cntr_dt.list = as.list(rep(NA,cntr_ln))
          for(j in 1:cntr_ln){
            cntr_dt.list[[j]] = as.data.table(cntr[[j]]) %>%
            .[,cntr_id:=j] %>%
            .[,.(cntr_id,x,y)] %>%
            setnames(.,c("x","y"),target_par) %>%
            .[,group_id:=unique(tmp_contour$group_id)]
            cntr_dt.list[[j]] = rbind(cntr_dt.list[[j]],cntr_dt.list[[j]][1])
          }
          cntr_dt = rbindlist(cntr_dt.list)
        } else {
          cntr_dt = as.data.table(cntr[[1]]) %>%
            .[,cntr_id:=1] %>%
            .[,.(cntr_id,x,y)] %>%
            setnames(.,c("x","y"),target_par) %>%
            .[,group_id:=unique(tmp_contour$group_id)]
            cntr_dt = rbind(cntr_dt,cntr_dt[1])
        }


        contour_dt.list[[i]] = merge(unique(tmp_contour[,.(type,run_label,group_id)]),cntr_dt,by="group_id")
      }
      contour_dt = rbindlist(contour_dt.list) %>%
                   .[,plot_id:=paste0(group_id,"-",cntr_id)] %>%
                   .[,type:=sapply(group_id,function(x)strsplit(x,"-")[[1]][1])] %>%
                   .[,run_label:=sapply(group_id,function(x)strsplit(x,"-")[[1]][2])] %>%
                   .[,type:=factor(type,levels=c("Prior","Posterior"))]
      if(input$kbmj.combine=="TRUE"){
        contour_dt = contour_dt %>%
                    .[,run_label:=factor(run_label,levels=c("Combined prior","Combined posterior"))]
      }
    }

    plot_dt = plot_dt %>%
         .[!is.na(value)] %>%
        .[,.(med=median(value)),by=.(run_label,type,name,row)] %>%
        dcast(.,run_label+type+row~name)
    
    p = plot_dt %>% ggplot() +
			xlab(expression(P/P["MSY"])) +
		  ylab(expression(F/F["MSY"])) +
      coord_fixed(ylim=c(0,2.25),xlim=c(0,2.25)) +
      scale_x_continuous(expand = expansion(mult=c(0,0.05))) +
      scale_y_continuous(expand = expansion(mult=c(0,0.05))) +
      geom_polygon(data=data.table(P_Pmsy=c(0,1,1,0,0),F_Fmsy=c(0,0,1,1,0)),aes(x=P_Pmsy,y=F_Fmsy),fill="yellow",alpha=0.2) +
      geom_polygon(data=data.table(P_Pmsy=c(0,1,1,0,0),F_Fmsy=c(1,1,5,5,1)),aes(x=P_Pmsy,y=F_Fmsy),fill="red",alpha=0.2) +
      geom_polygon(data=data.table(P_Pmsy=c(1,5,5,1,1),F_Fmsy=c(1,1,5,5,1)),aes(x=P_Pmsy,y=F_Fmsy),fill="orange",alpha=0.2) +
      geom_polygon(data=data.table(P_Pmsy=c(1,5,5,1,1),F_Fmsy=c(0,0,1,1,0)),aes(x=P_Pmsy,y=F_Fmsy),fill="green",alpha=0.2) +
      geom_hline(yintercept=0,color="black") +
		  geom_vline(xintercept=0,color="black") +
      geom_hline(yintercept=1,linewidth=1.15,color="black") +
		  geom_vline(xintercept=1,linewidth=1.15,color="black") +
      geom_path(aes(x=P_Pmsy,y=F_Fmsy,linetype=type,color=run_label),linewidth=1.25)
     if(input$kbmj.uncertainty=="TRUE"){
      p = p + geom_polygon(data=contour_dt,aes(x=P_Pmsy,y=F_Fmsy,color=run_label,fill=run_label,group=plot_id,linetype=type),alpha=0.1)
     }
      p = p +  
        geom_point(data=plot_dt[row==1],aes(x=P_Pmsy,y=F_Fmsy,color=run_label),shape=21,fill="white",size=3) +
        geom_point(data=plot_dt[row==max(plot_dt$row)],aes(x=P_Pmsy,y=F_Fmsy,fill=run_label),color="black",shape=21,size=3) +
        scale_linetype_manual("Distribution type", values=c("dotted","solid"),drop=FALSE) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


    # plots_mj
  output$plots_mj = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)
    # map parameters
      parameter_map = cbind(c("Depletion (D)","Population (P)","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","Removals","Process error","Process error (raw)","Surplus production"),
                            c("D","P","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","removals","dev","raw_epsp","surplus_production"))
      colnames(parameter_map) = c("input","grab")
      target_par = c("Depletion (D)","F_Fmsy")

    posterior_dt.list = prior_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(prior_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))
      if(input$kbmj.show%in% c("Both","Prior")){
        tmp_samples = ssp_prior_pushforward(ssp_summary=tmp_ssp_summary,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings)
                                    
        prior_dt.list[[i]] = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = tmp_samples,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$kbmj.prop))
      }
      if(input$kbmj.show%in% c("Both","Posterior")){
        posterior_dt.list[[i]] = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = fread(paste0(model_stem,selected_models[i],"hmc_samples.csv")),
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$kbmj.prop))
      }
    }

    if(input$kbmj.show%in% c("Both","Prior")){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      prior_dt = rbindlist(prior_dt.list) %>% .[name %in% grab_parameters] %>% .[,name:=factor(name,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"])] %>%
                .[,type:="Prior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    }
 
    if(input$kbmj.show%in% c("Both","Posterior")){
      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      posterior_dt = rbindlist(posterior_dt.list) %>% .[name %in% grab_parameters] %>% .[,name:=factor(name,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"])] %>%
                .[,type:="Posterior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    }   

    if(input$kbmj.show=="Both"){
      plot_dt = rbind(prior_dt,posterior_dt) %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    } else if(input$kbmj.show=="Posterior"){
      plot_dt = posterior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    } else {
      plot_dt = prior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])
    }

    if(input$kbmj.combine=="TRUE"){
      plot_dt = plot_dt %>%
                .[type=="Posterior",run_label:="Combined posterior"] %>%
                .[type=="Prior",run_label:="Combined prior"] %>%
                .[,run_label:=factor(run_label,levels=c("Combined prior","Combined posterior"))]
    } 

    if(nrow(plot_dt) == 0){
      return()
    }

    if(input$kbmj.uncertainty=="TRUE"){
      contour_points_dt = plot_dt[row==max(plot_dt$row)] %>%
                   .[,.(type,run_label,name,iter,value)] %>%
                   dcast(.,type+run_label+iter~name) %>%
                   .[,group_id:=paste0(type,"-",run_label)]
      unique_id = unique(contour_points_dt$group_id)
      contour_dt.list = as.list(rep(NA,length(unique_id)))
      for(i in 1:length(unique_id)){
        tmp_contour = contour_points_dt[group_id == unique_id[i]]

        mv.kde = MASS::kde2d(tmp_contour$D, tmp_contour$F_Fmsy,n = as.numeric(input$kbmj.resolution),lims=c(range(tmp_contour$D)*c(0.75,1.25),range(tmp_contour$F_Fmsy)*c(0.75,1.25)))
        dx = diff(mv.kde$x[1:2])  # lifted from emdbook::HPDregionplot()
        dy = diff(mv.kde$y[1:2])
        sz = sort(mv.kde$z)
        c1 = cumsum(sz) * dx * dy
        dimnames(mv.kde$z) = list(mv.kde$x,mv.kde$y)
        dc = reshape2::melt(mv.kde$z)
        dc$prob = approx(sz,1-c1,dc$value)$y
        dc = as.data.table(dc[,c("Var1","Var2","prob")]) %>%
            dcast(.,Var1~Var2)
        kd = MASS::kde2d(tmp_contour$D, tmp_contour$F_Fmsy,n = as.numeric(input$kbmj.resolution))
        kd$x = dc$Var1
        kd$y = as.numeric(colnames(dc)[-1])
        kd$z = as.matrix(dc)[,-1]
        cntr = contourLines(kd,levels=as.numeric(input$kbmj.quants)/100)
        cntr_ln = length(cntr)
        if(length(cntr)>1){
          cntr_dt.list = as.list(rep(NA,cntr_ln))
          for(j in 1:cntr_ln){
            cntr_dt.list[[j]] = as.data.table(cntr[[j]]) %>%
            .[,cntr_id:=j] %>%
            .[,.(cntr_id,x,y)] %>%
            setnames(.,c("x","y"),c("D","F_Fmsy")) %>%
            .[,group_id:=unique(tmp_contour$group_id)]
            cntr_dt.list[[j]] = rbind(cntr_dt.list[[j]],cntr_dt.list[[j]][1])
          }
          cntr_dt = rbindlist(cntr_dt.list)
        } else {
          cntr_dt = as.data.table(cntr[[1]]) %>%
            .[,cntr_id:=1] %>%
            .[,.(cntr_id,x,y)] %>%
            setnames(.,c("x","y"),c("D","F_Fmsy")) %>%
            .[,group_id:=unique(tmp_contour$group_id)]
            cntr_dt = rbind(cntr_dt,cntr_dt[1])
        }


        contour_dt.list[[i]] = merge(unique(tmp_contour[,.(type,run_label,group_id)]),cntr_dt,by="group_id")
      }
      contour_dt = rbindlist(contour_dt.list) %>%
                   .[,plot_id:=paste0(group_id,"-",cntr_id)] %>%
                   .[,type:=sapply(group_id,function(x)strsplit(x,"-")[[1]][1])] %>%
                   .[,run_label:=sapply(group_id,function(x)strsplit(x,"-")[[1]][2])] %>%
                   .[,type:=factor(type,levels=c("Prior","Posterior"))]
      if(input$kbmj.combine=="TRUE"){
        contour_dt = contour_dt %>%
                    .[,run_label:=factor(run_label,levels=c("Combined prior","Combined posterior"))]
      }
    }

    plot_dt = plot_dt %>%
         .[!is.na(value)] %>%
        .[,.(med=median(value)),by=.(run_label,type,name,row)] %>%
        dcast(.,run_label+type+row~name)
    
    p = plot_dt %>% ggplot() +
			xlab(expression(P/P[0])) +
		  ylab(expression(F/F["MSY"])) +
      coord_fixed(ylim=c(0,2.25),xlim=c(0,1.25)) +
      scale_x_continuous(expand = expansion(mult=c(0,0.05))) +
      scale_y_continuous(expand = expansion(mult=c(0,0.05))) +
      geom_polygon(data=data.table(D=c(0,0.2,0.2,0,0),F_Fmsy=c(0,0,5,5,0)),aes(x=D,y=F_Fmsy),fill="red",alpha=0.2) +
      geom_polygon(data=data.table(D=c(0.2,5,5,0.2,0.2),F_Fmsy=c(1,1,5,5,1)),aes(x=D,y=F_Fmsy),fill="orange",alpha=0.2) +
      geom_hline(yintercept=0,color="black") +
		  geom_vline(xintercept=0,color="black") +
      geom_hline(yintercept=1,linewidth=1.15,color="black",linetype="dashed") +
		  geom_vline(xintercept=0.2,linewidth=1.15,color="black") +
      geom_path(aes(x=D,y=F_Fmsy,linetype=type,color=run_label),linewidth=1.25)
     if(input$kbmj.uncertainty=="TRUE"){
      p = p + geom_polygon(data=contour_dt,aes(x=D,y=F_Fmsy,color=run_label,fill=run_label,group=plot_id,linetype=type),alpha=0.1)
     }
      p = p +  
        geom_point(data=plot_dt[row==1],aes(x=D,y=F_Fmsy,color=run_label),shape=21,fill="white",size=3) +
        geom_point(data=plot_dt[row==max(plot_dt$row)],aes(x=D,y=F_Fmsy,fill=run_label),color="black",shape=21,size=3) +
        scale_linetype_manual("Distribution type", values=c("dotted","solid"),drop=FALSE) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


    # plots_forecasts
  output$plots_fcast = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1|(length(input$forecasts.var)<1)){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)
    # map parameters
      parameter_map = cbind(c("Depletion (D)","Population (P)","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","Removals","Process error","Process error (raw)","Surplus production"),
                            c("D","P","U","F","D_Dmsy","P_Pmsy","U_Umsy","F_Fmsy","removals","dev","raw_epsp","surplus_production"))
      colnames(parameter_map) = c("input","grab")
      target_par = input$forecasts.var

    posterior_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(posterior_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))

      # run forecasts
      tmp_forecast = ssp_forecast(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = fread(paste0(model_stem,selected_models[i],"hmc_samples.csv")),
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=as.numeric(input$forecasts.prop),
                                                  forecast_years=as.numeric(input$forecasts.nyears),
                                                  forecast_type=input$forecasts.type,
                                                  avg_years=as.numeric(input$forecasts.avg_year),
                                                  scalar=as.numeric(input$forecasts.scalar),
                                                  resample_raw_epsp=input$forecasts.resample_epsp)


      posterior_dt.list[[i]] = ssp_derived_quants_ts(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = tmp_forecast,
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  sub_sample_prop=1)
    }
    

      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      posterior_dt = rbindlist(posterior_dt.list) %>% .[name %in% grab_parameters] %>% .[,name:=factor(name,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"],labels=parameter_map[match(target_par,parameter_map[,"input"]),"input"])] %>%
                .[,type:="Posterior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    
      plot_dt = posterior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])


    if(input$forecasts.combine=="TRUE"){
      plot_dt = plot_dt %>%
                .[type=="Posterior",run_label:="Combined posterior"]
    } 

    if(nrow(plot_dt) == 0){
      return()
    }

    plot_dt = plot_dt %>%
         .[!is.na(value)] %>%
        .[,.(med=median(value),avg=mean(value),lp=quantile(value,probs=0.5-(0.5*((as.numeric(input$forecasts.quants)-1e-1)/100))),up=quantile(value,probs=0.5+(0.5*((as.numeric(input$forecasts.quants)-1e-1)/100)))),by=.(run_label,type,name,row)] %>%
        .[row>=1,year:=year_one+(row-1)] %>%
        .[row<1,year:=year_one+(row-1)] %>%
        .[name %in% c("Process error","Process error (raw)")&row>0,year:=year+1]
    
    p = plot_dt %>% ggplot() +
			ylab("Metric") +
      xlab("Year") +
      facet_wrap(~name,scales="free_y",ncol=min(c(4,uniqueN(plot_dt$name))))
    p = p + geom_ribbon(data=plot_dt[type=="Posterior"],aes(x=year,ymin=lp,ymax=up,fill=run_label),alpha=0.4,linewidth=1.15)
    p = p + geom_path(data=plot_dt[type=="Posterior"],aes(x=year,y=med,color=run_label),linewidth=1.15)
    p = p + geom_hline(yintercept=0) + geom_vline(xintercept=max(plot_dt$year)-as.numeric(input$forecasts.nyears)+0.5) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })


    # plots_cr
    output$plots_cr = renderPlot({
    input_models = unique(filtered_id())
    if(length(input_models) < 1|(length(input$cr.var)<1)){
      return()
    }
    selected_models = sapply(sapply(input_models,function(x)strsplit(x,"-")[[1]][1]),function(x)all_dirs[grep(x,all_dirs,fixed=TRUE)])
    tmp_summary = rbindlist(lapply(paste0(model_stem,selected_models,"fit_summary.csv"),fread),fill=TRUE)
    # map parameters
      parameter_map = cbind(c("CPUE (LL)","CPUE (DFN)","Removals (LL)","Removals (DFN)","Effort (LL)","Effort (DFN)", "Catch error (LL; raw)", "Catch error (DFN; raw)","Process error (raw)","Depletion (D)", "Removals"),
                            c("catch_per_khooks","catch_per_ktan","catch_ll","catch_dfn","effort_ll","effort_dfn", "catch_error_ll", "catch_error_dfn","raw_hindcast_epsp","hindcast_x", "hindcast_removals"))
      colnames(parameter_map) = c("input","grab")
      target_par = input$cr.var

    posterior_dt.list = as.list(rep(NA,length(selected_models)))
    for(i in 1:length(posterior_dt.list)){
      tmp_ssp_summary=fread(paste0(model_stem,selected_models[i],"fit_summary.csv"))
      tmp_stan_data=fread(paste0(model_stem,selected_models[i],"stan_data.csv"))
      tmp_settings=fread(paste0(model_stem,selected_models[i],"settings.csv"))
      tmp_effort = fread(file="./input-data/effort_1945_1994.csv")


      # run catch-reconstruction
      posterior_dt.list[[i]] = ssp_catch_reconstruction(ssp_summary=tmp_ssp_summary,
                                                  samples_dt = fread(paste0(model_stem,selected_models[i],"hmc_samples.csv")),
                                                  stan_data=tmp_stan_data,
                                                  settings=tmp_settings,
                                                  effort_dt=tmp_effort,
                                                  sub_sample_prop=as.numeric(input$cr.prop),
                                                  resample_raw_epsp=input$cr.resample_epsp,
                                                  catch_error=as.numeric(input$cr.effort_error))
    }
    

      grab_parameters = parameter_map[match(target_par,parameter_map[,"input"]),"grab"]
      posterior_dt = rbindlist(posterior_dt.list) %>% .[name %in% grab_parameters] %>% .[,name:=factor(name,levels=parameter_map[match(target_par,parameter_map[,"input"]),"grab"],labels=parameter_map[match(target_par,parameter_map[,"input"]),"input"])] %>%
                .[,type:="Posterior"] %>%
                .[,type:=factor(type,levels=c("Prior","Posterior"))]
    
      plot_dt = posterior_dt %>%
                merge(.,tmp_summary[,.(run_id,run_label)])


    if(input$forecasts.combine=="TRUE"){
      plot_dt = plot_dt %>%
                .[type=="Posterior",run_label:="Combined posterior"]
    } 

    if(nrow(plot_dt) == 0){
      return()
    }

    plot_dt = plot_dt %>%
         .[!is.na(value)] %>%
        .[,.(med=median(value),avg=mean(value),lp=quantile(value,probs=0.5-(0.5*((as.numeric(input$forecasts.quants)-1e-1)/100))),up=quantile(value,probs=0.5+(0.5*((as.numeric(input$forecasts.quants)-1e-1)/100)))),by=.(run_label,type,name,row)] %>%
        .[row>=1,year:=year_one+(row-1)] %>%
        .[row<1,year:=year_one+(row-1)] 
    
    p = plot_dt %>% ggplot() +
			ylab("Metric") +
      xlab("Year") +
      facet_wrap(~name,scales="free_y",ncol=min(c(4,uniqueN(plot_dt$name))))
    p = p + geom_ribbon(data=plot_dt[type=="Posterior"],aes(x=year,ymin=lp,ymax=up,fill=run_label),alpha=0.4,linewidth=1.15)
    p = p + geom_path(data=plot_dt[type=="Posterior"],aes(x=year,y=med,color=run_label),linewidth=1.15)
    p = p + geom_hline(yintercept=0) + geom_vline(xintercept=1993.5) +
			        viridis::scale_color_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        viridis::scale_fill_viridis("Model run",begin = 0.1,end = 0.8,direction = -1,option = "H",discrete=TRUE,drop=FALSE) +
			        theme(text = element_text(size = 20),panel.background = element_rect(fill = "white", color = "black", linetype = "solid"),
							panel.grid.major = element_line(color = 'gray70',linetype = "dotted"), 
							panel.grid.minor = element_line(color = 'gray70',linetype = "dotted"),
							strip.background =element_rect(fill="white"),
							legend.key = element_rect(fill = "white"))
			
    return(p)
  },
  height=function(){
        return(height_per_panel*1.5)
  })

} # End of server
