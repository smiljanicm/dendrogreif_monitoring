get_data <- function(checkbox, variable_id, cybox, minutes = 0:59, source = "observations", start = "2013-01-01", end = Sys.Date(), toclean = 'raw') {
  if(is.null(checkbox) | is.null(variable_id)) return(NULL)
  locs <- checkbox %>%
    map_dbl(function(x){
      string_vec <- unlist(strsplit(x,'_'))
      as.numeric(string_vec[[length(string_vec)]])}) #Line needs cleaning for now location_id must be last in the string
  print("locations:")
  print(locs)
  locs <- na.omit(locs)
  vars <- variable_id %>% map_dbl(function(x) { as.numeric(x) })
  
  print(vars)
  print(class(vars))
  if(length(locs)==0) { return() }
  sql_query <- paste0("SELECT o.timestamp as timestamp, o.value, p.label, l.description, v.variable_id,
                       l.height_above_ground as height, v.description as variable, l.location_id
                         FROM ", source, " as o
                         LEFT JOIN locations as l USING(location_id)
                         LEFT JOIN plates as p USING(plate_id)
                         LEFT JOIN variables as v USING(variable_id)
                         WHERE location_id IN (", paste0(locs, collapse=','), ") 
                           AND variable_id IN ( ", paste0(variable_id, collapse=','), ")
                           AND EXTRACT(MINUTE FROM o.timestamp) IN (", paste0(minutes, collapse=','),") --change zero with minute resolutions
                           AND o.timestamp BETWEEN '", start, "'::timestamp AND '", end, "'::timestamp
                         ORDER BY o.timestamp")
  res <- DBI::dbGetQuery(con, sql_query)
  res <- res %>% mutate(location_id = case_when(is.null(label) ~ paste0(variable_id, '_', description, '_', variable, '_', height, '_', location_id),
                                                TRUE ~ paste0(variable_id, '_', label, '_', variable, '_', height, '_', location_id))) %>% select(timestamp, value, location_id)
  
  if(toclean != 'raw') {
    print(toclean)
    print(res %>% head())
    cdff <- tbl(con, 'cleaning_instructions') %>% collect()
    if(!is.null(res)) {
      print(locs)
      print(vars)
      comb <- locs %>% map(function(loc = locs, var = vars) { data.frame(loc, vars) }) %>% 
        bind_rows() %>% 
        t() %>% 
        as.data.frame()
      print("comb")
      print(comb)
      #      res_clean <- res %>% distinct(location_id) %>% unlist() %>%
      res_clean <- comb %>% map(function(com) {
        print("com:")
        print(com)
        x <- com[[1]]
        v <- com[[2]]
        print("x:")
        print(x)
        print("v:")
        print(v)
        
        t <- res %>% rename("TIMESTAMP" = timestamp) %>% 
          rename("Sensor" = value) %>%
          #          filter(grepl(paste0('_',x), location_id)) %>%
          mutate(locs = location_id) %>%  
          separate(locs, c("variable_id", "label", "variable", "height", "loc_id"), sep="_") 
        print(t)
        t <- t %>% 
          filter(loc_id == x) %>%
          filter(variable_id == v) %>%
          select(TIMESTAMP, Sensor, location_id) 
        print(t %>% head())
        t <- t %>%
          clean_sensor(locID = x, varID = v, clean_df = cdff)
        t
      }) %>% bind_rows() %>% rename("timestamp" = TIMESTAMP, "value" = Sensor) %>% arrange(timestamp)
      if(toclean == "clean") {
        res <- res_clean
      } else if(toclean == "compare") {
        res <- res_clean %>% mutate(location_id = paste0(location_id, '/1.cleaned')) %>%
          bind_rows(res %>% mutate(location_id = paste0(location_id, '/0.raw')))
      }
    }
  }
  
  if(cybox) {
    res <- res %>% rename(time = timestamp) %>%
      mutate(Years = lubridate::year(time),
             yday = lubridate::yday(time),
             dec_date = lubridate::decimal_date(time),
             time = dec_date - Years,
             label = paste0(location_id, '_', Years))
  } else {
    res <- res %>%
      rename(label = location_id,
             time = timestamp)
  }
  return(res)
}

new_plotting <- function(res, ...) {
  if(is.null(res)) return(NULL)
  res <- res %>% select(time, value, label)
  res_dp_orig <- nrow(res)
  while(nrow(res) > 500000) {
    res <- res %>% group_by(label) %>% slice_sample(prop=0.75) %>% arrange(label, time)
  }
  print(res %>% group_by(label) %>% summarize(n = n()))
  lbs <- res %>% distinct(label)
  p <- plot_ly(
    x = res$time,
    y = res$value,
    split = res$label,
    alpha = 0.8,
    type = 'scatter',
    mode = 'line') %>% 
    layout(title = paste0(lbs, "\nPlotting ", nrow(res), " data points from ", res_dp_orig, " initially selected.")) %>%
    toWebGL() 
  if(res_dp_orig > 50000) {
    p <- p %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE),
             title = paste0(lbs, "\nPlotting ", nrow(res), " data points from ", res_dp_orig, " initially selected.\n Zooming disabled (keep the number of points < 50000)"))
  }
  return(p)
}

AllSeries_reactive <- reactiveValues(res = NULL)
AllSeries_trigger <- reactive({
  list(input$AllSeriesAction, input$tabset)
})
observeEvent(input$AllSeriesDateRangePlus, {
  new_start <- input$AllSeriesDateRange[[1]] %m+% months(1)
  new_end <- input$AllSeriesDateRange[[2]] %m+% months(1)
  updateDateRangeInput(inputId = "AllSeriesDateRange", start = new_start, end = new_end)
})
observeEvent(ignoreInit=TRUE, AllSeries_trigger(), {
  if(input$tabset == 'Plot/Download') {
    withProgress(message = 'Getting data...', value=0.5, {
      s <- input$seriesDT_rows_selected
      print('These rows were selected:\n\n')
      print(s, sep = ', ')
      if (length(s)) {
        var_buff <- all_buff %>% filter(site %in% input$selectedSites)
        sensor_list_dt <- var_buff[s,] %>%
          unite('cons', everything()) %>%
          unlist()
        names(sensor_list_dt) <- NULL
        print(sensor_list_dt)
        AllSeries_reactive$res <- get_data(sensor_list_dt, 
                                           input$AllSeriesVariableCheckbox,
                                           input$compareYearsAllSeries, 
                                           source = input$AllSeriesSource,
                                           start = input$AllSeriesDateRange[[1]],
                                           end = input$AllSeriesDateRange[[2]],
                                           toclean = input$AllSeriesToClean,
                                           minutes=0:59)
      }      
    })

    if(!is.null(AllSeries_reactive$res)){
      if(nrow(AllSeries_reactive$res)>0){
        AllSeries_reactive$res <- AllSeries_reactive$res %>% separate(label, c('label', 'cleaning'), sep='/')
        unq_labels <- AllSeries_reactive$res %>% 
          distinct(label) %>% 
          unlist()
        v <- list()
        for(i in 1:length(unq_labels)) {
          print(paste0('label: ',i))
          
          plot_data <- AllSeries_reactive$res %>% filter(label == unq_labels[[i]])
          plot_data <- plot_data %>% mutate(label = paste0(label, '_', cleaning)) 
          v[[i]] <- new_plotting(plot_data) 
          
          
          # output$Plots <- renderUI ({
          #   v
          # })
          showModal(modalDialog(v,
                                downloadButton("downloadData", "Download"),
                                easyClose = TRUE))
          output$downloadData <- downloadHandler(filename = function() { paste0('DendroGreifData_', format(Sys.time(), "%Y_%M_%d_%H_%m_%S"), '.csv') },
                                                 content = function(file) {
                                                   write_csv(AllSeries_reactive$res %>% 
                                                               rename("timestamp" = time) %>%
                                                               arrange(label, timestamp) %>%
                                                               separate(label, into = c('variable_id', 'label','variable','height_above_ground', 'location_id'), sep='_'),
                                                             file)
                                                 })
        }
      }
      else {showModal(modalDialog("NoData",
                                  easyClose = TRUE))}
    } 
  }
})

PlotSeries_reactive <- reactiveValues(res = NULL)
PlotSeries_trigger <- reactive({
  list(input$Plot_series, input$tabset, input$seriestabs)
})
observeEvent(ignoreInit=TRUE, PlotSeries_trigger(), {
  if(input$tabset == 'Map') {
    if(input$seriestabs == 'Power')
      withProgress(message = 'Getting data...', value=0.5, {
        s <- input$power_status_rows_selected
        print('These rows were selected:\n\n')
        print(s, sep = ', ')
        if (length(s)) {
          print(names(input))
          print(input$power_status_rows_all)
          #         var_buff <- batt_buff %>% filter(site %in% input$selectedSites)
          var_buff <- batt_buff_showed[s,]
          
          sensor_list_dt <- var_buff %>%
            unite('cons', everything()) %>%
            unlist()
          names(sensor_list_dt) <- NULL
          print(sensor_list_dt)
          PlotSeries_reactive$res <- get_data(sensor_list_dt, 
                                              6,
                                              0, 
                                              source = "obs_120",
                                              start = Sys.Date() - 360,
                                              end = Sys.Date(),
                                              toclean = 'raw',
                                              minutes=0:59)
          print(nrow(PlotSeries_reactive$res))
          if(nrow(PlotSeries_reactive$res)>0){
            PlotSeries_reactive$res <- PlotSeries_reactive$res %>% separate(label, c('label', 'cleaning'), sep='/')
            unq_labels <- PlotSeries_reactive$res %>% 
              distinct(label) %>% 
              unlist()
            v <- list()
            #            print(sensor_list_dt)
            print(length(unq_labels))
            for(i in 1:length(unq_labels)) {
              #              print(paste0('label: ',i))
              sldt <- sensor_list_dt[[i]]
              #              print(sldt)
              sldt_cut <- paste0(sldt %>% str_sub(end = 20), '...', str_sub(sldt, start = -10))
              plot_data <- PlotSeries_reactive$res %>% filter(label == unq_labels[[i]])
              plot_data <- plot_data %>% mutate(label = paste0(label, '_', sldt_cut)) 
              v[[i]] <- new_plotting(plot_data) 
              
            }
            #            print("Plot_series")
            #            print(PlotSeries_reactive$res)
            showModal(modalDialog(v,
                                  downloadButton("downloadData", "Download"),
                                  easyClose = TRUE))
            output$downloadData <- downloadHandler(filename = function() { paste0('DendroGreifData_', format(Sys.time(), "%Y_%M_%d_%H_%m_%S"), '.csv')  },
                                                   content = function(file) {
                                                     write_csv(PlotSeries_reactive$res %>% 
                                                                 rename("timestamp" = time) %>%
                                                                 arrange(label, timestamp) %>% 
                                                                 separate(label, into = c('variable_id', 'label','variable','height_above_ground', 'location_id'), sep='_'),
                                                               file)
                                                   })
          } 
        }      
      })
  }
})

