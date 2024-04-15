get_data <- function(checkbox, variable_id, cybox, minutes = 0:59, source = "observations", start = "2013-01-01", end = Sys.Date(), toclean = 'raw') {
  if(is.null(checkbox) | is.null(variable_id)) return(NULL)
  locs <- checkbox %>%
    map_dbl(function(x){
      string_vec <- unlist(strsplit(x,'_'))
      as.numeric(string_vec[[length(string_vec)]])}) #Line needs cleaning for now location_id must be last in the string
  sql_query <- paste0("SELECT o.timestamp as timestamp, o.value, p.label, l.description, l.height_above_ground as height, v.description as variable, l.location_id
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
  res <- res %>% mutate(location_id = case_when(is.null(label) ~ paste0(description, '_', variable, '_', height, '_', location_id),
                                                TRUE ~ paste0(label, '_', variable, '_', height, '_', location_id))) %>% select(timestamp, value, location_id)
  
  if(toclean != 'raw') {
#    print(toclean)
#    print(res %>% head())
    if(!is.null(res)) {
      print(locs)
#      res_clean <- res %>% distinct(location_id) %>% unlist() %>%
      res_clean <- locs %>% unlist() %>% map(function(x) {
#          print(x)
          t <- res %>% rename("TIMESTAMP" = timestamp) %>% rename("Sensor" = value) %>%
            filter(grepl(paste0('_',x), location_id)) %>%
            clean_sensor(locID = x, clean_df = cdff)
          t
        }) %>% bind_rows() %>% rename("timestamp" = TIMESTAMP, "value" = Sensor) %>% arrange(timestamp)
      if(toclean == "clean") {
        res <- res_clean
      } else if(toclean == "compare") {
        res <- res_clean %>% mutate(location_id = paste0(location_id, '_1.cleaned')) %>%
          bind_rows(res %>% mutate(location_id = paste0(location_id, '_0.raw')))
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
  p <- plot_ly(
    x = res$time,
    y = res$value,
    split = res$label,
    alpha = 0.8,
    type = 'scatter',
    mode = 'line') %>% 
    layout(title = paste0("Plotting ", nrow(res), " data points from ", res_dp_orig, " initially selected.")) %>%
    toWebGL() 
  if(res_dp_orig > 50000) {
    p <- p %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE),
             title = paste0("Plotting ", nrow(res), " data points from ", res_dp_orig, " initially selected.\n Zooming disabled (keep the number of points < 50000)"))
  }
  return(p)
}

SF_reactive <- reactiveValues(res = NULL)
SF_trigger <- reactive({
  list(input$SF_action, input$tabset)
})
observeEvent(ignoreInit=TRUE, SF_trigger(), {
  if(input$tabset == 'SapFlow') {
    withProgress(message = 'Getting data...', value=0.5, {
      SF_reactive$res <- get_data(input$SFSensorCheckbox, input$SFVariableCheckbox, input$compareYearsSF, minutes = 0:59)
    })
  }
})
output$SFPlotly <- renderPlotly({
  new_plotting(SF_reactive$res)
})
# output$SFPlotly <- renderPlotly({
#   print(input$SFVariableCheckbox)
#   print(paste(input$SFVariableCheckbox, collapse = ', '))
#   plotting(input$SFSensorCheckbox, input$SFVariableCheckbox, input$compareYearsSF, minutes=0:59)
# })




power_reactive <- reactiveValues(res = NULL)
power_trigger <- reactive({
  list(input$Power_action, input$tabset)
})
observeEvent(ignoreInit=TRUE, power_trigger(), {
  if(input$tabset == 'Power Supply') {
    withProgress(message = 'Getting data...', value=0.5, {
      power_reactive$res <- get_data(input$powerCheckbox, 6, input$compareYearsPower)
    })
  }
})
output$powerPlotly <- renderPlotly({
  new_plotting(power_reactive$res)
})

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
  if(input$tabset == 'All Series') {
    withProgress(message = 'Getting data...', value=0.5, {
      s <- input$seriesDT_rows_selected
      if (length(s)) {
        print('These rows were selected:\n\n')
        print(s, sep = ', ')
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
  
  } 
})
output$AllSeriesPlotly <- renderPlotly({
  new_plotting(AllSeries_reactive$res)
})