get_data <- function(checkbox, variable_id, cybox, minutes = 0:59, source = "observations", start = "2013-01-01", end = Sys.Date(), toclean = 'raw') {
  if(is.null(checkbox) | is.null(variable_id)) return(NULL)
  locs <- checkbox %>%
    map_dbl(function(x){
      string_vec <- unlist(strsplit(x,'_'))
      as.numeric(string_vec[[length(string_vec)]])}) #Line needs cleaning for now location_id must be last in the string
  sql_query <- paste0("SELECT o.timestamp as timestamp, o.value, o.location_id
                         FROM ", source, " as o
                         WHERE location_id IN (", paste0(locs, collapse=','), ") 
                           AND variable_id IN ( ", paste0(variable_id, collapse=','), ")
                           AND EXTRACT(MINUTE FROM o.timestamp) IN (", paste0(minutes, collapse=','),") --change zero with minute resolutions
                           AND o.timestamp BETWEEN '", start, "'::timestamp AND '", end, "'::timestamp
                         ORDER BY o.timestamp")
  res <- DBI::dbGetQuery(con, sql_query)
  
  if(input$BaseDendrometersToClean != 'raw') {
    print(input$BaseDendrometersToClean)
    print(res %>% head())
    if(!is.null(res)) {
      res_clean <- res %>% distinct(location_id) %>% unlist() %>%
        map(function(x) {
          res %>% rename("TIMESTAMP" = timestamp) %>% rename("Sensor" = value) %>%
            filter(location_id == x) %>%
            clean_sensor(locID = x, clean_df = cdff)
        }) %>% bind_rows() %>% rename("timestamp" = TIMESTAMP, "value" = Sensor) %>% arrange(timestamp)
      if(input$BaseDendrometersToClean == "clean") {
        res <- res_clean
      } else if(input$BaseDendrometersToClean == "compare") {
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

baseDendro_reactive <- reactiveValues(res = NULL)
baseDendro_trigger <- reactive({
  list(input$baseDendroAction, input$tabset)
})
observeEvent(input$BaseDendrometersDateRangePlus, {
  new_start <- input$BaseDendrometersDateRange[[1]] %m+% months(1)
  new_end <- input$BaseDendrometersDateRange[[2]] %m+% months(1)
  updateDateRangeInput(inputId = "BaseDendrometersDateRange", start = new_start, end = new_end)
})

observeEvent(ignoreInit=TRUE, baseDendro_trigger(), {
  if(input$tabset == 'Dendrometers') {
    withProgress(message = 'Getting data...', value=0.5, {
      baseDendro_reactive$res <- get_data(input$baseDendrometerCheckbox, 
                                          c(1,9), 
                                          input$compareYearsBaseDendrometer, 
                                          source = input$BaseDendrometersSource,
                                          start = input$BaseDendrometersDateRange[[1]],
                                          end = input$BaseDendrometersDateRange[[2]],
                                          toclean = input$BaseDendrometersToClean)      
    })
  } 
})
output$DendroPlotly <- renderPlotly({
  new_plotting(baseDendro_reactive$res)
})

crownDendrometers_reactive <- reactiveValues(res = NULL)
crownDendrometers_trigger <- reactive({
  list(input$crownDendrometers_action, input$tabset)
  #    list(input$tabset)
})

observeEvent(ignoreInit=TRUE, crownDendrometers_trigger(), {
  if(input$tabset == 'CrownDendrometers') {
    withProgress(message = 'Getting data...', value=0.5, {
      crownDendrometers_reactive$res <- get_data(input$crownDendrometers_checkbox, 1, input$crownDendrometers_compareYears)
    })
  }
})
output$crownDendrometers_plotly <- renderPlotly({
  new_plotting(crownDendrometers_reactive$res)
})

airTemp_reactive <- reactiveValues(res = NULL)
airTemp_trigger <- reactive({
  list(input$airTemp_action, input$tabset)
})
observeEvent(ignoreInit=TRUE, airTemp_trigger(), {
  if(input$tabset == 'Air Temperature') {
    withProgress(message = 'Getting data...', value=0.5, {
      airTemp_reactive$res <- get_data(input$airTempCheckbox, 2, input$compareYearsAirTemp)
    })
  }
})
output$airTempPlotly <- renderPlotly({
  new_plotting(airTemp_reactive$res)
})

RH_reactive <- reactiveValues(res = NULL)
RH_trigger <- reactive({
  list(input$RH_action, input$tabset)
})
observeEvent(ignoreInit=TRUE, RH_trigger(), {
  if(input$tabset == 'Relative Humidity') {
    withProgress(message = 'Getting data...', value=0.5, {
      RH_reactive$res <- get_data(input$RHCheckbox, 3, input$compareYearsRH)
    })
  }
})
output$RHPlotly <- renderPlotly({
  new_plotting(RH_reactive$res)
})

soilTemp_reactive <- reactiveValues(res = NULL)
soilTemp_trigger <- reactive({
  list(input$SoilTemp_action, input$tabset)
})
observeEvent(ignoreInit=TRUE, soilTemp_trigger(), {
  if(input$tabset == 'Soil Temperature') {
    withProgress(message = 'Getting data...', value=0.5, {
      soilTemp_reactive$res <- get_data(input$soilTempCheckbox, 2, input$compareYearsSoilTemp)
    })
  }
})
output$soilTempPlotly <- renderPlotly({
  new_plotting(soilTemp_reactive$res)
})

VWC_reactive <- reactiveValues(res = NULL)
VWC_trigger <- reactive({
  list(input$VWC_action, input$tabset)
})
observeEvent(ignoreInit=TRUE, VWC_trigger(), {
  if(input$tabset == 'Volumetric Water Content') {
    withProgress(message = 'Getting data...', value=0.5, {
      VWC_reactive$res <- get_data(input$VWCCheckbox, 4, input$compareYearsVWC)
    })
  }
})
output$VWCPlotly <- renderPlotly({
  new_plotting(VWC_reactive$res)
})

bulk_reactive <- reactiveValues(res = NULL)
bulk_trigger <- reactive({
  list(input$Bulk_action, input$tabset)
})
observeEvent(ignoreInit=TRUE, bulk_trigger(), {
  if(input$tabset == 'Bulk_EC') {
    withProgress(message = 'Getting data...', value=0.5, {
      bulk_reactive$res <- get_data(input$bulkCheckbox, 7, input$compareYearsBulk)
    })
  }
})
output$bulkPlotly <- renderPlotly({
  new_plotting(bulk_reactive$res)
})

permittivity_reactive <- reactiveValues(res = NULL)
permittivity_trigger <- reactive({
  list(input$Permittivity_action, input$tabset)
})
observeEvent(ignoreInit=TRUE, permittivity_trigger(), {
  if(input$tabset == 'Permittivity') {
    withProgress(message = 'Getting data...', value=0.5, {
      permittivity_reactive$res <- get_data(input$permittivityCheckbox, 8, input$compareYearsPermittivity)
    })
  }
})
output$permittivityPlotly <- renderPlotly({
  new_plotting(permittivity_reactive$res)
})



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