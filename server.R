# Define server logic required to draw a histogram

server <- function(input, output, session) {
  capture.output(file=stderr(),print(sessionInfo()))
  capture.output(file=stderr(),print(options()))
  source("map_selection.R", local = TRUE)
  
  source("observeEvents_selectall.R", local = TRUE)

  updateCheckboxes <- function(buff, checkgroup, selection = input$selectedSites, select = "None"){
    sensor_list <- buff %>%
      filter(site %in% selection) %>%
      unite('cons', everything()) %>%
      unlist()
    names(sensor_list) <- NULL
    if(select == "None") {
      updateCheckboxGroupInput(session, checkgroup,
                               label = paste("There are", length(sensor_list), "sensors in the list"),
                               choices = sensor_list)
      
    } else if (select == "All") {
      updateCheckboxGroupInput(session, checkgroup,
                               label = paste("There are", length(sensor_list), "sensors in the list"),
                               choices = sensor_list, selected = sensor_list)
    }
  }
  
  observeEvent(input$selectedSites, {
    updateCheckboxes(base_dendrometer_buff, "baseDendrometerCheckbox")
    updateCheckboxes(crownDendrometers_buff, "crownDendrometers_checkbox")
    updateCheckboxes(airTemp_buff, "airTempCheckbox")
    updateCheckboxes(RH_buff, "RHCheckbox")
    updateCheckboxes(soilTemp_buff, "soilTempCheckbox")
    updateCheckboxes(VWC_buff, "VWCCheckbox")
    updateCheckboxes(bulk_buff, "bulkCheckbox")
    updateCheckboxes(permittivity_buff, "permittivityCheckbox")
    updateCheckboxes(sf_buff, 'SFSensorCheckbox')
    updateCheckboxes(power_buff, "powerCheckbox")
  }, ignoreNULL = FALSE)
  
  source("plotting_reactive.R", local = TRUE)
  
  output$location_status <- renderDT({loc_buff}, filter = list(position = 'top', clear = FALSE))
  output$site_status <- renderDT({
    loc_buff %>%
      select(site, first_timestamp, last_timestamp, number_observations) %>%
      group_by(site) %>%
      summarize(last_timestamp = max(last_timestamp, na.rm = T),
                number_observations = sum(number_observations, na.rm = T)) %>%
      mutate(days_from_now = difftime(Sys.time(), last_timestamp, units = "days"))
  }, filter = list(position = 'top', clear = FALSE))
  output$power_status <- renderDT({
    loc_buff %>%
      filter(variable == "Battery") %>%
      select(site, loc_description, last_timestamp, most_recent_value) %>%
      mutate(days_from_now = difftime(Sys.time(), last_timestamp, units = "days"))
  }, filter = list(position = 'top', clear = FALSE))
  
}
