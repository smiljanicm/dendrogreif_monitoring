# Define server logic required to draw a histogram

server <- function(input, output, session) {
  capture.output(file=stderr(),print(sessionInfo()))
  capture.output(file=stderr(),print(options()))
  source("map_selection.R", local = TRUE)
  
  source("observeEvents_selectall.R", local = TRUE)

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
