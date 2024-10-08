# Define server logic required to draw a histogram

server <- function(input, output, session) {
  capture.output(file=stderr(),print(sessionInfo()))
  capture.output(file=stderr(),print(options()))
  source("map_selection.R", local = TRUE)
  
  source("observeEvents_selectall.R", local = TRUE)

  source("plotting_reactive.R", local = TRUE)
  
  #we need some kind of info about standard deviations-here is the sql queries as a start
  # --select * from obs_120 limit 10
  #
  # --calculate sd over last three months
  # --select location_id, variable_id, stddev(value) as sd from obs_120 where timestamp > (now() - interval '3 months')::date group by location_id, variable_id order by sd desc
  # 
  # -- get the residuals of the moving average over last 3 months -- probably needs a window function
  # select * from observations where timestamp > (now() - interval '3 months')::date
  
  output$dendrometer_status <- renderDT({datatable(dendrometer_status,
                                                filter = list(position='top', clear = F),
                                               # extension = c('Responsive'),
                                                options = list(pageLength = 50)) %>% #, responsive = T)) %>% 
      formatStyle('condition', target = 'row', 
                  backgroundColor = styleEqual(c('ok', 'broken', 'suspect', 'needs_reset', 'high_sd (growing?)', 'no_recent_values'), 
                                               c('green', 'red','yellow', 'orange', 'lightblue', 'pink')))
  })
  output$site_status <- renderDT({
    loc_buff %>%
      select(site, first_timestamp, last_timestamp, number_observations) %>%
      group_by(site) %>%
      summarize(last_timestamp = max(last_timestamp, na.rm = T),
                number_observations = sum(number_observations, na.rm = T)) %>%
      mutate(days_from_now = difftime(Sys.time(), last_timestamp, units = "days"))
  }, filter = list(position = 'top', clear = FALSE))
  output$power_status <- renderDT({
    datatable(batt_buff_showed, 
    		filter = list(position = 'top', clear = FALSE),
    		extensions = c('Responsive'),
    		options = list(pageLength = 50, responsive = TRUE)) %>% 
      formatStyle('most_recent_value', backgroundColor = styleInterval(c(11.95, 12.2), c('red', 'yellow', 'green'))) %>% 
      formatStyle('days_from_now', backgroundColor = styleInterval(c(1, 5), c('green', 'yellow', 'red'))) %>%
      formatStyle('should_be_visited', backgroundColor = styleEqual(c(F, T), c('green', 'red')))
  })
}
