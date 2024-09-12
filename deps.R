library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(dendwrang)
library(DBI)
library(plotly)
library(shinycssloaders)
library(DT)
library(jsonlite)
library(lubridate)

options(expressions = 500000)
options(java.parameters = "-Xss2560k")
# Connection to the database
con <- DBI::dbConnect(RPostgres::Postgres(), 
                      dbname="monitoring_raw", 
                      host='localhost',
#                      host='141.53.44.233',
#                      password = 'temp2',
                      user='rstudio_read')

sites <- tbl(con, "sites")

sites_df <- sites %>% collect()

sites_df <- sites_df %>% mutate(lat_long_nest = strsplit(gsub('^\\(|\\)', '', gps), ',')) %>%
  unnest(lat_long_nest) %>% 
  mutate(q_name = rep(c("Lat", "Long"), nrow(.)/2)) %>%
  pivot_wider(id_cols = c(site_id, name, description, gps, parent_id), names_from = q_name, values_from = lat_long_nest) %>%
  mutate(Lat = as.numeric(Lat)) %>%
  mutate(Long = as.numeric(Long))

loc_buff <-  tbl(con, "location_overview") %>% 
  collect() %>% 
  mutate(site = factor(site),
         tree_id = factor(tree_id),
         location_type = factor(location_type),
         loc_description = factor(loc_description),
         variable = factor(variable))

all_buff <- tbl(con, "site_locs_overview") %>% 
  collect() %>%
  arrange(site, species, label, height)
print(all_buff %>% head())
print(loc_buff %>% head())

batt_buff <- all_buff %>% 
  left_join(loc_buff, 
            by=join_by(description==loc_description, site)) %>%
  filter(variable == "Battery") %>%
  select(site, description, last_timestamp, most_recent_value,location_id) %>%
  separate(description, c('loc_desc', 'online', 'Battery'),  '; ') %>%
  mutate(days_from_now = difftime(Sys.time(), last_timestamp, units = "days")) %>%
  mutate(days_from_now = as.numeric(days_from_now)) %>%
  mutate(should_be_visited = case_when(online == 'Online' ~ ((most_recent_value < 12.2) | (days_from_now > 5)),
                                        TRUE ~ days_from_now > 35)) %>%
  relocate(location_id, .after=last_col())
batt_buff_showed <- batt_buff %>% arrange(desc(online), desc(should_be_visited))


print(batt_buff)
all_variables <- tbl(con, "variables") %>% 
  select(description, variable_id) %>%
  collect()

clean_sensor <- function(data, clean_df = cdff, locID = 2, varID = 1, clsetID = 1) {
  #cleaning_set_id;location_id;variable_id;c
  clean_df <- clean_df %>% 
    filter(cleaning_set_id == clsetID) %>%
    filter(location_id == locID) %>% 
    filter(variable_id == varID) %>%
    select(correction, arguments)
  out <- data
  print(clean_df)
  print(out %>% head())
  out %>% head() %>% print()
  if(nrow(clean_df) > 0) {
    for(i in 1:nrow(clean_df)){
      corr <- clean_df[[i, "correction"]]
      args <- clean_df[[i, "arguments"]]
      
      args_list <- fromJSON(args)
      args_list <- c(list(data=out), args_list)
      out <- do.call(corr, args_list)
    }
  }
  return(out)
}
