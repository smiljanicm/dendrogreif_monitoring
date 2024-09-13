source("deps.R")
# Define UI for application that draws a histogram
ui <- navbarPage("DendroGreifMonitoring", id="tabset",
                 tabPanel("Map", 
                          tags$script(HTML("$(function(){ 
                                              $(document).keyup(function(e) {
                                                if (e.which == 80) {
                                                  $('#Plot_series').click()
                                                }
                                              });
                          })")),
                          tags$style(
                            type = 'text/css',
                            '.modal-dialog { width: fit-content !important; }'
                          ),
                          sidebarLayout(
                            sidebarPanel(
                              actionLink("selectall_sites","Select All"),
                              checkboxGroupInput("selectedSites", 
                                                 "Choose sites:",
                                                 choices = sites_df$name,
                                                 inline=F),
                              actionButton("Plot_series", label = "Plot [p]")
                            ),
                            mainPanel = mainPanel(
                              leafletOutput("sitemap", height = '400'),
                              tabsetPanel(id='seriestabs',
                                tabPanel("Power",
                                         DT::DTOutput("power_status"),
                                         uiOutput("Plots_power")
                                ),
                                tabPanel("Site Info",
                                         DT::DTOutput("site_status")
                                ),
                                tabPanel("Raw",
                                         DT::DTOutput("location_status")
                                )
                              )
                            )
                          )
                          
                 ),
                 tabPanel("Plot/Download", 
                          tags$script(HTML("$(function(){ 
                                              $(document).keyup(function(e) {
                                                if (e.which == 80) {
                                                  $('#AllSeriesAction').click()
                                                }
                                              });
                          })")),
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("compareYearsAllSeries", 
                                            "Compare years"),
                              radioButtons("AllSeriesSource", 
                                           "Choose data resolution:",
                                           choiceNames = c("RAW", "05 minutes", "15 minutes", "30 minutes", "60 minutes", "120 minutes"),
                                           choiceValues = c("observations", "obs_05", "obs_15", "obs_30", "obs_60", "obs_120"),
                                           selected = "obs_120"),
                              radioButtons("AllSeriesToClean", 
                                           "Cleaning data:",
                                           choiceNames = c("RAW", "Cleaned", "Comparison"),
                                           choiceValues = c("raw", "clean", "compare"),
                                           selected = "raw"),
                              dateRangeInput("AllSeriesDateRange", label = "Select Date Range", start = '2013-01-01', end = Sys.Date()),
                              actionButton("AllSeriesDateRangePlus", label = "Add a month to dates"),
                              selectizeInput("AllSeriesVariableCheckbox", "Variables", 
                                             choices = map(1:nrow(all_variables), 
                                                           function(x) { 
                                                             all_variables$variable_id[[x]]
                                                             }) 
                                             %>% set_names(all_variables$description), multiple = T),
                              
                              actionButton("AllSeriesAction", "Plot [p]")
                            ),
                            mainPanel(DT::DTOutput("seriesDT")#,
                                      #uiOutput("Plots")
                            )
                          )
                 )
)
