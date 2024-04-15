source("deps.R")
# Define UI for application that draws a histogram
ui <- navbarPage("DendroGreifMonitoring", id="tabset",
                 tabPanel("Map", 
                          sidebarLayout(
                            sidebarPanel(
                              actionLink("selectall_sites","Select All"),
                              checkboxGroupInput("selectedSites", 
                                                 "Choose sites:",
                                                 choices = sites_df$name,
                                                 inline=F)
                            ),
                            mainPanel = mainPanel(
                              leafletOutput("sitemap", height = '400'),
                              DT::DTOutput("sites")
                            )
                          )
                          
                 ),
                 tabPanel("Power Supply",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("compareYearsPower", 
                                            "Compare years"),
                              actionLink("selectall_Power","Select All"),
                              actionButton("Power_action", "Update plot"),
                              checkboxGroupInput("powerCheckbox", 
                                                 "Choose sensors:",
                                                 choices = character(0))
                            ),
                            mainPanel(plotlyOutput("powerPlotly") %>% 
                                        withSpinner(type=3, 
                                                    color.background = "white", 
                                                    hide.ui = FALSE))
                          )
                          
                 ),
                 tabPanel("Status",
                          tabsetPanel(
                            tabPanel("Power",
                                     DT::DTOutput("power_status")
                            ),
                            tabPanel("Site Info",
                                     DT::DTOutput("site_status")
                            ),
                            tabPanel("Raw",
                                     DT::DTOutput("location_status")
                            )
                          )
                 ),
                 tabPanel("All Series", 
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
                              actionButton("AllSeriesAction", "Update"),
                              checkboxGroupInput("AllSeriesVariableCheckbox", 
                                                 "Choose variables:",
                                                 choiceNames = all_variables$description,
                                                 choiceValues = all_variables$variable_id),
                              actionLink("selectall_AllSeries","Select All"),
                              checkboxGroupInput("AllSeriesCheckbox", 
                                                 "Choose dendrometers:",
                                                 choices = character(0))
                            ),
                            mainPanel(DT::DTOutput("seriesDT"),
                                      plotlyOutput("AllSeriesPlotly") %>% 
                                        withSpinner(type=3, 
                                                    color.background = "white", 
                                                    hide.ui = FALSE)
                            )
                          )
                 ),
)

