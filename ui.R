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
                 tabPanel("Dendrometers", 
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("compareYearsBaseDendrometer", 
                                            "Compare years"),
                              radioButtons("BaseDendrometersSource", 
                                           "Choose data resolution:",
                                           choiceNames = c("RAW", "05 minutes", "15 minutes", "30 minutes", "60 minutes", "120 minutes"),
                                           choiceValues = c("observations", "obs_05", "obs_15", "obs_30", "obs_60", "obs_120"),
                                           selected = "obs_120"),
                              radioButtons("BaseDendrometersToClean", 
                                           "Cleaning data:",
                                           choiceNames = c("RAW", "Cleaned", "Comparison"),
                                           choiceValues = c("raw", "clean", "compare"),
                                           selected = "raw"),
                              dateRangeInput("BaseDendrometersDateRange", label = "Select Date Range", start = '2013-01-01', end = Sys.Date()),
                              actionButton("BaseDendrometersDateRangePlus", label = "Add a month to dates"),
                              actionLink("selectall_BaseDendrometers","Select All"),
                              actionButton("baseDendroAction", "Update"),
                              checkboxGroupInput("baseDendrometerCheckbox", 
                                                 "Choose dendrometers:",
                                                 choices = character(0))
                            ),
                            mainPanel(plotlyOutput("DendroPlotly") %>% 
                                        withSpinner(type=3, 
                                                    color.background = "white", 
                                                    hide.ui = FALSE)
                                      )
                          )
                 ),
                 tabPanel("CrownDendrometers",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("crownDendrometers_compareYears", 
                                            "Compare years"),
                              actionLink("selectall_CrownDendrometers","Select All"),
                              actionButton("crownDendrometers_action", "Update"),
                              checkboxGroupInput("crownDendrometers_checkbox", 
                                                 "Choose dendrometers:",
                                                 choices = character(0))
                            ),
                            mainPanel(plotlyOutput("crownDendrometers_plotly") %>% 
                                        withSpinner(type=3, 
                                                    color.background = "white", 
                                                    hide.ui = FALSE))
                          )
                 ),
                 tabPanel("Air Temperature",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("compareYearsAirTemp", 
                                            "Compare years"),
                              actionLink("selectall_AirTemp","Select All"),
                              actionButton("airTemp_action", "Update"),
                              checkboxGroupInput("airTempCheckbox", 
                                                 "Choose sensors:",
                                                 choices = character(0))
                            ),
                            mainPanel(plotlyOutput("airTempPlotly") %>% 
                                        withSpinner(type=3, 
                                                    color.background = "white", 
                                                    hide.ui = FALSE))
                          )
                          
                 ),
                 tabPanel("Relative Humidity",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("compareYearsRH", "Compare years"),
                              actionLink("selectall_RH","Select All"),
                              actionButton("RH_action", "Update plot"),
                              checkboxGroupInput("RHCheckbox", 
                                                 "Choose sensors:",
                                                 choices = character(0))
                            ),
                            mainPanel(plotlyOutput("RHPlotly") %>% 
                                        withSpinner(type=3, 
                                                    color.background = "white", 
                                                    hide.ui = FALSE))
                          )
                          
                 ),
                 navbarMenu("Soil",
                            tabPanel("Soil Temperature",
                                     sidebarLayout(
                                       sidebarPanel(
                                         checkboxInput("compareYearsSoilTemp", 
                                                       "Compare years"),
                                         actionLink("selectall_SoilTemp","Select All"),
                                         actionButton("SoilTemp_action", "Update plot"),
                                         checkboxGroupInput("soilTempCheckbox", 
                                                            "Choose sensors:",
                                                            choices = character(0))
                                       ),
                                       mainPanel(plotlyOutput("soilTempPlotly") %>% 
                                                   withSpinner(type=3, 
                                                               color.background = "white", 
                                                               hide.ui = FALSE))
                                     )
                                     
                            ),
                            tabPanel("Volumetric Water Content",
                                     sidebarLayout(
                                       sidebarPanel(
                                         checkboxInput("compareYearsVWC", 
                                                       "Compare years"),
                                         actionLink("selectall_VWC","Select All"),
                                         actionButton("VWC_action", "Update plot"),
                                         checkboxGroupInput("VWCCheckbox", 
                                                            "Choose sensors:",
                                                            choices = character(0))
                                       ),
                                       mainPanel(plotlyOutput("VWCPlotly") %>% 
                                                   withSpinner(type=3, 
                                                               color.background = "white", 
                                                               hide.ui = FALSE))
                                     )
                                     
                            ),
                            tabPanel("Bulk_EC",
                                     sidebarLayout(
                                       sidebarPanel(
                                         checkboxInput("compareYearsBulk", 
                                                       "Compare years"),
                                         actionLink("selectall_Bulk","Select All"),
                                         actionButton("Bulk_action", "Update plot"),
                                         checkboxGroupInput("bulkCheckbox", 
                                                            "Choose sensors:",
                                                            choices = character(0))
                                       ),
                                       mainPanel(plotlyOutput("bulkPlotly") %>% 
                                                   withSpinner(type=3, 
                                                               color.background = "white", 
                                                               hide.ui = FALSE))
                                     )
                                     
                            ),
                            tabPanel("Permittivity",
                                     sidebarLayout(
                                       sidebarPanel(
                                         checkboxInput("compareYearsPermittivity", 
                                                       "Compare years"),
                                         actionLink("selectall_Permittivity","Select All"),
                                         actionButton("Permittivity_action", "Update plot"),
                                         checkboxGroupInput("permittivityCheckbox", 
                                                            "Choose sensors:",
                                                            choices = character(0))
                                       ),
                                       mainPanel(plotlyOutput("permittivityPlotly") %>% 
                                                   withSpinner(type=3, 
                                                               color.background = "white", 
                                                               hide.ui = FALSE))
                                     )
                            )),
                 tabPanel("SapFlow",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("compareYearsSF", "Compare years"),
                              actionLink("selectall_SF","Select All"),
                              actionButton("SF_action", "Update plot"),
                              checkboxGroupInput("SFSensorCheckbox", 
                                                 "Choose sensors:",
                                                 choices = character(0)),
                              checkboxGroupInput("SFVariableCheckbox", 
                                                 "Choose variables:",
                                                 choiceNames = sf_variables$description,
                                                 choiceValues = sf_variables$variable_id),
                              radioButtons("radioSF", "What kind of facetting",c("None" = 'none', 'Sensors' = 'sensors', 'Variables' = 'variables'))
                            ),
                            mainPanel(plotlyOutput("SFPlotly") %>% 
                                        withSpinner(type=3, 
                                                    color.background = "white", 
                                                    hide.ui = FALSE))
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

