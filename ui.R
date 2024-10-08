source("deps.R")
# Define UI for application that draws a histogram
ui <- navbarPage("DendroGreifMonitoring", id="tabset", collapsible = TRUE,
                 tabPanel("Diagnostics", 
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
#                          sidebarLayout(
#                                div(class = "col-lg-4",
                                    # actionLink("selectall_sites","Select All"),
                                    # checkboxGroupInput("selectedSites", 
                                    #                    "Choose sites:",
                                    #                    choices = sites_df$name,
                                    #                    inline=F)#,
                                    #actionButton("Plot_series", label = "Plot [p]")
 #                               ),
#                                div(class = "col-lg-8",
                                    tabsetPanel(id='seriestabs',
                                                tabPanel("Power",
                                                         DT::DTOutput("power_status"),
                                                         uiOutput("Plots_power")
                                                ),
                                                tabPanel("Dendrometers",
                                                         DT::DTOutput("dendrometer_status")
                                                ),
                                                tabPanel("Site Info",
                                                         DT::DTOutput("site_status")
                                                ),
 #                                   ),       
                                    fixedPanel(
                                      actionButton("Plot_series", label = "Plot [p]"),
                                      right = 10,
                                      bottom = 10
                                    )
 #                               )
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
                            div(class = "col-lg-4",
                                actionLink("selectall_sites","Select All"),
                                selectizeInput("selectedSites", 
                                               "Choose sites:",
                                               choices = sites_df$name,
                                               multiple = T),
                              #                     inline=F),
                                #actionButton("Plot_series", label = "Plot [p]")
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
                                             selected = "clean"),
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
                            div(class = "col-lg-8",
                                leafletOutput("sitemap", height = '400'),
                                DT::DTOutput("seriesDT"),
                                fixedPanel(
                                  actionButton("test", label = "test [p]"),
                                  #  left = 0,
                                  #  width= '100%',
                                  right = 10,
                                  bottom = 0
                                )#,
                                #uiOutput("Plots")
                            )
                          )
                 )
)
