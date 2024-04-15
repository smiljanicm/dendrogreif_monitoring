observeEvent(input$selectall_sites, {
  if(input$selectall_sites == 0) {
    return(NULL)
  } else if (input$selectall_sites%%2 == 0) {
    updateCheckboxGroupInput(session,"selectedSites",choices=sites_df$name)
  } else {
    updateCheckboxGroupInput(session,"selectedSites",choices=sites_df$name,selected=sites_df$name)
  }
})

observeEvent(input$selectall_SF, {
  if(input$selectall_SF == 0) {
    return(NULL)
  } else if (input$selectall_SF%%2 == 0) {
    updateCheckboxes(sf_buff, "SFSensorCheckbox", select = "None")
  } else {
    updateCheckboxes(sf_buff, "SFSensorCheckbox", select = "All")
  }
})

observeEvent(input$selectall_AllSeries, {
  if(input$selectall_AllSeries == 0) {
    return(NULL)
  } else if (input$selectall_AllSeries%%2 == 0) {
    updateCheckboxes(all_buff, "AllSeriesCheckbox", select = "None")
  } else {
    updateCheckboxes(all_buff, "AllSeriesCheckbox", select = "All")
  }
})

observeEvent(input$selectall_Power, {
  if(input$selectall_Power == 0) {
    return(NULL)
  } else if (input$selectall_Power%%2 == 0) {
    updateCheckboxes(power_buff, "powerCheckbox", select = "None")
  } else {
    updateCheckboxes(power_buff, "powerCheckbox", select = "All")
  }
})
