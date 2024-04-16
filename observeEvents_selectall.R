observeEvent(input$selectall_sites, {
  if(input$selectall_sites == 0) {
    return(NULL)
  } else if (input$selectall_sites%%2 == 0) {
    updateCheckboxGroupInput(session,"selectedSites",choices=sites_df$name)
  } else {
    updateCheckboxGroupInput(session,"selectedSites",choices=sites_df$name,selected=sites_df$name)
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


