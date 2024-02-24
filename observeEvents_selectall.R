observeEvent(input$selectall_sites, {
  if(input$selectall_sites == 0) {
    return(NULL)
  } else if (input$selectall_sites%%2 == 0) {
    updateCheckboxGroupInput(session,"selectedSites",choices=sites_df$name)
  } else {
    updateCheckboxGroupInput(session,"selectedSites",choices=sites_df$name,selected=sites_df$name)
  }
})

observeEvent(input$selectall_BaseDendrometers, {
  if(input$selectall_BaseDendrometers == 0) {
    return(NULL)
  } else if (input$selectall_BaseDendrometers%%2 == 0) {
    updateCheckboxes(base_dendrometer_buff, "baseDendrometerCheckbox", select = "None")  
  } else {
    updateCheckboxes(base_dendrometer_buff, "baseDendrometerCheckbox", select = "All")
  }
})

observeEvent(input$selectall_CrownDendrometers, {
  if(input$selectall_CrownDendrometers == 0) {
    return(NULL)
  } else if (input$selectall_CrownDendrometers%%2 == 0) {
    updateCheckboxes(crownDendrometers_buff, "crownDendrometers_checkbox", select = "None")
  } else {
    updateCheckboxes(crownDendrometers_buff, "crownDendrometers_checkbox", select = "All")
  }
})

observeEvent(input$selectall_AirTemp, {
  if(input$selectall_AirTemp == 0) {
    return(NULL)
  } else if (input$selectall_AirTemp%%2 == 0) {
    updateCheckboxes(airTemp_buff, "airTempCheckbox", select = "None")
  } else {
    updateCheckboxes(airTemp_buff, "airTempCheckbox", select = "All")
  }
})

observeEvent(input$selectall_RH, {
  if(input$selectall_RH == 0) {
    return(NULL)
  } else if (input$selectall_RH%%2 == 0) {
    updateCheckboxes(RH_buff, "RHCheckbox", select = "None")
  } else {
    updateCheckboxes(RH_buff, "RHCheckbox", select = "All")
  }
})

observeEvent(input$selectall_SoilTemp, {
  if(input$selectall_SoilTemp == 0) {
    return(NULL)
  } else if (input$selectall_SoilTemp%%2 == 0) {
    updateCheckboxes(soilTemp_buff, "soilTempCheckbox", select = "None")
  } else {
    updateCheckboxes(soilTemp_buff, "soilTempCheckbox", select = "All")
  }
})

observeEvent(input$selectall_VWC, {
  if(input$selectall_VWC == 0) {
    return(NULL)
  } else if (input$selectall_VWC%%2 == 0) {
    updateCheckboxes(VWC_buff, "VWCCheckbox", select = "None")
  } else {
    updateCheckboxes(VWC_buff, "VWCCheckbox", select = "All")
  }
})

observeEvent(input$selectall_Bulk, {
  if(input$selectall_Bulk == 0) {
    return(NULL)
  } else if (input$selectall_Bulk%%2 == 0) {
    updateCheckboxes(bulk_buff, "bulkCheckbox", select = "None")
  } else {
    updateCheckboxes(bulk_buff, "bulkCheckbox", select = "All")
  }
})

observeEvent(input$selectall_Permittivity, {
  if(input$selectall_Permittivity == 0) {
    return(NULL)
  } else if (input$selectall_Permittivity%%2 == 0) {
    updateCheckboxes(permittivity_buff, "permittivityCheckbox", select = "None")
  } else {
    updateCheckboxes(permittivity_buff, "permittivityCheckbox", select = "All")
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

observeEvent(input$selectall_Power, {
  if(input$selectall_Power == 0) {
    return(NULL)
  } else if (input$selectall_Power%%2 == 0) {
    updateCheckboxes(power_buff, "powerCheckbox", select = "None")
  } else {
    updateCheckboxes(power_buff, "powerCheckbox", select = "All")
  }
})
