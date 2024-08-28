observeEvent(input$selectall_sites, {
  if(input$selectall_sites == 0) {
    return(NULL)
  } else if (input$selectall_sites%%2 == 0) {
    updateCheckboxGroupInput(session,"selectedSites",choices=sites_df$name)
  } else {
    updateCheckboxGroupInput(session,"selectedSites",choices=sites_df$name,selected=sites_df$name)
  }
})

observeEvent(input$selectedSites, {
  output$seriesDT = NULL
  output$seriesDT = renderDT({all_buff %>% filter(site %in% input$selectedSites)}, filter = list(position = 'top', clear = FALSE))
  seriesDT_proxy <- DT::dataTableProxy("seriesDT")
  DT::selectRows(seriesDT_proxy, NULL)
}, ignoreNULL = FALSE)
