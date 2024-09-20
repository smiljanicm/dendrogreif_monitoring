output$sitemap <- renderLeaflet({
  sites_df %>%
    leaflet() %>%
    addProviderTiles('Esri.WorldTopoMap', group = 'Terrain') %>%
    addProviderTiles('Esri.WorldImagery', group = 'Satellite') %>%
    addLayersControl(baseGroups = c('Terrain', 'Satellite')) %>%
    addCircleMarkers(layerId = ~name, label=~name, lng=~Long, lat=~Lat,
                     popup = ~name,
                     clusterOptions = markerClusterOptions(maxClusterRadius = 25)) %>%
    setView(12.6, 53.8, zoom = 8)
})
output$sites = renderDT(sites_df)

observeEvent(input$sitemap_marker_click, {
  click <- input$sitemap_marker_click
  if(click$id %in% input$selectedSites) {
    selected = input$selectedSites[input$selectedSites != click$id]
  } else {
    selected = c(input$selectedSites, click$id)
  }
  updateSelectizeInput(session, "selectedSites", 
                    selected = selected)
})