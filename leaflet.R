library(leaflet)
s <- as.data.frame(point.sp@coords)[1:25,]
data(quakes)

# Show first 20 rows from the `quakes` dataset
leaflet(data = s) %>% addTiles() %>%
  addMarkers(~x, ~y)

library(shiny)
ui <- fluidPage(
  leafletOutput("mymap"),
  p()
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(data = s) %>% addTiles() %>%
      addMarkers(~x, ~y, popup = "a") %>% 
      %>% addProviderTiles("MtbMap") %>%
      addProviderTiles("Stamen.TonerLines",
                       options = providerTileOptions(opacity = 0.35)
      ) %>%
      addProviderTiles("Stamen.TonerLabels")
  })
}

shinyApp(ui, server)
