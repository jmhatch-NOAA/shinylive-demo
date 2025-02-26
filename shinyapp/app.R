## install libraries
library(shiny)
library(bslib)
library(leaflet)
library(jsonlite)
library(sf)
sf_use_s2(FALSE)
library(plotly)

## ui
ui = page_sidebar(

  ## app title
  title = 'Hourly Weather Forecasts',

  ## sidebar
  sidebar = sidebar(

    ## display selected coordinates
    p("Selected longitude:"),
    textOutput(outputId ="lon"),
    p("Selected latitude:"),
    textOutput(outputId ="lat"),

    ## action button
    actionButton(inputId = 'query', label = 'Run')

  ),

  ## output plots
  navset_card_underline(
    nav_panel("Map", leafletOutput("map")),
    nav_panel("Forecasts",
              plotlyOutput("atmpPlot"),
              plotlyOutput("precipPlot"),
              plotlyOutput("dewPlot")
      )
  )

)

## server
server = function(input, output, session) {

  ## store longitude and latitude
  coordinates <- reactiveValues(latitude = NULL, longitude = NULL, name = NULL, hourly_data = NULL)

  ## leaflet map output
  output$map = renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = -70.9277, lat = 41.6341, zoom = 10)
  })

  ## capture map click
  observeEvent(input$map_click, {
    click = input$map_click
    if (!is.null(click)) {
      coordinates$latitude = click$lat
      coordinates$longitude = click$lng
      output$lon = renderText({ coordinates$longitude })
      output$lat = renderText({ coordinates$latitude })
    }
  })

  ## query data
  observeEvent(input$query, {

    ## data url
    data_url = paste0('https://api.weather.gov/points/', coordinates$latitude, ',', coordinates$longitude)

    ## data
    nws_data = fromJSON(data_url)
    coordinates$name = paste0(nws_data$properties$relativeLocation$properties$city, ', ', nws_data$properties$relativeLocation$properties$state)

    ## hourly forecast
    hourly_data = fromJSON(nws_data$properties$forecastHourly)
    coordinates$hourly_data = hourly_data$properties$periods

    ## bounding box
    bb = data.frame(x = hourly_data$geometry$coordinates[,,1], y = hourly_data$geometry$coordinates[,,2]) |>
      sf::st_as_sf(coords = c('x', 'y'), crs = 4326) |>
      summarise(geometry = st_combine(geometry)) |>
      sf::st_cast('POLYGON')

    ## bb coordinates
    bb_coords = bb |>
      st_buffer(dist = 0.001) |>
      st_coordinates()

    ## modify map
    leafletProxy("map") |>
      addPolygons(data = bb) |>
      fitBounds(lng1 = min(bb_coords[, 'X']), lat1 = min(bb_coords[, 'Y']), lng2 = max(bb_coords[, 'X']), lat2 = max(bb_coords[, 'Y']))

  })

  ## display plots
  output$atmpPlot = renderPlotly({

    ## plotly
    plot_ly(coordinates$hourly_data, type = 'scatter', mode = 'lines') |>
      add_trace(x = ~startTime, y = ~temperature) |>
      layout(showlegend = FALSE, title = coordinates$name, xaxis = list(title = ''), yaxis = list(title = 'Air Temperature (degrees F)'))

  })
  output$precipPlot = renderPlotly({

    ## plotly
    plot_ly(coordinates$hourly_data, type = 'scatter', mode = 'lines') |>
      add_trace(x = ~startTime, y = ~temperature) |>
      layout(showlegend = FALSE, title = coordinates$name, xaxis = list(title = ''), yaxis = list(title = 'Air Temperature (degrees F)'))

  })
  output$dewPlot = renderPlotly({

    ## plotly
    plot_ly(coordinates$hourly_data, type = 'scatter', mode = 'lines') |>
      add_trace(x = ~startTime, y = ~dewpoint$value, line = list(color = 'rgb(22, 96, 167)')) |>
      layout(showlegend = FALSE, xaxis = list(title = 'Date'), yaxis = list(title = 'Dewpoint (degrees C)'))

  })

}

## Shiny app
shinyApp(ui, server)
