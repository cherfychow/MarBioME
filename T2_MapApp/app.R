#
# MarBioME Task 2
# Dynamic spatial visualisation of the marine monitoring programmes in the European region
#

require(shiny)
require(shinyjs)
require(leaflet)

require(sf)
require(dplyr)
require(readxl)
require(viridis)
require(stringr)

# Data set up -------------------------------------------------------------

# paths are relative to app.R, not working directory!

# read in cleaned and prepped spatial geometries
load('T2_spatial.RData')
# make a vector object of the DataIDs that have coordinates
# this will be our key to determine choro vs geometry viz
coordID <- c(as.vector(T2_coord_hex$DataID),
             as.vector(T2_coord_polygon$DataID),
             as.vector(T2_coord_pt$DataID))
programs <- read_excel('../Task2_MonitoringProgrammes.xlsx', sheet=1) # read this in so we can link geometries with metadata fields
programs$StartYear <- as.integer(programs$StartYear)
programs$EndYear <- as.integer(programs$EndYear)

# make an object of pretty taxa names
taxa <- colnames(programs)[14:25]
taxa[4] <- 'Benthic invertebrates'
taxa[9] <- 'Hard corals'
rm(T2_coord_mp)
programs$Taxa <- '' # switch back to comma separated format for easier filtering
for (i in 14:25) {
  programs$Taxa[which(programs[i] == 1)] <- paste(programs$Taxa[which(programs[i] == 1)], taxa[i-12], sep=', ')
}
programs$Taxa <- str_remove_all(programs$Taxa, '^\\,\\s')

# link spatial geometries with programme metadata
T2_coord_pt$Latitude <- st_coordinates(T2_coord_pt)[,2]
T2_coord_pt$Longitude <- st_coordinates(T2_coord_pt)[,1]
T2_coord_hex <- left_join(T2_coord_hex, programs, by="DataID")
T2_coord_pt <- left_join(T2_coord_pt, programs, by="DataID")
T2_coord_polygon <- left_join(T2_coord_polygon, programs, by="DataID")

# CRS check, make sure they're all the same
# proj <- '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m no_defs' # projection code
T2_coord_hex <- st_transform(T2_coord_hex, crs = st_crs(4326))

# T2_coord_pt$Longitude <- st_coordinates(T2_coord_pt)[,1] # make lat longs separate columns again for leaflet to read
# T2_coord_pt$Latitude <- st_coordinates(T2_coord_pt)[,2]
# make a tibble for studies where coordinate info isn't available for choropleth mapping
T2_choro <- programs %>% filter(!DataID %in% coordID)


# UI ------------------------------------------------------------------

# Define UI for application that produces an interactive map
# This app has UI elements for filtering mapping according to taxa and monitoring programme years
ui <- fluidPage(
  
  tags$head(
    tags$link(rel="stylesheet", type='text/css', href="style.css")
    # Style import
    #tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    # things on UI/client side are located in www folder
    # all assets that have to be accessible for frontend like css or images have to be in the www folder
  ),
  useShinyjs(),
  
  # Application title
  h1(id ='title',"Biodiversity Monitoring Programmes in Europe"),
  
  tags$div(id = 'container',
  # Sidebar with a slider input for number of bins 
    # instruction/brief description panel
    tags$div(id = 'control',
                 tags$p("This is an interactive map showing MarBioME metadata on 653 marine monitoring programmes. These programmes are represented either in spatial polygons, hexagon grids, points, or in a choropleth map of the country's EEZ areas, depending on available data. Click on each element to show more details. Please be patient when filtering elements by taxa or years covered." ),
                 h4('Filter by taxa'),
                 actionButton("selectall", label="Select/Deselect all"),
                 checkboxGroupInput('Taxa', label=NULL,
                                    choiceNames=taxa,
                                    selected=taxa,
                                    choiceValues=taxa),
                 
                 h4('Filter by programme year'),
                 sliderInput('Year', label=NULL, sep='',
                             min=1893, max=2022, value=c(1893, 2022),
                             round = T, ticks = F, animate = FALSE),
                 actionButton('reset', 'Reset'),
             tags$p(style="color: #BBB; font-size: 11px; margin-top: 25px;", 
                    strong("Acknowledgments"), br(), 
                    "The spatial data for the world exclusive economic zone boundaries came from the Flanders Marine Insitute through MarineRegions.,
                    Flanders Marine Institute (2019). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 11. Available online at https://www.marineregions.org/. https://doi.org/10.14284/386")
    ),
    
    # Show a plot of the generated distribution
    tags$div(id="map",
              leafletOutput("ProgramMap")
    ))
  )


# Server --------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Implement user settings
  
  # reset button
  observeEvent(input$reset, {
    reset('control')
  })
  
  # select/deselect all taxa button
  
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0){
        updateCheckboxGroupInput(inputId="Taxa",
                                 choices = colnames(programs)[14:25],
                                 selected = colnames(programs)[14:25])
      } else {
        updateCheckboxGroupInput(inputId="Taxa",
                                 choices = colnames(programs)[14:25],
                                 selected = c())
      }}
  })
  
  # Implement user defined filters on our data geometry objects
  # eval(parse(text = paste0(input$Taxa, ' == 1')))
  
  # build a search string from the taxa chosen
  taxafind <- reactive({paste(input$Taxa, collapse='|')})
  
  T2_hex <- reactive({
    T2_coord_hex %>% filter(str_detect(Taxa, taxafind()),
                            StartYear >= input$Year[1] | is.na(StartYear), EndYear <= input$Year[2] | is.na(EndYear))
  })
  
  T2_polygon <- reactive({
    T2_coord_polygon %>% filter(str_detect(Taxa, taxafind()),
                                StartYear >= input$Year[1] | is.na(StartYear), EndYear <= input$Year[2] | is.na(EndYear))
  })
  
  T2_point <- reactive({
    T2_coord_pt %>% filter(str_detect(Taxa, taxafind()),
                           StartYear >= input$Year[1] | is.na(StartYear), EndYear <= input$Year[2] | is.na(EndYear))
  })
  
  ## Data prep choropleth ----------------------------------------------------
  
  # Choropleth data is not prepped with the other files because that changes depending on filters
  # I'm only doing this once and exporting the simplified shapefile because it's computationally taxing
  # EEZ <- st_read('boundaries_europe/World_EEZ/EEZ_cleaned.shp')
  # EEZ <- st_transform(EEZ, st_crs(4326)) %>% st_make_valid() %>% st_wrap_dateline(options = "WRAPDATELINE=YES")
  load('EEZ.RData')
  
  # choropleth calculations
  # Program totals by country
  # some programme are multi-countries, so we need to use stringr to detect these counts
  # all of these objects change if T2_choro gets filered, so they need to be reactive
  
  
  
  choro <- reactive({
    T2_choro %>% filter(str_detect(Taxa, taxafind()),
                        StartYear >= input$Year[1] | is.na(StartYear), EndYear <= input$Year[2] | is.na(EndYear)) %>% 
      pull(Country) %>% na.omit %>% 
      str_split(., '\\,', simplify=F) %>% # split comma separated countries
      unlist %>% # collapse into one vector
      data.frame(Country = .) %>% as_tibble %>% # turn the country list into a tibble column
      filter(!str_detect(Country, 'Slovak')) %>% # can't visualise programs that only say they're regional
      group_by(Country) %>% summarise(ProgramTotal = n())
  })
  
  ## Leaflet overall prep ------------------------------------------------------------
  
  # define the CRS, leaflet style
  # EUconic <- leafletCRS(crsClass = 'L.Proj.CRS', proj4def = proj, resolutions = seq(2^25, 1, length.out = 15), code = 'ESRI:102031')
  # create a palette for the EEZ choropleth
  pal <- colorBin(palette=viridis(n = 9, begin = 0.1, end = 1, direction = -1, option = 'mako'),
                  domain=0:50,
                  bins=seq(0, 50, by=10))
  
  output$ProgramMap <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom=3, maxZoom = 6)) %>%
      setView(lng = 15, lat = 60, zoom = 4) %>% 
      # add a legend for the choropleth
      addLegend(data=choro() %>% na.omit, position='bottomright', title='Number of programmes', pal=pal, opacity=1,
                values=~ProgramTotal) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      # # EEZ choropleth
      addPolygons(data = choro() %>% # calculate program totals per country and make a column
                    left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # put that column into the EEZ sf object
                    filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION') %>% filter(!is.na(ProgramTotal)),
                  fillColor = ~pal(ProgramTotal), fillOpacity = 0.7,
                  weight = 1, color = '#DDD', highlightOptions = highlightOptions(weight = 2.5, color = "#1f53ff", bringToFront = F),
                  popup = ~paste0('<h4>', TERRITORY1, '</h4>',
                                  '<strong>Total monitoring programmes: </strong>', ProgramTotal)) %>%
      # multipoint hex grids
      addPolygons(data = T2_hex(), fillColor = '#FFFFFF', color = '#222222', fillOpacity = 0.9, weight = 1.5,
                  highlightOptions = highlightOptions(weight = 5, fillOpacity = 1, color = "#1f53ff", bringToFront = T),
                  popup = ~paste0('<h4>', ProgramName, '</h4>',
                                  '<span style = "font-size: 15px;">', StartYear, ' - ', ifelse(is.na(EndYear) & Ongoing == 'Yes', yes = 'Ongoing', no = 'Unknown'), '</span><br/>',
                                  '<strong>Organisation:</strong> ', Organisation, '<br/>',
                                  '<strong>Location:</strong> ', Location, '<br/>',
                                  '<strong>Covered taxa:</strong> ', Taxa, '<br/>',
                                  '<strong>Monitoring frequency: </strong>', TemporalRes, '<br/>'
                  )) %>%
      # Programme polygons
      addPolygons(data = T2_polygon(), fillColor = '#FFFFFF', color = '#222222', fillOpacity = 0.7, weight = 1.5,
                  highlightOptions = highlightOptions(weight = 5, fillOpacity = 1, color = "#1f53ff", bringToFront = T),
                  popup = ~paste0('<h4>', ProgramName, '</h4>',
                                  '<span style = "font-size: 15px;">', StartYear, ' - ', ifelse(is.na(EndYear) & Ongoing == 'Yes', yes = 'Ongoing', no = 'Unknown'), '</span><br/>',
                                  '<strong>Organisation:</strong> ', Organisation, '<br/>',
                                  '<strong>Location:</strong> ', Location, '<br/>',
                                  '<strong>Covered taxa:</strong> ', Taxa, '<br/>',
                                  '<strong>Monitoring frequency: </strong> ', TemporalRes, '<br/>')) %>%
      # single point programmes
      addCircleMarkers(data = T2_point(), lat = ~Latitude, lng = ~Longitude, fillOpacity = 0.7,
                       radius = 8, fillColor = '#FFFFFF', color = "#333333", weight = 3,
                       clusterOptions = markerClusterOptions(),
                       popup = ~paste0('<h4>', ProgramName, '</h4>',
                                       '<span style = "font-size: 15px;">', StartYear, ' - ', ifelse(is.na(EndYear) & Ongoing == 'Yes', yes = 'Ongoing', no = 'Unknown'), '</span><br/>',
                                       '<strong>Organisation:</strong> ', Organisation, '<br/>',
                                       '<strong>Location:</strong> ', Location, '<br/>',
                                       '<strong>Covered taxa:</strong> ', Taxa, '<br/>',
                                       '<strong>Monitoring frequency: </strong>', TemporalRes, '<br/>'))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# # preliminary plots
# library(ggplot2)
# ggplot() +
#   geom_sf(data = world, color = 'grey80', fill = 'white') +
#   geom_sf(data = choro %>% filter(is.na(ProgramTotal)), fill = 'white', color = 'grey80', size = 0.2) +
#   geom_sf(data = choro %>% filter(!is.na(ProgramTotal)), aes(fill = ProgramTotal), size = 0.2) +
#   geom_sf(data = T2_coord_hex, fill = 'white', colour = 'blue') +
#   geom_sf(data = T2_coord_polygon, fill = 'transparent', color = 'blue') +
#   geom_sf(data = T2_coord_pt, fill = 'white', colour= 'blue', size = 2, shape = 21) +
#   scale_fill_viridis(option = 'mako', begin = 0.25, end = 1, direction = -1) +
#   coord_sf(xlim = c(-20,60), ylim = c(30,80)) +
#   theme_minimal()
# 
# ggplot() +
#   geom_sf(data = world, color = 'grey80', fill = 'white') +
#   geom_sf(data = T2_coord_pt, fill = 'white', colour= 'blue', size = 2, shape = 21)
