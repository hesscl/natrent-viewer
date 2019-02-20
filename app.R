## app.R ##
library(dplyr)
library(dbplyr)
library(ggplot2)
library(stringr)
library(sf)
library(leaflet)
library(pool)
library(yaml)
library(shiny)
library(shinydashboard)

#make sure wd is base of natrent-viewer repo
setwd("H:/natrent-viewer")

#store credentials at base dir of UDrive (H:/) as YAML
cred <- read_yaml("H:/natrent0.yaml")

#create a pool for database connections
natrent <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "natrent",
  host = "natrent0.csde.washington.edu",
  user = names(cred),
  password = cred[[names(cred)]],
  bigint = "character"
)

#close the connection pool when the app stops
onStop(function() {
  poolClose(natrent)
})


#Client UI
ui <- dashboardPage(
  dashboardHeader(
    title = "natrent@UW"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$div(style="position: absolute; top: 0; left: 0; right: 0; bottom: 0"),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidPage(
                column(width = 9, 
                       box(width = NULL, 
                           leafletOutput("metro_map", width = "100%", height = "1000px"))),
                column(width = 3,
                       box(width = NULL,
                           title = "Controls",
                           selectInput("metro_select", "Metro:", 
                                       choices = natrent %>% 
                                                 tbl("cbsa17") %>% 
                                                 filter(lsad == "M1") %>%
                                                 pull(name) ,
                                       selected = "Seattle-Tacoma-Bellevue, WA"),
                           uiOutput("county_select"),
                           selectInput("bed_size_select", "Bedroom Size:",
                                       choices = c(
                                        "Studio" = 0,
                                        "1 Bedroom" = 1,
                                        "2 Bedroom" = 2,
                                        "3 Bedroom" = 3),
                                      selected = 1),
                           selectInput("quantile_select", "Asking Rent Quantile:",
                                       choices = c(
                                         "5th Quantile" = .05,
                                         "25th Quantile" = .25,
                                         "40th Quantile" = .4,
                                         "Median" = .50,
                                         "75th Quantile" = .75,
                                         "95th Quantile" = .95),
                                       selected = .5)
                       )
                    )
            )
          ) #,
      
      # Second tab content
      #tabItem(tabName = "widgets",
      #        h2("Widgets tab content")
      #        )
)))


#Server Backend
server <- function(input, output) {
  
  #compute the n per tract in a metro county, return sf
  base_sql <-  "SELECT quantile(e.clean_rent, ?quantile) AS quantile, ST_Transform(f.geometry, 4326), f.geoid
                FROM clean e
                JOIN (
                      SELECT c.geoid, c.geometry
                      FROM tract17 c
                      JOIN (
                            SELECT b.statefp, b.countyfp, a.cbsafp, a.name AS metro_name, b.name AS county_name
                            FROM cbsa17 a
                            JOIN county17 b ON a.cbsafp = b.cbsafp
                            WHERE a.name = ?metro AND b.name = ?county
                      ) d ON c.statefp = d.statefp AND c.countyfp = d.countyfp
                ) f ON ST_Within(e.geometry, f.geometry)
                WHERE e.listing_date >= '2019-01-01' AND
                      e.clean_beds = ?beds AND
                      e.listing_loc LIKE ?loc
                GROUP BY f.geoid, f.geometry
                HAVING count(*) >= 5
                ORDER BY f.geoid"
  
  #populate the counties selection based on metro selection
  output$county_select <- renderUI({
    
    counties <- natrent %>% 
      tbl("county17") %>%
      semi_join(tbl(natrent, "cbsa17") %>%
                  filter(name == input$metro_select) %>%
                  select(cbsafp), by = "cbsafp") %>%
      pull(name) 
    
    selectInput("county_select", "Choose County", counties,
                selected = "King")
  })
  
  #create a reactive data frame for caching the query result
  db_result <- reactive({
    
    #interpolate the values from input into the sql
    query_rent <- sqlInterpolate(natrent, base_sql,
                                 metro = input$metro_select,
                                 loc = paste0(substr(str_split_fixed(input$metro_select, 
                                          pattern = "\\-", n = 2)[1], 1, 8), "%"),
                                 county = input$county_select,
                                 beds = input$bed_size_select,
                                 quantile = input$quantile_select)
    
    #read the query result into memory
    st_read(natrent, query = query_rent)
  })
  
  #render map dynamically based on user's input for state
  output$metro_map <- renderLeaflet({
    
    if(length(input$county_select) == 0){
      return(NULL)
      
    } else{
      
      #reactive result will be what we plot
      result <- db_result()
      
      pal <- colorNumeric("viridis", domain = result$quantile)
      
      leaflet(result) %>%
        addTiles() %>%
        addPolygons(fillColor = ~ pal(quantile),
                    fillOpacity = .75,
                    color = "white",
                    opacity = 1,
                    weight = .5) %>%
        addLegend(pal = pal, 
                  values = ~ quantile,
                  position = "bottomleft")

      #visualize the simple feature with sf
      #ggplot(db_result(), aes(fill = quantile)) +
      #  geom_sf(color = alpha("white",  .5), size = 0.2) +
      #  scale_fill_viridis_c(option = "D", direction = -1)
    }
  })
}

#Create Shiny App
shinyApp(ui, server)
