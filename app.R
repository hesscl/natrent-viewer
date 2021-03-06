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

#store credentials read in from YAML
cred <- read_yaml("./natrent0.yaml")

#create a pool for database connections
natrent <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "natrent",
  host = "natrent0.csde.washington.edu",
  user = names(cred),
  password = cred[[names(cred)]],
  bigint = "numeric"
)

#close the connection pool when the app stops
onStop(function() {
  poolClose(natrent)
})

#read in crosswalk between loc and metro values
cw <- read.csv("./data/cw.csv", stringsAsFactors = FALSE)

#alphabetize by metro name
cw <- cw %>% arrange(metro)


#### Prep values we will want to draw on --------------------------------------

#distinct locations from Craigslist listings
#locs <- natrent %>%
#  tbl("clean") %>%
#  distinct(listing_loc) %>%
#  arrange(listing_loc) %>%
#  pull(listing_loc) 
#locs[65] <- "New York"
#locs[106] <- "Washington"

#create a vector of the smaller metros with same/similar names to ones we scrape
#match_collisions <- c("Albany, OR", "Albany, GA",  "Anniston-Oxford-Jacksonville, AL",
#                      "Charleston, WV", "Charlottesville, VA", "Columbia, MO", "Columbus, GA-AL", 
#                      "Cleveland, TN", "Columbus, IN", "Greenville, NC", 
#                      "Jackson, MI", "Jackson, TN",
#                      "Jacksonville, NC",  "Rochester, MN", "Springfield, OH",  "Portland-South Portland, ME",
#                      "Springfield, MO", "Springfield, IL", "Wichita Falls, TX")

#CBSA names
#metros <- natrent %>%
#  tbl("cbsa17") %>%
#  filter(memi == "1") %>%
#  distinct(name) %>%
#  arrange(name) %>%
#  filter(!name %in% match_collisions) %>%
#  collect() %>%
#  filter(str_detect(name, str_flatten(locs, collapse = "|"))) %>%
#  pull(name)

#accomodate the honolulu weirdness
#metros <- c(metros[1:16], "New York-Newark-Jersey City, NY-NJ-PA"
#            metros[17]
#            metros[96], 
#            metros[39:71], 
#            "Poughkeepsie-Newburgh-Middletown, NY", metros[72:95], metros[97:101])

#compile a crosswalk
#cw <- cbind.data.frame(locs, metros, stringsAsFactors = FALSE)

#restore original loc names
#cw[61, 1] <- "New York City"
#cw[99, 1] <- "Washington DC"

#add in Durham
#cw[nrow(cw)+1,] <- cbind("Raleigh", "Durham-Chapel Hill, NC")


#### Application --------------------------------------------------------------

#Client UI
ui <- navbarPage("natrent@UW", id = "nav",
         tabPanel("Map",
            div(class="outer",
                
                tags$head(
                  # Include our custom CSS
                  includeCSS("styles.css")
                  #includeScript("gomap.js")
                ),
                
                # If not using custom CSS, set height of leafletOutput to a number instead of percent
                leafletOutput("map", width="100%", height="100%"),
                
                # Shiny versions prior to 0.11 should use class = "modal" instead.
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                              
                              #title
                              h3("Map Controls"),
                              
                              #metro input populated based on values queried above
                              selectInput("metro_select", "Metro:", cw$metro,
                                          selected = "Seattle-Tacoma-Bellevue, WA"),
                              
                              #county input populated dynamically based on the metro chosen
                              uiOutput("county_select"),
                              
                              #bedroom size input populated based on static options
                              selectInput("bed_size_select", "Bedroom Size:",
                                          choices = c(
                                            "Studio" = 0,
                                            "1 Bedroom" = 1,
                                            "2 Bedroom" = 2,
                                            "3 Bedroom" = 3),
                                          selected = 1),
                              
                              #quantile input populated based on static options
                              selectInput("quantile_select", "Asking Rent Quantile:",
                                          choices = c(
                                            "5th Quantile" = .05,
                                            "25th Quantile" = .25,
                                            "40th Quantile" = .4,
                                            "Median" = .50,
                                            "75th Quantile" = .75,
                                            "95th Quantile" = .95),
                                          selected = .5) ,
                              
                              #plot the distribution of the quantile among tracts in county
                              plotOutput("dens", height = 200)
                              )
                )
            )
         )


#Server Backend
server <- function(input, output) {
  
  #### Define SQL Queries
  
  #compute the input quantile per tract in a metro county, return sf
  map_sql <-  "SELECT quantile(e.clean_rent, ?quantile) AS quantile, ST_Transform(f.geometry, 4326), f.geoid, f.statefp, f.countyfp
               FROM clean e
               RIGHT JOIN (
                     SELECT c.geoid, c.countyfp, c.statefp, c.geometry
                     FROM tract17 c
                     INNER JOIN (
                           SELECT b.statefp, b.countyfp, a.cbsafp, a.name AS metro_name, b.name AS county_name
                           FROM cbsa17 a
                           INNER JOIN county17 b ON a.cbsafp = b.cbsafp
                           WHERE a.name = ?metro AND b.countyfp IN (?county)
                     ) d ON c.statefp = d.statefp AND c.countyfp = d.countyfp
               ) f ON ST_Within(e.geometry, f.geometry)
               WHERE e.listing_date >= '2019-01-01' AND
                     e.clean_beds = ?beds AND
                     e.listing_loc IN (?loc)
               GROUP BY f.geoid, f.statefp, f.countyfp, f.geometry
               HAVING count(*) >= 4
               ORDER BY f.geoid"
    
  #compute n per county with names for input dropdown
  cty_sql <- "SELECT count(*) AS cty_n, d.name, d.countyfp
              FROM clean c
              INNER JOIN (
                          SELECT a.name, a.geometry, a.countyfp
                          FROM county17 a
                          JOIN cbsa17 b ON a.cbsafp = b.cbsafp
                          WHERE b.name = ?metro 
              ) d ON ST_Within(c.geometry, d.geometry)
              WHERE c.listing_date >= ?cutoff AND
              c.listing_loc IN (?loc)
              GROUP BY d.name, d.countyfp
              HAVING count(*) >= 4
              ORDER BY cty_n DESC"
  
  #### Assign reactive expressions for the query results
  
  #create a reactive data.frame for caching the county values
  cty_query_result <- reactive({
    
    loc_string <- data.frame(val = paste0("'", cw$loc[cw$metro == input$metro_select], "'", collapse = ","))
    
    
    #interpolate the query for metro's county counts of listings for past week, descending n
    cty_query <- sqlInterpolate(natrent, cty_sql,
                                metro = input$metro_select,
                                loc = loc_string$val,
                                cutoff = as.character(Sys.Date()-7))
    
    #submit the query, return as fn() output
    dbGetQuery(natrent, cty_query)
  })
  
  #create a reactive data frame for caching the primary query result
  map_query_result <- reactive({
    
    #trying a workaround to supply a list to sqlInterpolate
    cty_string <- data.frame(val = paste0("'", input$county_select, "'", collapse = ","))
    
    loc_string <- data.frame(val = paste0("'", cw$loc[cw$metro == input$metro_select], "'", collapse = ","))
    
    
    #interpolate the values from input into the sql
    map_query <- sqlInterpolate(natrent, map_sql,
                                metro = input$metro_select,
                                loc = loc_string$val,
                                county = cty_string$val,
                                beds = input$bed_size_select,
                                quantile = input$quantile_select)
    
    #read the query result into memory, return as fn output
    st_read(natrent, query = map_query)
  })
  
  #### Use the county query results for the UI
  
  #populate the counties input dropdown based on metro selection
  output$county_select <- renderUI({
    
    choice_vec <- cty_query_result()$countyfp
    names(choice_vec) <- cty_query_result()$name
    
    checkboxGroupInput("county_select", "County:", 
                       choices = choice_vec,
                       selected = choice_vec[1])
  })
  
  #### Generate the focal visualizations
  
  #render map dynamically based on user's input for state
  output$map <- renderLeaflet({
    
    if(length(input$county_select) == 0){
      return()
      
    }
    
    validate(
      need(nrow(map_query_result()) > 0, "")
    )

    #format the labels for the hover over tooltip
    labels <- sprintf(
      "<strong>Tract: %s</strong><br/>%gth Quantile: $%g",
      map_query_result()$geoid, type.convert(input$quantile_select) * 100, map_query_result()$quantile
    ) %>% lapply(htmltools::HTML)
    
    #define a function for the choropleth scale
    pal <- colorNumeric("magma", 
                        domain = map_query_result()$quantile,
                        reverse = TRUE)
    
    #plot the sf using leaflet
    leaflet(map_query_result()) %>%
      addTiles() %>%
      addPolygons(fillColor = ~ pal(quantile),
                  fillOpacity = .75,
                  color = "white",
                  opacity = 1,
                  weight = .5,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666"),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, 
                values = ~ quantile,
                title = paste0(type.convert(input$quantile_select) * 100, "th Quantile"),
                position = "bottomleft")
  })
  
  #render rent density curve based on the current sf result
  output$dens <- renderPlot({
    
    if(length(input$metro_select) == 0 | length(input$county_select) == 0){
      return(NULL)
      
    } else{
      
      ggplot(map_query_result(), aes(x = quantile)) +
        geom_density() +
        theme_minimal() +
        scale_x_continuous(labels = scales::dollar) +
        xlab(paste0("\n", type.convert(input$quantile_select) * 100, "th Quantile")) +
        ylab("Density\n")
    }
    
  })
}

#Create Shiny App
shinyApp(ui, server)
