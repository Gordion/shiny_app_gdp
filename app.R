library(shiny)
library(SPARQL)
library(shinydashboard)
library(dplyr)
library(dygraphs)
library(tidyr)
library(xts)

# Load Open Data Platform data into memory

endpoint <- 'http://statistics.gov.scot/sparql'
query <-'PREFIX qb: <http://purl.org/linked-data/cube#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  select ?date ?sector ?value
  where {
  ?data qb:dataSet <http://statistics.gov.scot/data/gross-domestic-product-quarterly-output-by-industry>. #this line selects the data
  ?data <http://purl.org/linked-data/cube#measureType> <http://statistics.gov.scot/def/measure-properties/index>. #selects the measure
  ?data <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?dateURI. #exposes the reference periods as "dateURI"
  ?dateURI rdfs:label ?date. #translates the yearURI into a string, exposes it as ?date
  ?data <http://statistics.gov.scot/def/dimension/industrySector> ?sectorURI. #exposed industry sector as sector
  ?sectorURI rdfs:label ?sector.
  ?data <http://statistics.gov.scot/def/measure-properties/index> ?value. #exposes count data as ?value
  }'

query_data <- SPARQL(endpoint,query)$results %>%
  filter(nchar(date) == 7)


latest_data<- query_data %>%
  select(date) %>%
  filter(nchar(date) == 7) %>%
  arrange(date) %>%
  summarise(value=last(date))

earliest_data<- query_data %>%
  select(date) %>%
  filter(nchar(date) == 7) %>%
  arrange(date) %>%
  summarise(value=first(date))

current_year <- as.numeric(substr(latest_data,1,4))
first_year <- as.numeric(substr(earliest_data,1,4))

# Create shiny ui

ui <- dashboardPage(
  dashboardHeader(
    title = "GDP Quarterly Change"
  ),
  dashboardSidebar(
    sidebarMenu(
      checkboxGroupInput("main_1",
                         label="Economy Sector",
                         choices = 
                           list("Total Gross Value Added" = "Total Gross Value Added (GVA) (Section A-T)",
                                "Agriculture, Forestry and Fishing" = "Agriculture, Forestry and Fishing (Section A)",
                                "Production" = "Production (Section B-E)"),
                         selected = c("Total Gross Value Added (GVA) (Section A-T)",
                                      "Agriculture, Forestry and Fishing (Section A)",
                                      "Production (Section B-E)"
                         )
      ),
      menuItem("Production subsectors", startExpanded = FALSE,
               checkboxGroupInput("production_select", 
                                  label=NULL,
                                  choices =
                                    list("Mining & Quarrying" = "Mining & Quarrying (Section B)",
                                         "Manufacturing" = "Manufacturing (Section C)",
                                         "Elecricity, Gas, Steam and Air" = "Elecricity, Gas, Steam and Air (Section D)",
                                         "Water Supply and Sewerage" = "Water Supply and Sewerage (Section E)"
                                    )
               )
      ),
      checkboxGroupInput("main_2",
                         label=NULL,
                         choices = list("Construction" = "Construction (Section F)", 
                                        "Services" = "Services (Section G-T)"
                         ),
                         selected = c("Construction (Section F)",
                                      "Services (Section G-T)")
      ),
      menuItem("Services subsectors",
               checkboxGroupInput("service_select", 
                                  label=NULL,
                                  choices =
                                    list("Distribution, Hotels and Restaurants" = "Distribution, Hotels and Restaurants (Section G,I)",
                                         "Transport, Storage and Communication" = "Transport, Storage and Communication (Section H,J)",
                                         "Business Services and Finance" = "Business Services and Finance (Section K-N)",
                                         "Government and Other Services" = "Government and Other Services (Section O-T)")
               )
      )
    ),
    sliderInput("slider2", label = "Year range", min = first_year, 
                max = current_year, value = c(current_year-4, current_year),sep="")
  ),
  
  # Show the line graph created by server
  
  dashboardBody(box(
    title = paste("Scotland's Gross Domestic Product, latest revision:", latest_data),
        dygraphOutput("linePlot"),
    tagList("For further information see ", 
            a("Scotland's GDP report",
            href = "http://www.gov.scot/Topics/Statistics/Browse/Economy/GDP"))
    , width = 9
      ),
    box(title = "Legend", textOutput("legend_div"),width=3)
   ),
  
  # Modify styles
  
  tags$head(tags$style(HTML('
        .skin-blue .main-sidebar {
                              background-color: #420B4E;
        }
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #81358D;
        }  
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #420B4E;
        }
        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #420B4E;
        }
        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #81358D;
                            color: #EEEEEE;
                            }'
                            )
                       )
            )

)

# Define server logic
server <- shinyServer(function(input, output) {
  
   output$linePlot <- renderDygraph({
     
     year_value <- input$slider2
     sector_select <- c(input$main_1,input$main_2,
                        input$production_select,
                        input$service_select)
     
     req(sector_select)
     req(year_value)

     graph_data <- query_data %>%
       filter(nchar(date) == 7) %>%
       filter(substr(date,1,4) >= year_value[1] & substr(date,1,4) <= year_value[2]) %>%
       filter(sector %in% sector_select) %>%
       spread(sector,value)

     graph_data$date<-as.yearqtr(graph_data$date,format = "%Y-Q%q")
     
     #Create time series to automate x-axis labels
     graph_data<-xts(graph_data,order.by=graph_data$date)
     
     graph_data$date<-NULL
     
    # Create linegraph out of dataframe
     dygraph(graph_data) %>%
       dyOptions(strokeWidth = 3) %>%
       dyAxis("y", label = "Index (2014 = 100)") %>%
       dyLegend(labelsSeparateLines = TRUE,
                labelsDiv = "legend_div") 
 
   })
   
})

# Run the application 
shinyApp(ui = ui, server = server)