library(shiny)
library(SPARQL)
#library(ggplot2)
#library(stringi)
library(RColorBrewer)
library(shinydashboard)
library(dplyr)
library(dygraphs)
library(zoo)
library(xts)
library(tidyr)

#http://statistics.gov.scot/data/gross-domestic-product-quarterly-output-by-industry 

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

qd <- SPARQL(endpoint,query)$results %>%
  filter(nchar(date) == 7)


latest_data<- qd %>%
  select(date) %>%
  filter(nchar(date) == 7) %>%
  arrange(date) %>%
  summarise(value=last(date))

#getPalette <-colorRampPalette(brewer.pal(12, "Set3"))

#cols <- colorRampPalette(brewer.pal(8, "Dark2"))
#myPal <- cols(length(unique(qd$sector)))

# Define UI for application
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
      
      #checkboxInput("Total Gross Value Added (GVA) (Section A-T)", "Total Gross Value Added", TRUE),
      #checkboxInput("Agriculture, Forestry and Fishing (Section A)", "Agriculture, Forestry and Fishing", TRUE),
      #checkboxInput("Production (Section B-E)", "Production", TRUE),
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
    sliderInput("slider2", label = "Year range", min = 2000, 
                max = 2017, value = c(2013, 2017),sep="")
  ),
      # Show a plot of the line graph
  dashboardBody(box(
    title = paste("Scotland's Gross Domestic Product, latest revision:", latest_data),
        dygraphOutput("linePlot")
    , width = 9
      ),
    box(title = "Legend", textOutput("legend_div"),width=3)
   ),
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
                            }
')))


)

# Define server logic
server <- shinyServer(function(input, output) {
  
   output$linePlot <- renderDygraph({
     
     year_value <- input$slider2
     sector_select <- c(input$main_1,input$main_2,
                        input$production_select,
                        input$service_select)

     df <- qd %>%
       filter(nchar(date) == 7) %>%
       filter(substr(date,1,4) >= year_value[1] & substr(date,1,4) <= year_value[2]) %>%
       filter(sector %in% sector_select) %>%
       spread(sector,value) 
     
     df$date<-as.yearqtr(df$date,format = "%Y-Q%q")
     
     df<-xts(df,order.by=df$date)
     
     df$date<-NULL
     
     #df$date <- NULL 
      # draw the histogram with the specified number of bins
     dygraph(df) %>%
       dyOptions(strokeWidth = 3) %>%
       #dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
       dyLegend(labelsSeparateLines = TRUE,labelsDiv = "legend_div") 
     #%>% 
      # dyLegend()
     
      # ggplot(df,aes(x=date,y=value,group=sector, color = sector)) +
      #   geom_line(size=2) +
      #   theme_bw() + 
      #   theme(axis.title.x = element_text(face="bold",
      #                                     #colour="#990000",
      #                                     size=20),
      #         axis.title.y = element_text(face="bold",
      #                                     #colour="#990000",
      #                                     size=20),
      #         axis.text.x  = element_text(angle=90,
      #                                     vjust=.5,
      #                                     size=15),
      #         axis.text.y  = element_text(size=15)
      #         
      #         #legend.direction = "vertical"
      #         )  +
      #   theme(legend.text=element_text(size=14)) +
      #   ggtitle(paste("GDP by industry sector")) +
      #   theme(plot.title = element_text(face="bold", size=20, hjust=0),
      #         legend.title=element_text(size=15)) + 
      #   labs(x="Date",y="Index (2013 = 100)",color = "Industry Sector") +
      #   scale_fill_manual(values = myPal)
        
      
      #plot_ly(df,x=~year,y=~count,type="line")

   }
   #, height = 500, width = 1000
   
   )
   
})

# Run the application 
shinyApp(ui = ui, server = server)