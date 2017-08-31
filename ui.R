
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(plotly)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


shinyUI(fluidPage(
   includeCSS("www/DO.css"),        
                
        
  #Top header
  div(
          tags$img(src="logost.png", class='header_left'),
          tags$img(src="logo_ras.png", class='header_right')
  ),p(), br(),
  div(
          HTML("<h3>Sistema interattivo di analisi e visualizzazione dei dati sul movimento turistico della Sardegna 2016 
             (Fonte: <a href='http://operatori.sardegnaturismo.it/it/sired-0'>SIRED</a>) <div class='flags'><img src='bandiera-italia_scalata.png' class='flag_right'><img src='english-flag_resized.png' class='flag_right'></div></h3>") 
          # tags$img(src="logost.png", class='header_left'),
          # tags$img(src="logo_ras.png", class='header_right')  
  ),br(),
  
  div(
          radioButtons("measure", "Selezionare la misura di analisi: ",
                       c("Arrivi", "Presenze"), selected = "Arrivi")
          
  ),
  div(
       id = "page_bar",
       htmlOutput("page_bar", container = tags$p) 
  ),
  
  fluidRow(
          column( 
                  width = 6,
                  class = "main_map",
                  leafletOutput("province_map", height = "600")
                                           
          ),
          column(
                  width = 6,
                  class = 'border_map',
                  leafletOutput("municipalities_map", height = "280")
                        
                
          )

  ),
  div(
          id='pie_bar',
          tags$p("Seleziona la provenienza cliccando sugli elementi dei grafici")
  ),         
  fluidRow(
        column(
                offset = 4,
                width = 4,
                plotlyOutput("proveniences",  height = '600px')
        )),br(), br(), br(),
  fluidRow(
          column(
                  width = 12, 
                  plotlyOutput("prov_by_nation")
          )
  ), br(), br(), br(),br(), br(),
  fluidRow(
          column(
                  width = 12,
                  plotlyOutput("prov_by_region")
          )
  ),br(), br(), br(), br(),
  
  fluidRow(
          width = 12,
          plotlyOutput("trend_comparison")
          
  ),br(), br(), br(),
  
  
  div(
          id='pie_bar',
          tags$p("Seleziona il tipo di turista (dato riferito ai soli arrivi)")
  ), br(), br(), br(),
  fluidRow(
          column(
                  offset = 4,
                  width = 4,
                  plotlyOutput("sex",  height = '600px')                  
                  
          )
  ), br(), br(), br(),
  fluidRow(
          column(
                  width = 12, 
                  plotlyOutput("accomodated_type")
          )
  ), br(), br(), br(),
  fluidRow(
          column(
                  width = 12, 
                  plotlyOutput("age_range")
          )
  )
                  



  
  
  #actionButton("recalc", "New points")

  # Application title
  # titlePanel("Old Faithful Geyser Data"),
  # 
  # # Sidebar with a slider input for number of bins
  # sidebarLayout(
  #   sidebarPanel(
  #     sliderInput("bins",
  #                 "Number of bins:",
  #                 min = 1,
  #                 max = 50,
  #                 value = 30)
    

    # Show a plot of the generated distribution
    # mainPanel(
    #   plotOutput("distPlot")
    # )
  
))
