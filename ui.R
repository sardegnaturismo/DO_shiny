
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#

#

library(shiny)
library(leaflet)
library(plotly)
library(crosstalk)
source("R/translator.R")
source("R/utilities.R")

load("internazionalization/translation.bin")

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
          htmlOutput("header", container = tags$h3),
          
          # HTML("<h3>Sistema interattivo di analisi e visualizzazione dei dati sul movimento turistico della Sardegna 2016 
          #    (Fonte: <a href='http://operatori.sardegnaturismo.it/it/sired-0'>SIRED</a>)</h3>"), 
          div(
                  class="flags",
                  actionButton(
                          inputId = "it",
                          class = "btn action_button flag_right",
                          label = img(src = "bandiera-italia_scalata.png")
                  ),
                  
                  actionButton(
                          inputId = "en",
                          class = "btn action_button flag_right", 
                          label = img(src = "english-flag_resized.png")
                  )
                  
                  
                  
          )

          # tags$img(src="logost.png", class='header_left'),
          # tags$img(src="logo_ras.png", class='header_right')  
  ),br(),
  
  div(
          uiOutput("radio")         
          # radioButtons("measure", tr("misura", language),
          #               c("Arrivi", "Presenze"), selected = "Arrivi")
          
  ), br(),
  fluidRow(
          id = "page_bar",
       column(
               width = 8,
               htmlOutput("map_bar", container = tags$p, class="map_bar")
               
       ),
       column(
               width = 4,
               uiOutput("map_filter_button")
               #actionButton("stop_map_filters", "Elimina filtri mappe", class='stop_filter')
       )
      
       
       
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
                        
                
          ),
          column(
                  width = 6,
                  class = 'border_map',
                  leafletOutput("structure_map", height = "280")
                  
                  
          )
          

  ),
  div(
          id='pie_bar',
          column(
                  width = 8,
                  uiOutput("provenience_bar")
          ),
          column(
                  width = 4,
                  uiOutput("provenience_filter_button")
                  
          )
          
          #tags$p("Seleziona la provenienza cliccando sugli elementi dei grafici")
  ), br(), br(),        
  fluidRow(
        column(
                offset = 4,
                width = 4,
                plotlyOutput("proveniences",  height = '600px')
                # verbatimTextOutput("prov_click")
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
          id='profiling_bar',
          column(
                  width = 8,
                  uiOutput("profiling_bar_title")
          ),
          column(
                  width = 4,
                  uiOutput("profiling_filter_button")
                  
          )

  ), br(), br(), br(),
  fluidRow(
          column(
                  offset = 4,
                  width = 4,
                  plotlyOutput("sex",  height = '600px')                  
                  
          )
  ), br(), br(), br(),
  div(
          id='type_bar',
          column(
                  width = 8,
                  uiOutput("type_bar_title")
          ),
          column(
                  width = 4,
                  uiOutput("type_filter_button")

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
                  


))
