
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#

#

library(shiny)
library(shinyjs)
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
   useShinyjs(),
   extendShinyjs(text = "shinyjs.resetProfileClick = function() { Shiny.onInputChange('.clientValue-plotly_click-sex_pie', 'null'); }"),
   extendShinyjs(text = "shinyjs.resetProvenienceClick = function() { Shiny.onInputChange('.clientValue-plotly_click-prov_pie', 'null'); }"),        
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
  
  
  
  sidebarLayout(
    sidebarPanel(id="active_filters", width=2,
                       
                       div(id="footer" 
                       # HTML('<ul class="nav nav-tabs">
                       #      <li class="active"><a id="show_filters" href="#mostra-filtri" data-toggle="tab">Filtri Applicati</a></li>
                       #      </ul>')
                       )),
    mainPanel(
 
      
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
      
    )
  )

  

    

  
                  


))
