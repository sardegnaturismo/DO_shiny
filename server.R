
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(rgdal)
library(plotly)
library(data.table)
library(dplyr)
library(htmltools)
library(crosstalk)
source("R/arrivals_presences.R")
source("R/proveniences.R")
source("R/profiling.R")
source("R/utilities.R")
source("R/update_translation.R")
source("R/translator.R")
source("R/layoutBlocks.R")
source("internazionalization/translation_functions.R")

provinces <- readOGR("shapes/Prov2016_rprj.shp", encoding = "UTF-8")
sardinian_provinces <- subset(provinces, provinces$PROVINCIA %in% c("Sassari", "Nuoro", "Cagliari", "Oristano", "Olbia-Tempio", "Ogliastra",
                                                                    "Medio Campidano", "Carbonia-Iglesias"))
municipalities <- readOGR("shapes/Com2016_rprj.shp")
aggregate_movements <- fread("data/agg_ope_line_20xx.csv")
aggregate_web_data <- fread("data/agg_ope_web.csv")
map_threshold <- fread("data/soglia_map_prov_com_ope.csv")
structures <- fread("data/struttura_info_ope.csv")

### load translation file ###
load("internazionalization/translation.bin")


shinyServer(function(input, output, session) {
        
        ######## Map filters elimination ########
        ### reactive variables ###
        data <- reactiveValues(clickedProvince = NULL, clickedMunicipality = NULL)
        change <- reactiveValues(language = "it")
        
        ##### Observers ###################
        observeEvent(input$province_map_shape_click,
                     {data$clickedProvince <- input$province_map_shape_click
                     ####### highlight selection ####
                     #define leaflet proxy for second regional level map
                     # click <-input$province_map_shape_click
                     # proxy <- leafletProxy("province_map")
                     # #province_ids <- sapply(1:8, function(x) sardinian_provinces@polygons[[x]]@ID)
                     # selected_province <- sardinian_provinces[sardinian_provinces$COD_PRO == click$id, ]
                     # print("**selected province ***")
                     # print(selected_province)
                     # 
                     # #map clicked on polygons
                     # proxy %>% addPolygons(data = selected_province,
                     #                       fillColor = "red",
                     #                       fillOpacity = 1,
                     #                       weight = 1,
                     #                       color = "black",
                     #                       stroke = T,
                     #                       layerId = "selected")
                     # 
                     # #remove polygon group that are clicked twice
                     # if(click$group == "selected"){
                     #         proxy %>%
                     #                 clearGroup(group = "selected")
                     # } #END CONDITIONAL
                           
                                          
                     
                     })
        observeEvent(input$municipalities_map_shape_click,
                     {data$clickedMunicipality <- input$municipalities_map_shape_click})
        
        observeEvent(input$stop_map_filters,
                     {data$clickedProvince <- NULL
                      data$clickedMunicipality <- NULL})
        observeEvent(input$it, {
                change$language <- "it"
        })
        observeEvent(input$en, {
                change$language <- "en"
        })
        
        #################################################
        
        #### UI multilingual element generation ##### 
        output$header <- renderText({
                generateHeaderTitle(change$language)
        })
        
        
        output$radio <- renderUI({
                generateRadio(change$language)
        })
        
        output$map_filter_button <-renderUI({
                generateMapFilterButton(change$language)
        })
        
        output$provenience_bar <- renderUI({
                bar_title <- tr("provenience_bar", change$language)
                p(bar_title)
        })
        
        output$provenience_filter_button <- renderUI({
                generateFilterButton(change$language, "stop_provenience_filter", "elimina_filtri_provenienza")
        })
        
        output$profiling_bar_title <- renderUI({
                bar_title <- tr("profiling_bar_title", change$language)
                p(bar_title)
        })
        
        
        output$profiling_filter_button <- renderUI({
                generateFilterButton(change$language, "stop_profiling_filter", "elimina_filtri_profilazione")
        })
        
        output$type_bar_title <- renderUI({
                p()
        })
        
        output$type_filter_button <- renderUI({
                generateFilterButton(change$language, "stop_accomodated_type_filter", "elimina_filtri_alloggiato")
        })
         
        
        output$province_map <- renderLeaflet({
                
                measure = input$measure
                print(paste("measure: ", measure))
                measure_selected = get_arrivals(aggregate_movements)
                legend_title = tr("numero_arrivi", change$language)
                if ((!is.null(measure)) && ((measure == "Presenze") || (measure == "Presences"))){
                        measure_selected = get_presences((aggregate_movements))
                        legend_title = tr("numero_presenze", change$language)
                }
                 
                ### add arrivals to sardinian_provinces dataset
                sardinian_provinces$measure <- sapply(sardinian_provinces$SIGLA, function(x) measure_selected[[2]][measure_selected[[1]] == x])
                print(sardinian_provinces$measure)
                
                #qpal <- colorQuantile("YlOrRd", sardinian_provinces$arriv, n = 8)
                pal <- colorNumeric("Purples", 
                                    domain = sardinian_provinces$measure)
            
                #~colorQuantile("YlOrRd", arriv)(arriv)
                m <- leaflet() %>%
                        setView(lng=8.981, lat=40.072, zoom=8) %>%
                        addTiles() %>%
                        addPolygons(data=sardinian_provinces, layerId = sardinian_provinces$COD_PRO, color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.8, 
                                    fillColor = ~pal(measure),
                                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                        bringToFront = TRUE), label = sardinian_provinces$PROVINCIA, labelOptions = labelOptions(clickable = FALSE, noHide = TRUE)) %>%
                        
                        addLegend("bottomright", pal = pal, values = sardinian_provinces$measure, title = legend_title, opacity = 1)
              m        
                
        })
        

        output$map_bar <- renderText({
                
                selected_area = "Sardegna"
                out <- NULL
                print("clickMarker")
                print(data$clickedProvince)
                
               
                # if (!is.null(input$province_map_shape_click[[1]])){
                #         prov_code = input$province_map_shape_click[[1]]
                #         selected_area =  as.character(sardinian_provinces$PROVINCIA[sardinian_provinces$COD_PRO == prov_code])      
                #     }
                if(!is.null(data$clickedProvince[[1]])){
                        prov_code <- data$clickedProvince[[1]]
                        selected_area = as.character(sardinian_provinces$PROVINCIA[sardinian_provinces$COD_PRO == prov_code])
                }
                if(selected_area == "Sardegna"){
                        #out <- paste("Visualizza i dati turistici dell'intera <strong> Sardegna</strong> scorrendo in basso la pagina oppure seleziona una <strong>provincia</strong> nella mappa")
                        out <- tr("map_bar_region", change$language)
                        print("selected area: ")
                        print(selected_area)
                       
                        
                        
                }else{
                        area <- tr(selected_area, change$language)
                        out <- paste(tr("map_bar_province1", change$language), "<strong>", area, "</strong>", tr("map_bar_province2", change$language))
                }
                
                out
        })
        

        
        output$municipalities_map <- renderLeaflet({
                r <- NULL
                allowed_municipalities_code <- map_threshold %>% mutate(codicecomune = gsub("^0", "", codicecomune)) %>% filter(esito_unita == 1) %>% select(codicecomune)
                allowed_municipalities_code <- as.integer(allowed_municipalities_code[[1]]) %>% unique(.)
                if (!is.null(data$clickedProvince[[1]])){

                        province_code <- input$province_map_shape_click[["id"]]
                        province_symbol <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
                        
                        print("**** province symbol *****")
                        #print(province_symbol)
                        latitude <- input$province_map_shape_click[["lat"]] 
                        longitude <- input$province_map_shape_click[["lng"]]
                        print(input$province_map_shape_click)
                        
                        sardinian_municipalities <- subset(municipalities, (municipalities$COD_PRO %in% c(province_code)) & (municipalities$PRO_COM %in% allowed_municipalities_code))
                        # print("Radio***")
                        measure = input$measure
                        # print(paste("measure: ", measure))
                        # print("session")
                        # print(session$input[["measure"]])
                        # 
                        measure_selected = get_arrivals_by_municipal_code(aggregate_movements)
                        legend_title = tr("numero_arrivi", change$language)
                        if ((measure == "Presenze") || (measure == "Presences")){
                                print(paste("radio: ", measure))
                                measure_selected = get_presences_by_municipal_code((aggregate_movements))
                                legend_title = tr("numero_presenze", change$language)
                        }
                        print("measure_selected")
                        sardinian_municipalities$measure <- sapply(sardinian_municipalities$PRO_COM, function(x){
                          if(x %in% measure_selected$municipal_code){
                            measure_selected[[2]][measure_selected[[1]] == x]                            
                          }else{
                            0
                          }
                        })
                        
                        print(measure_selected)
                        
                        pal <- colorNumeric("Purples", 
                                            domain = sardinian_municipalities$measure)
                        
                        print(sardinian_municipalities$measure)
                        #~colorQuantile("Purples", SHAPE_Area)(SHAPE_Area)
                        
                        r <- leaflet(sardinian_municipalities)  %>%
                                setView(lng=longitude, lat=latitude, zoom=8) %>%
                                addTiles() %>%
                                addPolygons(color = "#444444", weight = 1, layerId = sardinian_municipalities$PRO_COM, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.8,
                                            fillColor = ~pal(measure),
                                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                bringToFront = TRUE), label = sardinian_municipalities$COMUNE, labelOptions = labelOptions(clickable = FALSE, noHide = TRUE)) %>%
                                             addLegend("bottomright", pal = pal, values = sardinian_municipalities$measure, title = legend_title, opacity = 1)                                        
                }
        })
        
        output$structure_map <- renderLeaflet({
                print("Inside structure ****")
                print("****Clicked Municipality***")
                print(data$clickedMunicipality)
  #################              
                r <- NULL
                allowed_municipalities <- map_threshold %>% mutate(codicecomune = gsub("^0", "", codicecomune)) %>% filter(esito_unita == 1)
                allowed_municipalities_code <- allowed_municipalities %>% select(codicecomune)
                allowed_municipalities_code <- as.integer(allowed_municipalities_code[[1]])
                struct <- structures %>% mutate(cod_com = gsub("^0", "", cod_com))

                # province_code <- input$province_map_shape_click[["id"]]
                # municipal_code <- input$municipalities_map_shape_click[["id"]]
                province_code <- data$clickedProvince[["id"]]
                municipal_code <- data$clickedMunicipality[["id"]]
                
                
                if (!is.null(data$clickedMunicipality[["id"]]) && sameProvince(municipal_code, province_code)){
                        province_symbol <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
                        
                        print("**** municipal parameters *****")
                        #print(province_symbol)
                        # latitude <- input$municipalities_map_shape_click[["lat"]] 
                        # longitude <- input$municipalities_map_shape_click[["lng"]]
                        latitude <- data$clickedMunicipality[["lat"]] 
                        longitude <- data$clickedMunicipality[["lng"]]
                        
                        municipal_structures <- struct %>% filter(cod_com == data$clickedMunicipality[["id"]])
                        municipal_structures <- municipal_structures %>% filter(!(lat == '' | lon == '')) %>% mutate(lat = as.numeric(lat), .) %>% mutate(lon = as.numeric(lon), .)
                        
                      if(nrow(municipal_structures) != 0){
                        municipal_structures$lat <- round(municipal_structures$lat, 3)
                        municipal_structures$lon <- round(municipal_structures$lon, 3)
                        
                        print(paste("id comune: ", data$clickedMunicipality[["id"]]))
                        
                        print("municipal structures: ")
                        print(municipal_structures$denominazione)
                        
                        m = municipal_structures
                        
                        #sardinian_municipalities <- subset(municipalities, (municipalities$COD_PRO %in% c(province_code)) & (municipalities$PRO_COM %in% allowed_municipalities_code))
                        r <- leaflet(municipal_structures)  %>%
                          setView(lng=longitude, lat=latitude, zoom=8) %>%
                          addTiles() %>%
                          addMarkers(~lon, ~lat, popup = ~build_marker_popup(denominazione, tipologia, classificazione, mesi_apertura, tot_letti, change$language), label = ~as.character(denominazione))
                      }else{
                          r <- leaflet(municipal_structures)  %>%
                          setView(lng=longitude, lat=latitude, zoom=8) %>%
                          addTiles()
                        
                      }
                      r  

                                # addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.3,
                                #             fillColor = ~colorQuantile("Purples", SHAPE_Area)(SHAPE_Area),
                                #             highlightOptions = highlightOptions(color = "white", weight = 2,
                                #                                                 bringToFront = TRUE), label = sardinian_municipalities$COMUNE)
                }
                
             #########   
        })
        
        
        
        
        output$proveniences <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
          
                if (!is.null(input$province_map_shape_click[['id']])){
                  province_code <- input$province_map_shape_click[['id']]
                  province_abbreviation <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
                  print(paste("Province abbreviation: ", province_abbreviation))
                  
                }
                
                if(!(is.null(input$municipalities_map_shape_click[["id"]]))){
                  municipality_code = input$municipalities_map_shape_click[["id"]]
                  if (!sameProvince(municipality_code, province_code)){
                    print(paste("Municipality code", municipality_code))
                    municipality_code <- NULL
                    
                  }
                  
                }
                
                     
                measure = input$measure
                if (is.null(measure) || measure == ""){
                        measure = 'Arrivi'
                }else{
                        measure = input$measure
                }
                print(paste("measure inside proveniences: ", measure))
                proveniences <- get_global_proveniences(aggregate_movements, province_abbreviation, municipality_code, measure)
                if (change$language == "en"){
                        proveniences$provenienza <- translate_vector(proveniences$provenienza, change$language)
                }
                ### margins ##
                m <- list(
                        pad = 4
                )
                d <- event_data("plotly_click", source = 'prov_pie' )
                print("Click event")
                print(d)
                print(class(d))
                if (!is.null(d)){
                        print("point number")
                        print(d[["pointNumber"]])
                        
                }
                plot_title <- tr("distribuzione_provenienza", change$language)
                
                p <- plot_ly(proveniences, labels = ~provenienza, values = ~movimenti, type = 'pie', textinfo = 'percent', hoverinfo = 'text',
                             text = ~paste(provenienza, ":", movimenti), marker = list(line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE, source = 'prov_pie') %>%
                        layout(title = plot_title, showlegend = T) %>%
                        highlight(
                                  persistent = TRUE)
        })
        
        
        # output$prov_click <- renderPrint({
        #         d <- event_data("plotly_selected")
        #         if (is.null(d)) "Click events appear here (double-click to clear)" else d
        # })
        
        output$prov_by_nation <- renderPlotly({
                province_abbreviation <- NULL
                municipality_code <- NULL
               
                if (!is.null(input$province_map_shape_click[['id']])){
                        province_code <- input$province_map_shape_click[['id']]
                        province_abbreviation <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
                        
                }
                
                if(!(is.null(input$municipalities_map_shape_click[["id"]]))){
                        municipality_code = input$municipalities_map_shape_click[["id"]]
                        if (!sameProvince(municipality_code, province_code)){
                                municipality_code <- NULL
                        }
                        
                }              
          ##################
                provenience_by_nation <- get_provenience_by_nation(aggregate_movements, province_abbreviation, municipality_code)
                plot_title <- tr("distribuzione_per_stato", change$language)
                y_axis_title <- tr("arrivi", change$language)
                
                if (change$language == "en"){
                        provenience_by_nation$nazione <- translate_vector(provenience_by_nation$nazione, change$language)
                }
                
                
                #prov_by_nation$nazione = factor(x = prov_by_nation$nazione, levels = prov_by_nation$nazione)
                ### axis params ###
                f2 <- list(
                  family = "Old Standard TT, serif",
                  size = 14
                )
                a <- list(
                  title = "AXIS TITLE",
                  titlefont = f2,
                  showticklabels = TRUE,
                  tickangle = 45,
                  tickfont = f2,
                  exponentformat = "E"
                )
               m <- list(b=110)
                
                
                
                ###################
                
                xform <- list(categoryorder = "array",
                              categoryarray = provenience_by_nation$nazione, title = "", tickfont = list(size = 9), tickangle = 35)
                
                
                
                p <- plot_ly(data = provenience_by_nation, x = ~nazione, y = ~arrivi, type = 'bar') %>%
                        layout(title = plot_title, xaxis = xform, yaxis = list(title = y_axis_title, tickfont = list(size = 9)), marker = list(color = 'rgb(158,202,225)',
                                line = list(color = 'rgb(8,48,107)', width = 1.5)), margin = m) %>%
                        highlight(
                                  persistent = TRUE,
                                  dynamic = TRUE
                        )
                
                               #yaxis = list(tickfont = list(size = 8)), xaxis = list(title = "Regione di provenienza", tickfont = list(size = 8)))
        })
        
        
        output$prov_by_region <- renderPlotly({
                prov_by_region <- get_provenience_by_region(aggregate_movements)
                plot_title <- tr("distribuzione_per_regione", change$language)
                y_axix_title <- tr("arrivi", change$language)
                
                if (change$language == "en"){
                        prov_by_region$regione <- translate_vector(prov_by_region$regione, change$language)
                }
                
                
                
                xform <- list(categoryorder = "array",
                              categoryarray = prov_by_region$regione, title = "", tickfont = list(size = 9), tickangle = 35)
                
                p <- plot_ly(data = prov_by_region, x = ~regione, y = ~arrivi, type = 'bar', marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        layout(title = plot_title, xaxis = xform, yaxis = list(title = y_axix_title, tickfont = list(size = 9)))
                
        })
        
        
        
        
        
        
        output$sex <- renderPlotly({
                sex_distribution <- get_sex(aggregate_web_data)
                
                
                plot_title <- tr("distribuzione_per_sesso", change$language)
                p <- plot_ly(sex_distribution, labels = ~sesso, values = ~arrivi, type = 'pie', textinfo = 'percent', hoverinfo = 'text',
                             text = ~paste(sesso, ":", arrivi), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
                        layout(title = plot_title, showlegend = T) 
                
        })
        
        
        output$accomodated_type <- renderPlotly({
                accomodated_type <- get_accomodated_type(aggregate_web_data)
                if (change$language == "en"){
                        accomodated_type$tipo_alloggiato <- translate_vector(accomodated_type$tipo_alloggiato, change$language)
                }
                
                
                xform <- list(categoryorder = "array",
                              categoryarray = accomodated_type$tipo_alloggiato, title = "")
                plot_title <- tr("distribuzione_per_alloggiato", change$language)
                p <- plot_ly(data = accomodated_type, x = ~tipo_alloggiato, y = ~arrivi, type = 'bar', marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        layout(title = plot_title, xaxis = xform, bargap = 0.8, yaxis = list(title = ""))                
                
        })
        
        
        output$age_range <- renderPlotly({
                age_range <- get_age_range(aggregate_web_data)
                plot_title <- tr("distribuzione_per_eta", change$language)
                p <- plot_ly(data = age_range, x = ~eta, y = ~arrivi, type = 'bar', marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        layout(title = plot_title, bargap = 0.8, xaxis = list(title = tr("fascia_eta", change$language)), yaxis = list(title = ""))
        })
        
        
        output$trend_comparison <- renderPlotly({
                trends <- get_last_three_years(aggregate_movements)
                #trend_2014 = filter(trends, anno == 2014) %>% select(., c("mese"))
                last_years <- tail(unique(trends$anno), n = 3)
                
                ### last three years 
                trends1 <- trends[trends$anno == last_years[1], ]
                mesi <- translate_vector(trends1$mese, change$language)
                trends1$mese <- factor(x = mesi, levels = mesi)
                
                trends2 <- trends[trends$anno == last_years[2], ]
                trends2$mese <- factor(x = mesi, levels = mesi)
                
                trends3 <- trends[trends$anno == last_years[3], ]
                
                trends3$mese <- factor(x = mesi[1:nrow(trends3)], levels = mesi[1:nrow(trends3)])
                
                y_axix_title <- tr("arrivi", change$language)
                
                plot_title <- tr("andamento_triennio", change$language)
                p <- plot_ly(trends1, x = ~mese, y = ~arrivi, name = paste(y_axix_title, last_years[1]), type = 'scatter', mode = 'lines+markers') %>%
                        add_trace(data = trends2, x = ~mese, y = ~arrivi, name = paste(y_axix_title, last_years[2]), mode = 'lines+markers') %>%
                        add_trace(data = trends3, x = ~mese, y = ~arrivi, name = paste(y_axix_title, last_years[3]), mode = 'lines+markers') %>%                        
                        layout(title = plot_title,
                               xaxis = list(title = ""),
                               yaxis = list (title = y_axix_title))        
        })
        
        
        # observeEvent(input$province_map_shape_click, { # update the location selectInput on map clicks
        #         p <- input$province_map_shape_click[[1]]
        #         print(p)
        # 
        # })
        
        # points <- eventReactive(input$recalc, {
        #         cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
        # }, ignoreNULL = FALSE)
        # 
        # output$mymap <- renderLeaflet({
        #         leaflet() %>%
        #                 addProviderTiles(providers$Stamen.TonerLite,
        #                                  options = providerTileOptions(noWrap = TRUE)
        #                 ) %>%
        #                 addMarkers(data = points())
        })


  # output$distPlot <- renderPlot({
  # 
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
#  })


