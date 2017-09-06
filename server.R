
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
source("R/arrivals_presences.R")
source("R/proveniences.R")
source("R/profiling.R")
source("R/utilities.R")
#source("R/proveniences")

provinces <- readOGR("shapes/Prov2016_rprj.shp", encoding = "UTF-8")
sardinian_provinces <- subset(provinces, provinces$PROVINCIA %in% c("Sassari", "Nuoro", "Cagliari", "Oristano", "Olbia-Tempio", "Ogliastra",
                                                                    "Medio Campidano", "Carbonia-Iglesias"))
municipalities <- readOGR("shapes/Com2016_rprj.shp")
aggregate_movements <- fread("data/agg_ope_line_20xx.csv")
aggregate_web_data <- fread("data/agg_ope_web.csv")
map_threshold <- fread("data/soglia_map_prov_com_ope.csv")
structures <- fread("data/struttura_info_ope.csv")


shinyServer(function(input, output, session) {
        
        
        
        output$province_map <- renderLeaflet({
                
                print("Radio***")
                measure = input$measure
                #### define province levels
                # provinces <- as.character(sardinian_provinces$PROVINCIA)
                # provinces <- factor(provinces, levels = provinces)
                measure_selected = get_arrivals(aggregate_movements)
                if (measure == "Presenze"){
                        measure_selected = get_presences((aggregate_movements))
                }
                 
                print(measure_selected)
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
                        
                        addLegend("bottomright", pal = pal, values = sardinian_provinces$measure, title = "Numero di arrivi", opacity = 1)
              m        
                
        })
        
        
        output$map_bar <- renderText({
                
                selected_area = "Sardegna"
                out <- NULL
                if (!is.null(input$province_map_shape_click[[1]])){
                        prov_code = input$province_map_shape_click[[1]]
                        selected_area =  as.character(sardinian_provinces$PROVINCIA[sardinian_provinces$COD_PRO == prov_code])      
                }
                if(selected_area == "Sardegna"){
                        out <- paste("Visualizza i dati turistici dell'intera <strong>", selected_area, "</strong> scorrendo in basso la pagina oppure seleziona una <strong>provincia</strong> nella mappa")
                        
                }else{
                        out <- paste("Visualizza i dati turistici della provincia <strong>", selected_area, "</strong> scorrendo in basso la pagina oppure seleziona un <strong>comune</strong> nella mappa")
                }
                out
                
        })
        output$municipalities_map <- renderLeaflet({
                r <- NULL
                allowed_municipalities_code <- map_threshold %>% mutate(codicecomune = gsub("^0", "", codicecomune)) %>% filter(esito_unita == 1) %>% select(codicecomune)
                allowed_municipalities_code <- as.integer(allowed_municipalities_code[[1]]) %>% unique(.)
                if (!is.null(input$province_map_shape_click[[1]])){

                        province_code <- input$province_map_shape_click[["id"]]
                        province_symbol <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
                        
                        print("**** province symbol *****")
                        #print(province_symbol)
                        latitude <- input$province_map_shape_click[["lat"]] 
                        longitude <- input$province_map_shape_click[["lng"]]
                        print(input$province_map_shape_click)
                        
                        sardinian_municipalities <- subset(municipalities, (municipalities$COD_PRO %in% c(province_code)) & (municipalities$PRO_COM %in% allowed_municipalities_code))
                        r <- leaflet(sardinian_municipalities)  %>%
                                setView(lng=longitude, lat=latitude, zoom=8) %>%
                                addTiles() %>%
                                addPolygons(color = "#444444", weight = 1, layerId = sardinian_municipalities$PRO_COM, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.8,
                                            fillColor = ~colorQuantile("Purples", SHAPE_Area)(SHAPE_Area),
                                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                bringToFront = TRUE), label = sardinian_municipalities$COMUNE, labelOptions = labelOptions(clickable = FALSE, noHide = TRUE))
                }
        })
        
        output$structure_map <- renderLeaflet({
                print("Inside structure ****")
                print(input$municipalities_map_shape_click)
  #################              
                r <- NULL
                allowed_municipalities <- map_threshold %>% mutate(codicecomune = gsub("^0", "", codicecomune)) %>% filter(esito_unita == 1)
                allowed_municipalities_code <- allowed_municipalities %>% select(codicecomune)
                allowed_municipalities_code <- as.integer(allowed_municipalities_code[[1]])
                struct <- structures %>% mutate(cod_com = gsub("^0", "", cod_com))

                province_code <- input$province_map_shape_click[["id"]]
                municipal_code <- input$municipalities_map_shape_click[["id"]]
                
                
                if (!is.null(input$municipalities_map_shape_click[["id"]]) && sameProvince(municipal_code, province_code)){
                        province_symbol <- sardinian_provinces$SIGLA[sardinian_provinces$COD_PRO == province_code]
                        
                        print("**** municipal parameters *****")
                        #print(province_symbol)
                        latitude <- input$municipalities_map_shape_click[["lat"]] 
                        longitude <- input$municipalities_map_shape_click[["lng"]]
                        municipal_structures <- struct %>% filter(cod_com == input$municipalities_map_shape_click[["id"]])
                        municipal_structures <- municipal_structures %>% filter(!(lat == '' | lon == '')) %>% mutate(lat = as.numeric(lat), .) %>% mutate(lon = as.numeric(lon), .)
                        
                      if(nrow(municipal_structures) != 0){
                        municipal_structures$lat <- round(municipal_structures$lat, 3)
                        municipal_structures$lon <- round(municipal_structures$lon, 3)
                        
                        print(paste("id comune: ", input$municipalities_map_shape_click[["id"]]))
                        
                        print("municipal structures: ")
                        print(municipal_structures$denominazione)
                        
                        m = municipal_structures
                        
                        #sardinian_municipalities <- subset(municipalities, (municipalities$COD_PRO %in% c(province_code)) & (municipalities$PRO_COM %in% allowed_municipalities_code))
                        r <- leaflet(municipal_structures)  %>%
                          setView(lng=longitude, lat=latitude, zoom=8) %>%
                          addTiles() %>%
                          addMarkers(~lon, ~lat, popup = ~build_marker_popup(denominazione, tipologia, classificazione, mesi_apertura, tot_letti), label = ~as.character(denominazione))
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
                  
                }
                
                if(!(is.null(input$municipalities_map_shape_click[["id"]]))){
                  municipality_code = input$municipalities_map_shape_click[["id"]]
                  if (!sameProvince(municipality_code, province_code)){
                    print("Test not passed!!!")
                    print(paste("Municipality code", municipality_code))
                    municipality_code <- NULL
                    
                  }
                  
                }
                     
                
                proveniences <- get_global_proveniences(aggregate_movements, province_abbreviation, municipality_code)
                ### margins ##
                m <- list(
                        pad = 4
                )
                
                p <- plot_ly(proveniences, labels = ~provenienza, values = ~arrivi, type = 'pie', textinfo = 'percent', hoverinfo = 'text',
                             text = ~paste(provenienza, ":", arrivi), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
                        layout(title = "Distribuzione per paese di provenienza", showlegend = T) 
        })
        
        
        output$prov_by_nation <- renderPlotly({
              
          
          
          
          
                prov_by_nation <- get_provenience_by_nation(aggregate_movements)
                #prov_by_nation$nazione = factor(x = prov_by_nation$nazione, levels = prov_by_nation$nazione)
                xform <- list(categoryorder = "array",
                              categoryarray = prov_by_nation$nazione)
                
                p <- plot_ly(data = prov_by_nation, x = ~nazione, y = ~arrivi, type = 'bar') %>%
                        layout(title = "Distribuzione per stato estero di provenienza", xaxis = xform, marker = list(color = 'rgb(158,202,225)',
                                                                                                                     line = list(color = 'rgb(8,48,107)', width = 1.5)))
                               #yaxis = list(tickfont = list(size = 8)), xaxis = list(title = "Regione di provenienza", tickfont = list(size = 8)))
        })
        
        
        output$prov_by_region <- renderPlotly({
                prov_by_region <- get_provenience_by_region(aggregate_movements)
                xform <- list(categoryorder = "array",
                              categoryarray = prov_by_region$regione)
                
                p <- plot_ly(data = prov_by_region, x = ~regione, y = ~arrivi, type = 'bar', marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        layout(title = "Distribuzione per regione italiana di provenienza", xaxis = xform)
                
        })
        
        
        
        
        
        
        output$sex <- renderPlotly({
                sex_distribution <- get_sex(aggregate_web_data)
                p <- plot_ly(sex_distribution, labels = ~sesso, values = ~arrivi, type = 'pie', textinfo = 'percent', hoverinfo = 'text',
                             text = ~paste(sesso, ":", arrivi), marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE) %>%
                        layout(title = "Distribuzione per sesso", showlegend = T) 
                
        })
        
        
        output$accomodated_type <- renderPlotly({
                accomodated_type <- get_accomodated_type(aggregate_web_data)
                xform <- list(categoryorder = "array",
                              categoryarray = accomodated_type$tipo_alloggiato)
                
                p <- plot_ly(data = accomodated_type, x = ~tipo_alloggiato, y = ~arrivi, type = 'bar', marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        layout(title = "Distribuzione per tipo alloggiato", xaxis = xform, bargap = 0.8)                
                
        })
        
        
        output$age_range <- renderPlotly({
                age_range <- get_age_range(aggregate_web_data)
                p <- plot_ly(data = age_range, x = ~eta, y = ~arrivi, type = 'bar', marker = list(color = 'rgb(158,202,225)', line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        layout(title = "Distribuzione per fascia di età", bargap = 0.8)
        })
        
        
        output$trend_comparison <- renderPlotly({
                trends <- get_last_three_years(aggregate_movements)
                #trend_2014 = filter(trends, anno == 2014) %>% select(., c("mese"))
                last_years <- tail(unique(trends$anno), n = 3)
                
                ### last three years 
                trends1 <- trends[trends$anno == last_years[1], ]
                trends1$mese <- factor(x = trends1$mese, levels = trends1$mese)
                
                trends2 <- trends[trends$anno == last_years[2], ]
                trends2$mese <- factor(x = trends2$mese, levels = trends2$mese)
                
                trends3 <- trends[trends$anno == last_years[3], ]
                trends3$mese <- factor(x = trends3$mese, levels = trends3$mese)
                
                
                p <- plot_ly(trends1, x = ~mese, y = ~arrivi, name = paste("arrivi", last_years[1]), type = 'scatter', mode = 'lines+markers') %>%
                        add_trace(data = trends2, x = ~mese, y = ~arrivi, name = paste("arrivi", last_years[2]), mode = 'lines+markers') %>%
                        add_trace(data = trends3, x = ~mese, y = ~arrivi, name = paste("arrivi", last_years[3]), mode = 'lines+markers') %>%                        
                        layout(title = "Andamento ultimo triennio",
                               xaxis = list(title = ""),
                               yaxis = list (title = "Arrivi"))        
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


