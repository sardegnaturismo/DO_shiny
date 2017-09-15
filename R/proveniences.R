get_global_proveniences <- function(dataset, province_abbreviation, municipality_code, measure){
        # if(!is.null(province_abbreviation)){
        #   dataset <-  filter(dataset, provincia == province_abbreviation)
        #   if (!is.null(municipality_code)){
        #     if (substring(municipality_code, 1, 1) == "9"){
        #       municipality_code = paste("0", municipality_code, sep = '')
        #       print("*** municipality code modified")
        #       print(municipality_code)
        #     }
        #     dataset <- dataset %>% filter(codicecomune == municipality_code)
        #   }
        #   
        # }
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        italians <- dataset %>% filter(grepl("^9", codicenazione)) %>% filter(!grepl("^9999", codicenazione)) %>% filter(anno_rif == 2016) 
        foreigners <- dataset %>% filter(!grepl("^9", codicenazione)) %>% filter(anno_rif == 2016)
        
        italians$descrizione = "Italia"
        foreigners$descrizione = "Estero"
        d <- rbind(italians, foreigners)
        
       
        proveniences <- aggregate(d$tot_arrivi ~ d$descrizione, FUN = sum)
        measure = tolower(measure)
        if (!is.null(measure) || measure != "") {
                if ((measure == 'presenze') || (measure == "presences")){
                        proveniences <- aggregate(d$tot_presenze ~ d$descrizione, FUN = sum)
                }
        }
        names(proveniences) = c("provenienza", "movimenti")
        print(proveniences)
        proveniences
}

get_provenience_by_nation <- function(dataset, province_abbreviation, municipality_code, measure){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        foreigners <- dataset %>% filter(!grepl("^9", codicenazione))
        proveniences <- aggregate(foreigners$tot_arrivi ~ foreigners$descrizione, FUN = sum)
        measure = tolower(measure)
        if (!is.null(measure) || measure != "") {
          if ((measure == 'presenze') || (measure == "presences")){
            proveniences <- aggregate(foreigners$tot_presenze ~ foreigners$descrizione, FUN = sum)
          }
        }
        names(proveniences) = c("nazione", "movimenti")
        proveniences <- proveniences[order(proveniences$movimenti, decreasing = T), ]
        

}

get_provenience_by_region <- function(dataset, province_abbreviation, municipality_code, measure){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)  
        italians_all <- dataset %>% filter(grepl("^9", codicenazione))
        italians <- italians_all %>% filter(!grepl("Italia", descrizione)) %>% filter(!grepl("Estero", descrizione))
        proveniences <- aggregate(italians$tot_arrivi ~ italians$descrizione, FUN = sum)
        measure = tolower(measure)
        if (!is.null(measure) || measure != "") {
          if ((measure == 'presenze') || (measure == "presences")){
            proveniences <- aggregate(italians$tot_presenze ~ italians$descrizione, FUN = sum)
          }
        }
        names(proveniences) = c("regione", "movimenti")
        proveniences <- proveniences[order(proveniences$movimenti, decreasing = T), ]
        
}