get_global_proveniences <- function(dataset, province_abbreviation, municipality_code, measure){
  
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        # italians <- dataset %>% filter(grepl("^9", codicenazione)) %>% filter(!grepl("^999", codicenazione)) %>% filter(periodo == "anno1") 
        # foreigners <- dataset %>% filter(!grepl("^9", codicenazione)) %>% filter(periodo == "anno1")

        italians <- dataset %>% filter(grepl("^999$", codicenazione)) %>% filter(periodo == "anno1")
        foreigners <- dataset %>% filter(grepl("9999", codicenazione)) %>% filter(periodo == "anno1")
        
        italians$descrizione = "Italia"
        foreigners$descrizione = "Estero"
        d <- rbind(italians, foreigners)
        
       
        proveniences <- tryCatch({
                aggregate(d$tot_arrivi ~ d$descrizione, FUN = sum)},
                finally = {data.frame(matrix(nrow = 1, ncol = 2))})                
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

get_current_coverage <- function(dataset, province_abbreviation = NULL, municipality_code = NULL){

  #current_month = select(filter(dataset, periodo == "anno1" & provincia == '' & anno_rif == max(anno_rif) & mese == max(mese)), c("mesestr_ita"))
   if (is.null(province_abbreviation)){
     dataset <- filter(dataset, provincia == '')
   }else if (!is.null(province_abbreviation)){
     if(is.null(municipality_code)){
       dataset <- filter(dataset, provincia == province_abbreviation & codicecomune == '')
     }else{
       if (substring(municipality_code, 1, 1) == "9"){
         municipality_code = paste("0", municipality_code, sep = '')
       }
       dataset <- filter(dataset, provincia == province_abbreviation & codicecomune == municipality_code)
     }
   }

  current_coverage <- filter(dataset, periodo == "anno1" & anno_rif == max(anno_rif)) %>% filter(mese == max(mese)) %>% select(c("mesestr_ita", "copertura"))
  # current_coverage = select(filter(dataset, periodo == 'anno1' & anno_rif == max(anno_rif) & mese == max(mese)), c("mesestr_ita", "copertura"))    

  names(current_coverage) <- c("mese", "copertura")
  if(nrow(current_coverage) == 0){
    current_month <- filter(dataset, periodo == "anno1" & anno_rif == max(anno_rif)) %>% filter(provincia == '' & mese == max(mese)) %>% select(c("mesestr_ita")) 
    current_coverage[1,1] = current_month
    current_coverage[1,2] = ""
  }
  current_coverage
  
}


get_coverage <- function(dataset, province_abbreviation = NULL, municipality_code = NULL){
  if (is.null(province_abbreviation)){
    dataset <- filter(dataset, periodo == "anno1" & provincia == '') %>% arrange(desc(anno_rif), desc(mese))
  }else{
    if (is.null(municipality_code)){
      dataset <- filter(dataset, periodo == "anno1" & provincia == province_abbreviation & codicecomune == '') %>% arrange(desc(anno_rif), desc(mese))
    }else{
      if (substring(municipality_code, 1, 1) == "9"){
        municipality_code = paste("0", municipality_code, sep = '')
      }
      dataset <- filter(dataset, periodo == "anno1" & provincia == province_abbreviation & codicecomune == municipality_code) %>% arrange(desc(anno_rif), desc(mese))
      
    }
  }
  coverage <- select(dataset, c("anno_rif", "mesestr_ita", "copertura"))
  if(nrow(coverage) != 0){
    coverage$copertura <- lapply(coverage$copertura, FUN = function(x) {if (!is.na(x)){paste(x*100, "%", sep = '')}else{"-"}})
  }else{
    coverage[1,1] = ""
    coverage[1,2] = ""
    coverage[1,3] = ""
  }
  names(coverage) = c("anno", "mese", "copertura")
  coverage
}

get_provenience_by_nation <- function(dataset, province_abbreviation, municipality_code, measure){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        foreigners <- dataset %>% filter(!grepl("^9", codicenazione)) %>% filter(periodo == "anno1")
        proveniences <- tryCatch({
                aggregate(foreigners$tot_arrivi ~ foreigners$descrizione, FUN = sum)},
                finally = {data.frame(matrix(nrow = 1, ncol = 2))})
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
        italians_all <- dataset %>% filter(grepl("^9", codicenazione)) %>% filter(periodo == "anno1")
        italians <- italians_all %>% filter(!grepl("Italia", descrizione)) %>% filter(!grepl("Estero", descrizione))
        proveniences <- tryCatch({
                aggregate(italians$tot_arrivi ~ italians$descrizione, FUN = sum)},
                finally = {data.frame(matrix(nrow = 1, ncol = 2))})                
        measure = tolower(measure)
        if (!is.null(measure) || measure != "") {
          if ((measure == 'presenze') || (measure == "presences")){
            proveniences <- aggregate(italians$tot_presenze ~ italians$descrizione, FUN = sum)
          }
        }
        names(proveniences) = c("regione", "movimenti")
        proveniences <- proveniences[order(proveniences$movimenti, decreasing = T), ]
        
}