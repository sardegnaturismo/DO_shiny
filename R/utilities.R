### This function returns True if the municipality is included in province provided

sameProvince <- function(municipality_code, province_code){
  return(substr(municipality_code, 1, nchar(province_code)) == province_code)
  
}

filter_dataset <- function(dataset, province_abbreviation = NULL, municipality_code = NULL){
  if(!is.null(province_abbreviation)){
    dataset <-  filter(dataset, provincia == province_abbreviation)
    if (!is.null(municipality_code)){
      if (substring(municipality_code, 1, 1) == "9"){
        municipality_code = paste("0", municipality_code, sep = '')
      }
      dataset <- dataset %>% filter(codicecomune == municipality_code)
    }
    
  }
  return(dataset)
}


build_marker_popup <- function(denominazione, tipologia, classificazione, mesi_apertura, posti_letto, language){
    
    # tipologia = tr("tipologia", language)
    # classificazione = tr("classificazione", language)
    # mesi_apertura = tr("mesi_apertura", language)
    # posti_letto = tr("posti_letto", language)
    denominazione = paste(tr("denominazione", language), denominazione, "</br>")
    tipologia = paste(tr("tipologia", language), tipologia, "</br>")
    classificazione = paste(tr("classificazione", language), classificazione, "</br>")
    mesi_apertura = paste(tr("mesi_apertura", language), mesi_apertura, "</br>")
    posti_letto = paste(tr("posti_letto", language), posti_letto)
    
    pop_up = paste(denominazione, tipologia, classificazione, mesi_apertura, posti_letto)
    p = paste("<p>", pop_up, "</p>")
}


get_language <- function(language = "it"){
        language
}

language = get_language()

