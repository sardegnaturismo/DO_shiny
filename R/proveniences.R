get_global_proveniences <- function(dataset){
        italians <- dataset %>% filter(grepl("^9", codicenazione)) %>% filter(!grepl("^9999", codicenazione)) %>% filter(anno_rif == 2016) 
        foreigners <- dataset %>% filter(!grepl("^9", codicenazione)) %>% filter(anno_rif == 2016)
        
        italians$descrizione = "Italia"
        foreigners$descrizione = "Estero"
        d <- rbind(italians, foreigners)
        proveniences <- aggregate(d$tot_arrivi ~ d$descrizione, FUN = sum)
        names(proveniences) = c("provenienza", "arrivi")
        proveniences
}

get_provenience_by_nation <- function(dataset){
        foreigners <- dataset %>% filter(!grepl("^9", codicenazione))
        proveniences <- aggregate(foreigners$tot_arrivi ~ foreigners$descrizione, FUN = sum)
        names(proveniences) <- c("nazione", "arrivi")
        proveniences <- proveniences[order(proveniences$arrivi, decreasing = T), ]
        

}

get_provenience_by_region <- function(dataset){
        italians_all <- dataset %>% filter(grepl("^9", codicenazione))
        italians <- italians_all %>% filter(!grepl("Italia", descrizione)) %>% filter(!grepl("Estero", descrizione))
        proveniences <- aggregate(italians$tot_arrivi ~ italians$descrizione, FUN = sum)
        names(proveniences) = c("regione", "arrivi")
        proveniences <- proveniences[order(proveniences$arrivi, decreasing = T), ]
        
}