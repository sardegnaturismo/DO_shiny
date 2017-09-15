require(dplyr)
get_sex <- function(dataset, province_abbreviation, municipality_code){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        sex <- aggregate(dataset$tot_arrivi ~ dataset$sesso_str, FUN = sum)
        names(sex) <- c("sesso", "movimenti")
        sex <- sex %>% filter(grepl("[F|M]", sesso))

}

get_accomodated_type <- function(dataset, province_abbreviation, municipality_code){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        accomodated_type <- aggregate(dataset$tot_arrivi ~ dataset$tipoalloggiato_str, FUN = sum)
        names(accomodated_type) <- c("tipo_alloggiato", "arrivi")
        accomodated_type <- accomodated_type %>% filter(!(tipo_alloggiato == ''))
        accomodated_type = accomodated_type[order(accomodated_type$arrivi, decreasing = T), ]
 
}

get_age_range <- function(dataset, province_abbreviation, municipality_code){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)        
        dd <- dataset  %>% 
                filter(!(fasciaeta == "ND"))
        age_range <- aggregate(dd$tot_arrivi ~ dd$fasciaeta, FUN = sum)
        names(age_range) = c("fasciaeta", "arrivi")
        result <- age_range %>% mutate(fasciaeta = gsub("eta_", "", fasciaeta) %>% gsub("65\\+", ">65", .))
        result$fasciaeta = factor(x = result$fasciaeta, levels = result$fasciaeta)
        names(result) <- c("eta", "arrivi")
        result
   
}