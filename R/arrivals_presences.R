get_arrivals <- function(dataset){
        ### 2016 Arrivals
        data <- dataset[dataset$anno_rif == 2016, ]
        arrivals <- aggregate(data$tot_arrivi ~ data$provincia, FUN = sum)
        names(arrivals) <- c("province", "tot_arrivals")
        res <- arrivals[order(arrivals$tot_arrivals, decreasing = T), ]
        res
        
}

get_arrivals_by_municipal_code <- function(dataset){
        ####2016 arrivals###
        data <- dataset[dataset$anno_rif == 2016, ]
        arrivals <- aggregate(data$tot_arrivi ~ data$codicecomune, FUN = sum)
        names(arrivals) <- c("municipal_code", "tot_arrivals")
        arrivals = arrivals[arrivals$municipal_code != "", ]
        arrivals$municipal_code <- arrivals$municipal_code %>% gsub("^0", "", .)
        res <- arrivals[order(arrivals$tot_arrivals, decreasing = T), ]
        res
        
}

get_presences <- function(dataset){
        ### 2016 Arrivals
        data <- dataset[dataset$anno_rif == 2016, ]
        presences <- aggregate(data$tot_presenze ~ data$provincia, FUN = sum)
        names(presences) <- c("province", "tot_presences")
        res <- presences[order(presences$tot_presences, decreasing = T), ]
        res
        
}


get_presences_by_municipal_code <- function(dataset){
        #### 2016####
        data <- dataset[dataset$anno_rif == 2016, ]
        presences <- aggregate(data$tot_presenze ~ data$codicecomune, FUN = sum)
        names(presences) <- c("municipal_code", "tot_presences")
        presences = presences[presences$municipal_code != "", ]
        presences$municipal_code <- presences$municipal_code %>% gsub("^0", "", .)
        res <- presences[order(presences$tot_presences, decreasing = T), ]
        res
        
        
}


get_last_three_years <- function(dataset, province_abbreviation, municipality_code, measure){
        dataset <- filter_dataset(dataset, province_abbreviation, municipality_code)
        mapping <- unique(cbind(dataset$mese, dataset$mesestr_ita))
        mapping_list <- mapping[,2]
        names(mapping_list) <- mapping[,1]
        measure = tolower(measure)
        res <- aggregate(dataset$tot_arrivi ~ dataset$mese + dataset$anno_rif, FUN = sum)
        if (!is.null(measure) || measure != "") {
          if ((measure == 'presenze') || (measure == "presences")){
            res <- aggregate(dataset$tot_presenze ~ dataset$mese + dataset$anno_rif, FUN = sum)
          }
        }
        names(res) <- c("mese", "anno", "movimenti")
        
        out <- res %>% mutate(mese = mapping_list[mese])
        out
}