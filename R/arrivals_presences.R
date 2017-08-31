get_arrivals <- function(dataset){
        ### 2016 Arrivals
        data <- dataset[dataset$anno_rif == 2016, ]
        arrivals <- aggregate(data$tot_arrivi ~ data$provincia, FUN = sum)
        names(arrivals) <- c("province", "tot_arrivals")
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

get_last_three_years <- function(dataset){
        mapping <- unique(cbind(dataset$mese, dataset$mesestr_ita))
        mapping_list <- mapping[,2]
        names(mapping_list) <- mapping[,1]
        res <- aggregate(dataset$tot_arrivi ~ dataset$mese + dataset$anno_rif, FUN = sum)
        names(res) <- c("mese", "anno", "arrivi")
        
        out <- res %>% mutate(mese = mapping_list[mese])
        # line_dataset = data.frame(matrix(nrow = 12, ncol = 4))
        # line_dataset[,1] = c("Gennaio", "Febbraio", "Marzo")
        # 
        # last_years <- tail(unique(out$anno), n = 3)
        # for (y in last_years){
        #         arr = out[out$anno == y, 3]
        #         
        #         
        #         
        # }
        # 
        # months <- filter(res, anno == 2015) %>% select(., mese) %
        
        out
}