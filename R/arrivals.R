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