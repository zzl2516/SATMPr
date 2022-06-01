tax.summary <- function(data){
    data <- data[,c(group$variable,"taxonomy")]
    data <- data[data$taxonomy != "Unculture",]
    group <- group[group$variable %in% colnames(data),]
    if (grepl("; ",data$taxonomy)) {
        taxonomy <- data %>% 
            separate(taxonomy,c("Domain","Phylum","Class","Order","Family","Genus","Species"),"; ")
    }else{
        taxonomy <- data %>% 
            separate(taxonomy,c("Domain","Phylum","Class","Order","Family","Genus","Species"),";")
        
    }
    taxonomy$Phylum <- gsub("p__","",taxonomy$Phylum)
    taxonomy$Class <- gsub("c__","",taxonomy$Class)
    taxonomy$Order <- gsub("o__","",taxonomy$Order)
    taxonomy$Family <- gsub("f__","",taxonomy$Family)
    taxonomy$Genus <- gsub("g__","",taxonomy$Genus)
    taxonomy$Species <- gsub("s__","",taxonomy$Species)
    data <- data[,colnames(data) %in% group$variable]
    data <- as.data.frame(cbind(data,taxonomy[,(ncol(taxonomy)-6):ncol(taxonomy)]))
    data <- data[data$Domain != "Unassignable",]
    data <- data[data$Domain != "Unclassified",]
    data <- subset(data,select = -Domain)
    data <- data[complete.cases(data$Phylum),]
    data <- data[rowSums(data[,1:(ncol(data)-6)]) > 1,]
    data1 <- t(data[,1:(ncol(data)-6)])
    data2 <- data[,1:(ncol(data)-6)]
    data2 <- t(t(data2)/colSums(data2)*100)
    data3 <- as.data.frame(cbind(data2,data[,(ncol(data)-5):ncol(data)]))
    for (i in (ncol(data3)-5):ncol(data3)) {
        data3[grepl("norank",data3[,i]),i] <- "Unclassified"
        data3[grepl("uncultured",data3[,i]),i] <- "Unclassified"
        data3[grepl("unclassified",data3[,i]),i] <- "Unclassified"
        data3[grepl("unidentified",data3[,i]),i] <- "Unclassified"
        data3[grepl("metagenome",data3[,i]),i] <- "Unclassified"
        data3[is.na(data3[,i]),i] <- "Unclassified"
        data3[,i] <- gsub("\\[","",data3[,i])
        data3[,i] <- gsub("\\]","",data3[,i])
    }
    
    data <- data3
    Phylum <- aggregate(data[,1:(ncol(data)-6)],
                             list(data$Phylum),sum)
    colnames(Phylum)[1] <- "Phylum"
    Class <- aggregate(data[,1:(ncol(data)-6)],
                        list(data$Class),sum)
    colnames(Class)[1] <- "Class"
    Order <- aggregate(data[,1:(ncol(data)-6)],
                        list(data$Order),sum)
    colnames(Order)[1] <- "Order"
    Family <- aggregate(data[,1:(ncol(data)-6)],
                        list(data$Family),sum)
    colnames(Family)[1] <- "Family"
    Genus <- aggregate(data[,1:(ncol(data)-6)],
                        list(data$Genus),sum)
    colnames(Genus)[1] <- "Genus"
    Species <- aggregate(data[,1:(ncol(data)-6)],
                        list(data$Species),sum)
    colnames(Species)[1] <- "Species"
    result <- list(Phylum,Class,Order,Family,Genus,Species)
    return(result)
}