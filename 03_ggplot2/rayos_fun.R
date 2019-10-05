##################################
##################################
# Descarga de Registros de Rayos #
# de Meteogalicia                #
# Autor: Dominic Royé            #
# Email: dominic.roye@gmail.com  #
##################################
##################################
packages <- c("XML","dplyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
### Packages ##
library(XML)
library(dplyr)

raios <- function(datein,datefin){
  
  if((as.Date(datefin,format="%d/%m/%Y")-as.Date(datein,format="%d/%m/%Y"))>7){
    temp1 <- NULL
    if(((as.numeric(as.Date(datefin,format="%d/%m/%Y")-as.Date(datein,format="%d/%m/%Y")))%%7)==0){
      tempseq <- seq(as.Date(datein,format="%d/%m/%Y"),as.Date(datefin,format="%d/%m/%Y"),7)
      tempseq <- as.character(format(tempseq,format="%d/%m/%Y"))
    }else{
      
      tempseq <- seq(as.Date(datein,format="%d/%m/%Y"),as.Date(datefin,format="%d/%m/%Y"),7)
      tempseq <- c(as.character(format(tempseq,format="%d/%m/%Y")),datefin)
    }
    
    
    for(i in 1:(length(tempseq)-1)){
      urlp <- "http://servizos.meteogalicia.es/rss/observacion/rssRaios.action?request_locale=gl&dataIni="
      url1 <- "&dataFin="
      
      url <- paste(urlp,tempseq[i],url1,tempseq[i+1],sep="")
      
      cfaincidents <- xmlInternalTreeParse(url)
      
      raiostotal <- as.numeric(sapply(getNodeSet(cfaincidents, "//Raios:total"),xmlValue))
      
      if(all(raiostotal==0)) next
      
      if(any(raiostotal==0)){
        raiosdata <- sapply(getNodeSet(cfaincidents, "//Raios:data"),xmlValue)
        raiosdata <- raiosdata[-which(raiostotal==0)]
      }else{
        
        raiosdata <- sapply(getNodeSet(cfaincidents, "//Raios:data"),xmlValue)
      }
      
      temp <- as.data.frame(t(sapply(getNodeSet(cfaincidents, "//Raios:raio"), xmlAttrs)))
      
      temp[,"day"] <- rep(1:length(raiostotal),raiostotal)
      
      temp <- split(temp,temp$day)
      
      names(temp) <- raiosdata
      
      temp <- as.data.frame(do.call(rbind,temp),stringsAsFactors=FALSE)
      temp[,"Date"] <- as.Date(row.names(temp),format="%d/%m/%Y")
      row.names(temp) <- NULL
      
      temp1 <- rbind(temp1,temp)
      
    }
    
    return(temp1)
    
  }else{
    
    
    urlp <- "http://servizos.meteogalicia.es/rss/observacion/rssRaios.action?request_locale=gl&dataIni="
    url1 <- "&dataFin="
    
    url <- paste(urlp,datein,url1,datefin,sep="")
    
    cfaincidents <- xmlInternalTreeParse(url)
    
    raiostotal <- as.numeric(sapply(getNodeSet(cfaincidents, "//Raios:total"),xmlValue))
    
    if(any(raiostotal==0)){
      which(raiostotal==0)
      raiosdata <- sapply(getNodeSet(cfaincidents, "//Raios:data"),xmlValue)
      raiosdata <- raiosdata[-which(raiostotal==0)]
    }else{
      
      raiosdata <- sapply(getNodeSet(cfaincidents, "//Raios:data"),xmlValue)
    }
    
    temp <- as.data.frame(t(sapply(getNodeSet(cfaincidents, "//Raios:raio"), xmlAttrs)))
    
    temp[,"day"] <- rep(1:length(raiostotal),raiostotal)
    
    temp <- split(temp,temp$day)
    
    names(temp) <- raiosdata
    
    temp <- as.data.frame(do.call(rbind,temp),stringsAsFactors=FALSE)
    temp[,"Date"] <- as.Date(row.names(temp),format="%d/%m/%Y")
    row.names(temp) <- NULL
    temp[,c(1,3:6)] <- as.data.frame(apply(temp[,c(1,3:6)],2,as.numeric),stringsAsFactors=FALSE)
    temp <- temp[,-6]
    return(temp)
  }
}

