rm(list=ls())
setwd(choose.dir())
list.files()

join_df<-function()
{
  
  
  names<-list.files("bases_transformadas/",pattern = "info_")
  df<-NULL
  for(name in names)
  {
    print(name)
    temp<-read.csv(paste0("bases_transformadas/",name), stringsAsFactors = F)
    df<-rbind(df,temp)
  }
  
  return(df)
}

df<-join_df()

library(RecordLinkage)
library(countrycode)

str <- unique(df$ship_from)[140]
shipping_from <- function(ship_from)
{
  country_list <- data.frame(matrix(nrow=length(ship_from)))
  
  for(str in unique(ship_from))
  {
    
    cont <- which(ship_from == str)
    if(grepl('title=',str))
    {
      str <- unlist(str_extract_all(str, pattern = "(?<=title=').+(?='>)"))
    }
    str<-unlist(str_split(str,'-|,|and|&|/'))
    
    for(s in str)
    {
      if(grepl('usa', s))
      {
        s <- gsub('usa','united states', s)
      }
      if(grepl('eu', s))
      {
        s <- gsub('eu','european union', s) 
      }
      if(s == '')
      {
        s <- 'NA'
      }
      country_list[,s] <- 0
      country_list[cont,s] <- 1 
    }
    
  }  
  return(country_list)
}

x<-shipping_from(df$ship_from)

str_extract_all(unique(df$ship_from)[135], pattern = "(?<=title=').*(?='>)")

(?<=[[Pp]urity|[Qq]uality|PURITY|QUALITY|[Pp]ure|grade]{5})[0-9][0-9]  
