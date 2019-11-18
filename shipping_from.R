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