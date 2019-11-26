shipping_from <- function(df){
  
  ship_from <- df$ship_from
  
  country_list <- data.frame(matrix(nrow=length(ship_from)))
  
  for(str in unique(ship_from)){
    if(nchar(str) <= 50){
      cont <- which(ship_from == str)
      str <- tolower(str)
      
      if(grepl('title=',str)){
        str <- unlist(str_extract_all(str, pattern = "(?<=title=').+(?='>)"))
      }
      str <- trimws(unlist(str_split(str,'-|,| and |&|/| including ')))
      
      for(s in str){
        
        s <- gsub('^usa$|^us$|^u.s.a$|united states',
                  'united states of america', s)
        s <- gsub('^uk$|^u.k.$|^u.k$','united kingdom',s)
        s <- gsub('^eu$|european union|europe union|europe (eu)|
                  western europe','europe', s) 
        s <- gsub('scandinavia','sweden',s)
        s <- gsub('^ww$|^world$|^wide$','worldwide',s)
        s <- gsub('^nl$','netherlands',s)
        s <- gsub('^fra$','france',s)
        s <- gsub('somewhere|including','',s)
        s <- gsub('new ze','new zealand',s)
        s <- gsub('russian federation','russia',s)
        s <- gsub('de \\(read profile','germany',s)
        if(s == ''){s <- 'NA'}
        
        sim_col <- unlist(lapply(colnames(country_list),
                                 function(x) levenshteinSim(s,x)))
        if(sum(sim_col > 0.6) > 0){
          og_country <- colnames(country_list)[which(sim_col == max(sim_col))]
        } else {
          country_names <- tolower(codelist$cow.name)
          country_names <- c(country_names, 'worldwide', 'europe',
                             'south america','saint martin',
                             'netherlands antilles','NA',
                             'hong kong','vatican')
          sim <- unlist(lapply(country_names,
                               function(x) levenshteinSim(s,x)))
          og_country <- country_names[which(sim == max(sim,na.rm=T) & sim > 0.6)]
          country_list[,og_country] <- 0
          if(length(og_country) == 0){
            #print(paste(s, '//', unique(ship_from[cont])))
          }
          
        }
        country_list[cont,og_country] <- 1 
      }
    } else {
      #print(str)
    }
    
  }  
  df$country_list <- country_list[,-1]
  return(df)
}
