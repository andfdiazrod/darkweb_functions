continent_from <- function(df){
  continents <-list(c("australia", "OCE"),
                    c("united kingdom", "EU"),
                    c("netherlands", "EU"),
                    c("germany", "EU"),
                    c("united states of america", "NAM"),
                    c("worldwide", "WW"),
                    c("france", "EU"),
                    c("belgium", "EU"),
                    c("europe", "EU"),
                    c("canada", "NAM"),
                    c("spain", "EU"),
                    c("switzerland", "EU"),
                    c("denmark", "EU"),
                    c("brazil", "SAC"),
                    c("czech republic", "EU"),
                    c("poland", "EU"),
                    c("italy", "EU"),
                    c("venezuela", "SAC"),
                    c("bolivia", "SAC"),
                    c("mexico", "NAM"),
                    c("south america", "SAC"),
                    c("china", "AS"),
                    c("finland", "EU"),
                    c("norway", "EU"),
                    c("sweden", "EU"),
                    c("ireland", "EU"),
                    c("argentina", "SAC"),
                    c("dominican republic", "SAC"),
                    c("peru", "SAC"),
                    c("belarus", "EU"),
                    c("new zealand", "OCE"),
                    c("angola", "AFR"),
                    c("colombia", "SAC"),
                    c("guatemala", "SAC"),
                    c("portugal", "EU"),
                    c("united arab emirates", "AS"),
                    c("afghanistan", "AS"),
                    c("cambodia", "AS"),
                    c("cameroon", "AFR"),
                    c("philippines", "AS"),
                    c("NA_", "NA_"),
                    c("bahrain", "AS"),
                    c("estonia", "EU"),
                    c("saint martin", "SAC"),
                    c("netherlands antilles", "SAC"),
                    c("india","AS"),
                    c("maldives","AS"),
                    c("malaysia","AS"),
                    c("hong kong","AS"),
                    c("japan","AS"),
                    c("russia","EU"),
                    c("ukraine","EU"),
                    c("vatican","EU"))
  
  uni_continents <- unique(unlist(lapply(continents, function(x) x[2])))
  continent_list <- data.frame(matrix(0,ncol=length(uni_continents),
                                      nrow=nrow(df)))
  colnames(continent_list) <- unique(uni_continents)
  
  for(c in colnames(df$country_list)){
    pos <- which(df[,'country_list'][,c] == 1)
    pos_cont <- which(unlist(lapply(continents, function(x) x[1] == c)))
    if(length(pos_cont)==0){
      print(c)
    } else {
      cont_temp <- continents[[pos_cont]][2]
      continent_list[pos,cont_temp] <- 1  
    }
  }
  
  df$continent_list <- continent_list
  
  return(df)
}
