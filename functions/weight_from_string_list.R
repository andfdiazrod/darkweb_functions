weight_from_string_list <- function(string_list){
  
  weight_words_1 <- c('gr','g.','g ','gs','gz','g','gm','gram','grams','gramme','oz','ounce','ounces','mg', 'kg','kilo')
  conversion <- c(1,1,1,1,1,1,1,1,1,1,28.3495,28.3495,28.3495,1/1000,1000,1000)[order(nchar(weight_words_1),decreasing = TRUE)]
  weight_words_1 <- weight_words_1[order(nchar(weight_words_1),decreasing = TRUE)]
  weight_words_sorted <- paste(weight_words_1,collapse = '|')
  
  weight_in_grams <- matrix(ncol=3)
  for(str in string_list){
    
    position_unit_cut <- c(1,1)
    
    str <- tolower(str)
    
    str <- sub('half','0.5',str)
    str <- sub('full','1',str)
    str <- sub('deux','2',str)
    str <- sub('1 one','1',str)
    
    replace_1 <- '[0-9]+\\s+[0-9]+'
    find_replace <- strsplit(str_extract_all(str,replace_1)[[1]],' ')
    for(num_temp in find_replace){
      if(length(num_temp)==2 & 
         (sum(as.numeric(num_temp)<50)==2 |
          0 %in% num_temp)){
        str_temp_1 <- paste0(num_temp,collapse='.')
        str <- sub(str_temp_1,paste0(' ',str_temp_1),str)
      } 
    }
    
    weight_not_found <- TRUE
    while(weight_not_found){
      str <- substr(str,position_unit_cut[2],nchar(str))
      unit <- str_extract(str, weight_words_sorted)
      position_unit_cut <- str_locate(str,unit)
      if(!is.na(unit)){
        pattern <- paste0("(\\S+)\\s*",unit)  
        match   <- regexec(pattern, str)
        adjacent_words <- unlist(regmatches(str, match))[-1]
        weight <- na.omit(suppressWarnings(as.numeric(adjacent_words)))
        if(length(weight) != 0 & unit %in% weight_words_1){
          if(unit %in% weight_words_1){
            weight_grams <- weight[1] * conversion[which(unit == weight_words_1)]  
          } else {
            weight_grams <- 99999999
            print(str)
          }
          weight_in_grams <- rbind(weight_in_grams, c(weight[1], unit, weight_grams))   
          weight_not_found <- FALSE
        } else if(nchar(str)==1){
          weight_in_grams <- rbind(weight_in_grams,NA) 
          weight_not_found <- FALSE
        }
      } else {
        weight_in_grams <- rbind(weight_in_grams,NA) 
        weight_not_found <- FALSE
      }
    }
  }
  
  weight_in_grams <- weight_in_grams[-1,]
  colnames(weight_in_grams) <- c('weight', 'unit','weight_in_grams')
  weight_in_grams[,c("weight","weight_in_grams")] <- as.numeric(weight_in_grams[,c("weight","weight_in_grams")])
  
  return(data.frame(weight_in_grams,stringsAsFactors = FALSE))
}