  find_weight <- function(df){
  weight_grams <- weight_from_string_list(df$listing_low) 
  weight_grams$weight_description <- 0
  if(length(which(is.na(weight_grams$unit))) > 0){
    weight_grams$weight_description[which(is.na(weight_grams$unit))] <- 1
    weight_grams[which(is.na(weight_grams$unit)),c('weight','unit','weight_in_grams')] <- weight_from_string_list(df$description[which(is.na(weight_grams$unit))])
  }
  weight_grams[,c("weight","weight_in_grams")] <- apply(weight_grams[,c("weight","weight_in_grams")],2,as.numeric)
  return(cbind(df, weight_grams))
}

if(FALSE){
  a <- find_weight(df)
  View(cbind(df$name, df$description, a))
  
}
