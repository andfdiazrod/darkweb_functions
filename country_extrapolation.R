country_extrapolation <- function(df){
  
  df <- df[order(df$vendor_name,df$day_format),]
  country_extrap <- c()
  for(vn in unique(df$vendor_name)){
    na <- is.na(df$country[which(df$vendor_name==vn)])*(1:length(df$country[which(df$vendor_name==vn)]))
    non_na <- (!is.na(df$country[which(df$vendor_name==vn)]))*(1:length(df$country[which(df$vendor_name==vn)]))
    
    if(sum(non_na) > 0 & sum(na) > 0){
      filled_country <- df$country[which(df$vendor_name==vn)]
      filled_country[na[na>0]] <- filled_country[unlist(sapply(na,function(x) max_less_than_x(x)))]
      country_by_vendor <- filled_country  
    } else {
      country_by_vendor <- df$country[which(df$vendor_name==vn)]
    }
    country_extrap <- c(country_extrap, country_by_vendor)
  }
  return(cbind(df,country_extrap))
}

