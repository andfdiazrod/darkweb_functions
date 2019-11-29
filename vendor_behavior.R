vendor_behavior <- function(df){
  
  columns = c("day_format", 'vendor_name',"in_sample","appears", "consistency",
            "relative_consistency")
  vendor_consistency <- data.frame(matrix(ncol=length(columns)))
  colnames(vendor_consistency) <- columns
  
  sum_up_to <- function(x){
    return(unlist(lapply(1:length(x),function(y) sum(x[1:y]))))
  }
  
  for(vn in unique(df$vendor_name)){
    vn_day <- data.frame(day_format = unique(as.Date(info_total %>% 
                                                       filter(vendor_name == vn) %>% pull(day_format))),
                         vendor_name=vn, appears = 1)
    vn_day_range <- read.csv('time_series.csv', stringsAsFactors = F)
    vn_day_range$day_format <- as.Date(vn_day_range$day_format)
    vn_day_range <- vn_day_range %>% 
      filter(day_format %in% seq.Date(min(vn_day[,1]),
                                      max(vn_day[,1]),1))
    vn_day_range <- na.omit(left_join(vn_day_range, vn_day, by='day_format') %>% 
                              replace_na(list(appears = -1)))
  
    vn_day_range$consistency <- sum_up_to(vn_day_range$appears)
    vn_day_range$relative_consistency <- vn_day_range$consistency/(1:nrow(vn_day_range)) 
    
    vendor_consistency <- rbind(vendor_consistency, vn_day_range)
  }
  return(vendor_consistency)
}

a = vendor_consistency %>% group_by(day_format) %>%  
  summarise(a = mean(relative_consistency, na.rm=T))
plot(a, type='l')
