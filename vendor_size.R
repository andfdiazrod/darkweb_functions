vendor_size <- function(df){
  mean_weight_by_vendor <- info_total %>% filter(weight_in_grams < 1e6 & 
                          is.na(is_crack) & 
                          sample == F & combos == 0 & 
                          weight_description == 0) %>%
    group_by(vendor_name) %>% 
    summarise(mean_weight=mean(weight_in_grams, na.rm=T))
  
  mean_weight_by_vendor <- mean_weight_by_vendor %>% 
    mutate(vendor_size=ifelse(mean_weight <= 10, 'small',
                              ifelse(mean_weight <= 100, 'medium',' large')))
  
  df_vendor_size <- into_total <- left_join(info_total, a, by='vendor_name')
  
  print(table(mean_weight_by_vendor$vendor_size))
  return(df_vendor_size)
}

if(FALSE){
a <- a %>% mutate(vendor_size=ifelse(mean_weight <= 10, 'small',
                                ifelse(mean_weight <= 100, 'medium',' large')))
into_total <- left_join(info_total, a, by='vendor_name')
}