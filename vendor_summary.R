
vendor_summary<-function(df)
{
  df$day<-as.Date(as.character(df$day), format="%Y%m%d")
  summary_cocaine<-df %>% filter(cocaine>=4 & not_cocaine<=1 & combos==0 & sample==F) %>% group_by(vendor_name) %>% 
    summarise(Dia_entrada=min(day), Dia_salida=max(day),
              min_price_per_gram=min(na.omit(price_in_bit)), max_price_per_gram=max(na.omit(price_in_bit)),
              min_quantity=min(na.omit(weight_in_grams)), max_quantity=max(na.omit(weight_in_grams)),
              n_listing=length(unique(listing_low)), n_days=length(unique(day)))
  
  summary_crack<-df %>% filter(is_crack>=4 & not_crack<=1) %>% group_by(vendor_name) %>% 
    summarise(Dia_entrada=min(day), Dia_salida=max(day),
              min_price_per_gram=min(na.omit(price_in_bit)), max_price_per_gram=max(na.omit(price_in_bit)),
              min_quantity=min(na.omit(weight_in_grams)), max_quantity=max(na.omit(weight_in_grams)),
              n_listings=length(unique(listing_low)),n_days=length(unique(day)))
  
  ret<-list(summary_cocaine, summary_crack)
  return(ret)
  
}

