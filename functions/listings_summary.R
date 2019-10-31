listings_summary<-function(df)
{
  cocaine_listings_sum<-df %>% filter(cocaine>=4 & not_cocaine<=1 & combos==0 & sample==F) %>% 
    group_by(listing_low) %>% summarise(min_price=min(na.omit(price_in_bit)), max_price=max(na.omit(price_in_bit)),
                                        peso=mean(na.omit(weight_in_grams)), mean_price=mean(na.omit(price_in_bit)),
                                        median_price=median(na.omit(price_in_bit)), var_price=var(na.omit(price_in_bit)),
                                        sd_price=sd(na.omit(price_in_bit)), n=length(price_in_bit), min_day=min(day), max_day=max(day))
  
  
  crack_listings_sum<-df %>% filter(is_crack>=4 & not_crack<=1) %>% 
    group_by(listing_low) %>% summarise(min_price=min(na.omit(price_in_bit)), max_price=max(na.omit(price_in_bit)),
                                        peso=mean(na.omit(weight_in_grams)), mean_price=mean(na.omit(price_in_bit)),
                                        median_price=median(na.omit(price_in_bit)), var_price=var(na.omit(price_in_bit)),
                                        sd_price=sd(na.omit(price_in_bit)), n=length(price_in_bit), min_day=min(day), max_day=max(day))
  
  return(list(cocaine_listings_sum, crack_listings_sum))
  
}


a<-listings_summary(agora)
