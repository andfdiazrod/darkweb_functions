listings_time_series<-function(df)
{
  list_cocaine<-list()
  a<-1
  df_1<-df %>% filter(cocaine>=4 & not_cocaine<=1 & combos==0 & sample==F)
  
  for (vendor in unique(df_1$vendor_name)) {
    df_1_temp<-df %>% filter(vendor_name==vendor)
    for (listing in unique(df_1_temp$listing_low)) {
      temp <- df %>% filter(listing_low==listing) %>% select(day,listing_low, price_in_bit, price_in_dollar, purity, Country, ship_from, uncut)
      list_cocaine[[a]]<-temp
      a<-a+1
      
    }
  }
  
  
  list_crack<-list()
  a<-1
  df_1<-df %>% filter(is_crack>=4 & not_crack<=1)
  
  for (vendor in unique(df_1$vendor_name)) {
    df_1_temp<-df %>% filter(vendor_name==vendor)
      for (listing in unique(df_1_temp$listing_low)) {
        temp <- df %>% filter(listing_low==listing) %>% select(day,listing_low, price_in_bit, price_in_dollar,price_in_dollar, purity, Country, ship_from, uncut)
        list_crack[[a]]<-temp
        a<-a+1
        
      }
  }
  
  return(list(list_cocaine, list_crack))
}

