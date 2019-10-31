listings_time_series<-function(df)
{
  list_cocaine<-list()
  a<-1
  df_1<-df %>% filter(cocaine>=4 & not_cocaine<=1 & combos==0 & sample==F)
  for (listing in unique(df_1$listing_low)) {
    temp <- df %>% filter(listing_low==listing) %>% select(day,listing_low, price_in_bit, price_in_dollar)
    list_cocaine[[a]]<-temp
    a<-a+1
    
  }
  
  list_crack<-list()
  a<-1
  df_1<-df %>% filter(is_crack>=4 & not_crack<=1)
  for (listing in unique(df_1$listing_low)) {
    temp <- df %>% filter(listing_low==listing) %>% select(day,listing_low, price_in_bit, price_in_dollar)
    list_crack[[a]]<-temp
    a<-a+1
    
  }
  
  return(list(list_cocaine, list_crack))
}

