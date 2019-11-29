
time_series_price_per_gram<-function(df)
{
  
  
  price_per_gram_webpage<-df %>%  group_by(day_format, webpage) %>% summarise(median_price=median(price_in_bit, na.rm = T))
  price_per_gram_webpage_spread<-price_per_gram_webpage %>% spread(webpage,value = median_price)
   
  list_price_per_gram_webpage<-list(price_per_gram_webpage, price_per_gram_webpage_spread)
  
  df$purity_group<-cut(df$purity,c(70,80,90,100))
  price_per_gram_purity<-df %>% group_by(day_format, purity_group) %>% summarise(median_price=median(price_in_bit, na.rm=T))
  price_per_gram_purity_spread<-price_per_gram_purity %>% spread(purity_group,median_price)
  
  list_price_per_gram_purity<-list(price_per_gram_purity, price_per_gram_purity_spread)
  
  df$uncut<-as.character(df$uncut)
  price_per_gram_uncut<-df%>% group_by(day_format, uncut) %>% summarise(median_price=median(price_in_bit, na.rm=T))
  price_per_gram_uncut_spread<- price_per_gram_uncut %>% spread(uncut, median_price)
  colnames(price_per_gram_uncut_spread)[2:4]<-c("not_uncut", "uncut_1", "uncut_2")
  
  list_price_per_gram_uncut<-list(price_per_gram_uncut, price_per_gram_uncut_spread)
  
  price_per_gram_country_origin<-df %>% group_by(day_format, country_extrap) %>% summarise(median_price= median(price_in_bit, na.rm=T))
  price_per_gram_country_origin_spread<-price_per_gram_country_origin %>% spread(country_extrap,median_price) 
  
  list_price_per_gram_country_origin<-list(price_per_gram_country_origin, price_per_gram_country_origin_spread)

  
  price_per_gram_shipping_from<-as.data.frame(unique(df$day_format))
  colnames(price_per_gram_shipping_from)[1]<-"day_format"
  a<-1
  for(pais in colnames(df$country_list))
  {
   
    
      print(pais)
      temp<- df[df[,"country_list"][,pais]==1,]
      temp<- temp %>% group_by(day_format) %>% summarise(median_price__per_gram_shipping_from= median(price_in_bit, na.rm = T))
    
      colnames(temp)[2]<-paste0("median_price__per_gram_shipping_from","_",pais)
      price_per_gram_shipping_from<-merge(price_per_gram_shipping_from, temp, all.x = T, by = "day_format")

     

  }
  
    america_norte <- c('','','')
    ###Precio Por Gramo Por grupo de paises
  
  price_per_gram_list<-list(list_price_per_gram_webpage, list_price_per_gram_purity,
                            list_price_per_gram_uncut, list_price_per_gram_country_origin, price_per_gram_shipping_from)

  return(price_per_gram_list)
}


