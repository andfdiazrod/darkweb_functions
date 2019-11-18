library(tidyr)
library(stringr)
price_per_gram<-function(df)
{
  
  
   price_per_gram_webpage<-df %>% group_by(day_format, webpage) %>% summarise(market_median_price=median(price_in_bit, na.rm = T))
   price_per_gram_webpage<-price_per_gram %>% spread(webpage,value = market_median_price)
   
  df$purity_group<-cut(df$purity,c(50,60,70,80,90,100))
  price_per_gram_purity<-df %>% group_by(day_format, purity_group) %>% summarise(purity_group_median_price=median(price_in_bit, na.rm=T))
  price_per_gram_purity<-price_per_gram_purity %>% spread(purity_group,purity_group_median_price)
  
  
  price_per_gram_country_origin<-df %>% group_by(day_format, country_extrap) %>% summarise(median_price_country_origin= median(price_in_bit, na.rm=T))
  price_per_gram_country_origin<-price_per_gram_country_origin %>% spread(country_extrap,median_price_country_origin) 
  
  colnames(df)[startsWith(colnames(df), prefix = "country_list.")]
  
  price_per_gram_shipping_from<-list()
  a<-1
  for(pais in colnames(df)[startsWith(colnames(df), prefix = "country_list.")])
  {
  
    print(pais)
    temp<- df[df[,pais]==1,]
    temp <- temp %>% group_by(day_format) %>% summarise(median_price__per_gram_shipping_from= median(price_in_bit, na.rm = T),
                                                        country=str_extract(string = pais,pattern = "(?<=country_list.).*"))
    price_per_gram_shipping_from[[a]]<-temp
    a<-a+1
  }
  
  
  price_per_gram_list<-list(price_per_gram_webpage, price_per_gram_purity, price_per_gram_country_origin, price_per_gram_shipping_from)

  return(price_per_gram_list)
  }

