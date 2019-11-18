outliers<-function(df)
{
  
  df<-df %>% mutate(no_weight=ifelse(is.na(weight_in_grams),1,0))
  df<- df %>% mutate(outlier_price=ifelse(is.finite(price_in_bit) & price_in_bit>=10, yes = 1,no = 0))
  df<- df %>% mutate(outlier_price_high=ifelse(is.finite(price_in_bit) & price_in_bit>=10, yes = 1,no = 0))
  df<- df %>% mutate(outlier_price_low=ifelse(is.finite(price_in_bit) & price_in_bit<=0.01, yes = 1,no = 0))
  
  no_weight<-df %>% filter(no_weight==1 )
  outlier_price_high<-df %>% filter( outlier_price_high==1 )
  outlier_price_low<-df %>% filter( outlier_price_low==1)
  return_list<-list()
 return_list[[1]]<-listings_summary(no_weight)
 return_list[[2]]<-listings_summary(outlier_price_high)
 return_list[[3]]<-listings_summary(outlier_price_low)
  return(return_list)
}



if(FALSE){
  
  library(dplyr)
  
  df<-read.csv("bases_transformadas/info_total.csv", stringsAsFactors = F)
  
x=read.csv('C:/Users/Juan Jose/Dropbox/Deepweb/salidas_1_11/listing_summary_df_cocaine.csv')

outliers=x%>%
  mutate(noweight=ifelse(test = !is.finite(min_price),yes = 1,no = 0))%>%
  mutate(minfilter=ifelse(test= is.finite(min_price) & min_price>=10, yes = 1,no = 0))%>%
  mutate(maxfilter=ifelse(test=(is.finite(max_price) & max_price>20) | (is.finite(max_price) & mean_price>10),yes = 1 ,no = 0))%>%
  mutate(maxfilterA=ifelse(test= maxfilter==1 & min_price<2,yes = 'A',no = 'B' ))

View(outliers)
}