x=read.csv('C:/Users/Juan Jose/Dropbox/Deepweb/salidas_1_11/listing_summary_df_cocaine.csv')

outliers=x%>%
  mutate(noweight=ifelse(test = !is.finite(min_price),yes = 1,no = 0))%>%
  mutate(minfilter=ifelse(test= is.finite(min_price) & min_price>=10, yes = 1,no = 0))%>%
  mutate(maxfilter=ifelse(test=(is.finite(max_price) & max_price>20) | (is.finite(max_price) & mean_price>10),yes = 1 ,no = 0))%>%
  mutate(maxfilterA=ifelse(test= maxfilter==1 & min_price<2,yes = 'A',no = 'B' ))

View(outliers)