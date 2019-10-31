descriptive=function(df, pag=pag){

cocained=df%>%
  filter(cocaine>=0 & not_cocaine<=1 & sample==F & combos==0)%>%
  group_by(day_format)%>%
  summarize(pg_mean=mean(price_in_bit, na.rm = T),pg_median=median(price_in_bit, na.rm = T),totalq=sum(weight_in_grams,na.rm = T),howmanyvendors=length(unique(vendor_name)))

crackd=df%>%
  filter(is_crack>=0 & not_crack<=1 & sample==F)%>% 
  group_by(day_format)%>%
  summarize(pg_mean=mean(price_in_bit, na.rm = T),pg_median=median(price_in_bit, na.rm = T),totalq=sum(weight_in_grams,na.rm = T),howmanyvendors=length(unique(vendor_name)))

pgmeanvsday_cocaine=ggplot(cocained, aes(x=day_format, y=pg_mean)) + geom_line() + xlab('Day') + ylab ('Mean Price per gram / Cocaine') + theme(axis.text.x = element_text(angle = 90)) + ggtitle(substring(pag,1,nchar(pag)-4))
pgmedianvsday_cocaine=ggplot(cocained, aes(x=day_format, y=pg_median)) + geom_line() + xlab('Day') + ylab ('Median Price per gram / Cocaine') + theme(axis.text.x = element_text(angle = 90))
totalqvsday_cocaine=ggplot(cocained, aes(x=day_format, y=totalq)) + geom_line() + xlab('Day') + ylab ('Quantity Offered / Cocaine') + theme(axis.text.x = element_text(angle = 90))
vendorsvsday_cocaine=ggplot(cocained, aes(x=day_format, y=howmanyvendors)) + geom_line() + xlab('Day') + ylab ('Number of Vendors / Cocaine') + theme(axis.text.x = element_text(angle = 90))

pgmeanvsday_crack=ggplot(crackd, aes(x=day_format, y=pg_mean)) + geom_line() + xlab('Day') + ylab ('Mean Price per gram / Crack') + theme(axis.text.x = element_text(angle = 90))
pgmedianvsday_crack=ggplot(crackd, aes(x=day_format, y=pg_median)) + geom_line() + xlab('Day') + ylab ('Median Price per gram / Crack') + theme(axis.text.x = element_text(angle = 90))
totalqvsday_crack=ggplot(crackd, aes(x=day_format, y=totalq)) + geom_line() + xlab('Day') + ylab ('Quantity Offered / Crack') + theme(axis.text.x = element_text(angle = 90))
vendorsvsday_crack=ggplot(crackd, aes(x=day_format, y=howmanyvendors)) + geom_line() + xlab('Day') + ylab ('Number of Vendors / Crack') + theme(axis.text.x = element_text(angle = 90))


descriptive<-list(cocained,crackd,pgmeanvsday_cocaine,pgmedianvsday_cocaine,totalqvsday_cocaine,vendorsvsday_cocaine,
                 pgmeanvsday_crack,pgmedianvsday_crack,totalqvsday_crack,vendorsvsday_crack)

return(descriptive)

}


#To view use View(descriptive[[1...10]]) for dataframes or descriptive[[1...10]] for graphs
#[[1]]=cocained= dataframe of cocaine descriptive variables
#[[2]]=crackd = dataframe of crack descriptive variables
#[[3]]=pgmeanvsday_cocaine = graph of pgmean vs day_format of cocaine listings
#[[4]]=pgmedianvsday_cocaine  = graph of pgmedian vs day_format of cocaine listings
#[[5]]=totalqvsday_cocaine  = graph of totalq vs day_format of cocaine listings
#[[6]]=vendorsvsday_cocaine  = graph of vendors vs day_format of cocaine listings
#[[7]]=pgmeanvsday_crack  = graph of pgmean vs day_format of cocaine listings
#[[8]]=pgmedianvsday_crack  = graph of pgmedian vs day_format of cocaine listings
#[[9]]=totalqvsday_crack  = graph of totalq vs day_format of cocaine listings
#[[10]]=vendorsvsday_crack = graph of vendors vs day_format of cocaine listings