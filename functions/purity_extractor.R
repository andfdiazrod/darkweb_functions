
# Funcion de Pureza -------------------------------------------------------

purity_extractor<-function(df)
{
  
  
  
  purity2<-str_extract(str_remove(df$description_low, pattern =  "[0-9].?[0-9]?(?=.?[g|gr|gram|grams|ounce|onz|lb|k|kilo|ki])"), 
                       pattern = "[0-9][0-9](?=[purity|quality|PURITY|QUALITY|pure|grade]{8})")
  
  purity1<-str_extract(str_remove(df$description_low, pattern =  "[0-9].?[0-9]?(?=.?[g|gr|gram|grams|ounce|onz|lb|k|kilo|ki])"), 
                       pattern = "(?<=[purity|quality|pure|grade]{5})[0-9][0-9]" )
  
  purity2_listing<-str_extract(str_remove(df$listing_low, pattern =  "[0-9].?[0-9]?(?=.?[g|gr|gram|grams|ounce|onz|lb|k|kilo|ki])"), 
                               pattern = "[0-9][0-9](?=.[[Pp]urity|[Qq]uality|PURITY|QUALITY|[Pp]ure|grade]{8})" )
  
  purity1_listing<-str_extract(str_remove(df$listing_low, pattern =  "[0-9].?[0-9]?(?=.?[g|gr|gram|grams|ounce|onz|lb|k|kilo|ki])"), 
                               pattern = "(?<=[[Pp]urity|[Qq]uality|PURITY|QUALITY|[Pp]ure|grade]{5})[0-9][0-9]" )
  
  purity3_listing<-str_extract(str_remove(df$listing_low, pattern =  "[0-9].?[0-9]?(?=.?[g|gr|gram|grams|ounce|onz|lb|k|kilo|ki])"), 
                               pattern = "[0-9][0-9]")
  pure<-purity2
  
  
  for( i in 1:length(pure))
  {
    pureza_temp<-pure[i]
    pureza_temp2<-purity1[i]
    pureza_listing2<-purity2_listing[i]
    pureza_listing1<-purity1_listing[i]
    pureza_listing3<-purity3_listing[i]
    if(!(is.na(pureza_temp2)| is.na(pureza_temp))&(pureza_temp<pureza_temp2))
    {
      pure[i]<-pureza_temp2
    }
    else if(is.na(pureza_temp) & !is.na(purity1[i]))
    {
      pure[i]<-purity1[i]
    }
    else if(is.na(pure[i]) & !is.na(purity2_listing[i]))
    {
      pure[i]<-purity2_listing[i]
    }
    else if(is.na(pure[i]) & !is.na(purity1_listing[i]))
    {
      pure[i]<-purity1_listing[i]
    }
    else if(is.na(pure[i]) & !is.na(purity3_listing[i]))
    {
      pure[i]<-purity3_listing[i]
    }
    if(!(is.na(pureza_listing1)|is.na(pureza_listing2)))
    { 
      max_pure<-max(na.omit(c(pure[i], pureza_listing1[i],pureza_listing2[i])))
      pure[i]<-max_pure
    }
   if( !(is.na(pure[i])| is.na(df$weight[i]))& (pure[i]==df$weight[i]))
     {
       temp_purity_desc_back<-str_extract(str_remove(df$description_low[i], pattern = df$weight[i]), 
                     pattern = "[0-9][0-9](?=[purity|quality|PURITY|QUALITY|pure|grade]{8})")
       temp_purity_desc_foward<-str_extract(str_remove(df$description_low[i], pattern = df$weight[i]), 
                                                     pattern = "(?<=[purity|quality|pure|grade]{5})[0-9][0-9]" )
       temp_purity_listing_back<-str_extract(str_remove(df$listing_low[i], pattern = df$weight[i]), 
                                             pattern = "[0-9][0-9](?=.[[Pp]urity|[Qq]uality|PURITY|QUALITY|[Pp]ure|grade]{8})" )
       temp_purity_listing_foward<-str_extract(str_remove(df$listing_low[i], pattern = df$weight[i]), 
                                              pattern =  "(?<=[[Pp]urity|[Qq]uality|PURITY|QUALITY|[Pp]ure|grade]{5})[0-9][0-9]" )
       
       pure[i]<-temp_purity_desc_back
       
       if(is.na(pure[i]) & !is.na(temp_purity_listing_foward))
       {
         pure[i]<-temp_purity_listing_foward
       }
       if( is.na(pure[i]) & !is.na(temp_purity_desc_foward))
       {
         pure[i]<-temp_purity_desc_foward
       }
       if(is.na(pure[i]) & !is.na(temp_purity_listing_back))
       {
         pure[i]<-temp_purity_listing_back
       }
       
     }
    
   
  }
  
  uncut_listing<-as.numeric(grepl(pattern = "uncut", x = df$listing_low))
  uncut_description<-as.numeric(grepl(pattern = "uncut", x= df$description_low))
  df$uncut<- uncut_listing+ uncut_description
  
  df$purity<-as.numeric(as.character(pure))
  return(df)
  
}

