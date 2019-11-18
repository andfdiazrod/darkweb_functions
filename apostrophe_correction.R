
# Apostrophe Correction ---------------------------------------------------

apostrophe_correction<-function(df)
{

# Corrigiendo los listings ------------------------------------------------

  
  df$listing_low<-str_replace_all(df$listing_low, pattern = "n 39 t", replacement = "n't")
  df$listing_low<-str_replace_all(df$listing_low, pattern = "i 39 m", replacement = "i'm")
  df$listing_low<-str_replace_all(df$listing_low, pattern = "i 39 ve", replacement = "i've")
  df$listing_low<-str_replace_all(df$listing_low, pattern = "e 39 re", replacement = "e're")
  df$listing_low<-str_replace_all(df$listing_low, pattern = "i 39 ll", replacement = "i'll")
  df$listing_low<-str_replace_all(df$listing_low, pattern = "e 39 ll", replacement = "e'll")
  df$listing_low<-str_replace_all(df$listing_low, pattern = "t 39 s", replacement = "t's")
  df$listing_low<-str_replace_all(df$listing_low, pattern = "u 39 re", replacement = "u're")
  df$listing_low<-str_replace_all(df$listing_low, pattern = " 39 ", replacement = "'")
  

# Corrigiendo las descripciones -------------------------------------------

  
  df$description_low<-str_replace_all(df$description_low, pattern = "n 39 t", replacement = "n't")
  df$description_low<-str_replace_all(df$description_low, pattern = "i 39 m", replacement = "i'm")
  df$description_low<-str_replace_all(df$description_low, pattern = "i 39 ve", replacement = "i've")
  df$description_low<-str_replace_all(df$description_low, pattern = "e 39 re", replacement = "e're")
  df$description_low<-str_replace_all(df$description_low, pattern = "i 39 ll", replacement = "i'll")
  df$description_low<-str_replace_all(df$description_low, pattern = "e 39 ll", replacement = "e'll")
  df$description_low<-str_replace_all(df$description_low, pattern = "t 39 s", replacement = "t's")
  df$description_low<-str_replace_all(df$description_low, pattern = "u 39 re", replacement = "u're")
  df$description_low<-str_replace_all(df$description_low, pattern = " 39 ", replacement = "'")
  
  return(df)
}


