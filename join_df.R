join_df<-function(dir_='C:/Users/af.diazr/Dropbox/Deepweb/bases_transformadas')
{
  
  names<-list.files(dir_,pattern = "info_")
  df<-NULL
  for(name in names)
  {
    print(name)
    temp<-read.csv(paste0(dir_,name), stringsAsFactors = F)
    df<-rbind(df,temp)
  }
  
  return(df)
}