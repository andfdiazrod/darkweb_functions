rm(list=ls())

library(haven)
library(stringr)
library(dplyr)

setwd('C:/Users/andfd/OneDrive - Universidad de los andes/')


for(dir_f in list.files('dark_Web_andres/darkweb_functions/functions',pattern='.R',full.names=1)){
  source(dir_f)
}

daily_dirs <- suppressWarnings(na.omit(as.numeric(list.dirs('Darkweb/grams',full.names = FALSE,recursive = FALSE))))

for(dir in as.character(daily_dirs)){
  daily_files <- list.files(paste0('Darkweb/grams/',dir),pattern = '.csv',full.names = FALSE)  
  
  agora_or_evol <- c(rep('Agora.csv',"Agora.csv" %in% daily_files),rep('EVO.csv',"EVO.csv" %in% daily_files))
  
  for(file in agora_or_evol){
    
    file_name_temp <- substr(file,1,(nchar(file)-4))
    df_orig <- read.csv(paste0('Darkweb/grams/',dir,'/',file),stringsAsFactors=FALSE)
    
    df_orig$listing_low <- tolower(df_orig$name)
    df_orig$description_low <- tolower(df_orig$description)
    
    df_cocaine <- is_cocaine_points_system(df_orig)
    df_crack <- is_crack(df_orig)
    colnames_cocaine_crack <- union(colnames(df_cocaine),colnames(df_crack))
    
    df <- data.frame(matrix(ncol=length(colnames_cocaine_crack)))
    colnames(df) <- colnames_cocaine_crack
    
    if(nrow(df_cocaine) > 0){
      df[1:nrow(df_cocaine),colnames(df_cocaine)] <- df_cocaine  
    }
    if(nrow(df_crack) > 0){
      df[(nrow(df)+1):(nrow(df_crack)+nrow(df)),colnames(df_crack)] <- df_crack
      df <- df[-1,]
    }
    #df <- find_weight(df)
    #df <- purity_extractor(df)
      
    df$day <- dir
    
    dir_output <- 'dark_web_andres/agora_evolution'
    if(!file %in% list.files(dir_output)){
      
      write.csv(df,paste0(dir_output,'/', file), 
                row.names=FALSE)
    } else {
      write.table(df,paste0(dir_output,'/', file),append=TRUE,
                  row.names=FALSE, col.names=FALSE,sep=',')
    }
  }
}
