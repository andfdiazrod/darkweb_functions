install.packages(c("readr","readxl"))
install.packages("dplyr")
library(readr)
library(readxl)
library(dplyr)

rm(list=ls())


##################################################################################
dates=c(20140611,20140612,20140613,20140614,20140615,20140616,20140617,20140618) #Aca agregar fechas como quieran

for (i in dates) {
    
                  #Esto lo cambian por su carpeta
    setwd(paste0('C:/Users/JOSE ALEJANDRO/Dropbox/Grams/',i))
  
                  #Aca nombran como quieren tener las bases en R
    assign(paste('agora',i,sep = "_"), read_csv('Agora.csv'))
}

#Crean lista con las bases 
to_rbind=list(agora_20140611,agora_20140612,agora_20140613,agora_20140614,agora_20140615,agora_20140616,agora_20140617,agora_20140618)

#Se hace Rbind
df=do.call(rbind,to_rbind)

##########################################################################



  
