#### INSTRUCTIONS 
  #Dates: vector with dates e.g. dates=20140609:20140831
  #Path: follow this form 'C:/Users/JOSE ALEJANDRO/Dropbox/Grams/' only modify C:/Users/JOSE ALEJANDRO e.g. 'C:/Users/Carlos/Dropbox/Grams/'
  #Website: its the EXACT NAME of the webpage e.g if you want evolution u should use website='EVO'

#####Example:

#If you want all dates from 2014 of evolution we should, the function parameters are the following:
#dates=20140609:20140831
#path='C:/Users/JOSE ALEJANDRO/Dropbox/Grams/'
#website='EVO'

    
join_days=function(dates,path,website){
  
  to_rbind=list()
  
  #Importing multiple dataases
  for (i in dates) {
    tryCatch(
      expr =  {           
        setwd(paste0(path,i))
        
        
        assign(paste(website,i,sep = "_"), read_csv(paste(website,'csv',sep = ".")))
      },
      error = function(e){
        message("* Caught an error on itertion ", i)
        print(e)
      }
    )
  }
  
  #Making a list with all databases
  for (i in dates) {
    tryCatch(
      expr =  { 
        z=i
        s <- paste(website, z, sep="_")
        d <- get(paste(website, z, sep="_"))
        to_rbind[[s]] <-d
      },
      error = function(e){
        message("* Caught an error on itertion ", i)
        print(e)
      }
    )
  }
  #creating the binded database
  df=do.call(rbind,to_rbind)
  
}








