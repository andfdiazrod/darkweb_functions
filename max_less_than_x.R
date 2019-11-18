max_less_than_x <- function(x){
  if(x > 0){
    ret <- suppressWarnings(max(non_na[which(non_na<x & non_na>0)])  )
    if(is.infinite(ret)){
      ret <- min(non_na[which(non_na>x)])  
    }
  } else {
    ret <- 0
  }
  return(ret)
}