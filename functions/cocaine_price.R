cocaine_price <- function(df){
  df$price_in_bit <- as.numeric(df$price) / as.numeric(df$weight_in_grams)
  
  bitcoin_price <- read.csv(file = 'functions/precio_bitcoin.csv')
  dates <- c()
  for(d in bitcoin_price$Date){
    date_temp <- strsplit(as.character(d),'-')[[1]]
    date <- paste0(date_temp[1], date_temp[2], date_temp[3])
    dates <- c(dates,date)
  }
  bitcoin_price <- cbind(bitcoin_price, dates)
  
  df$price_in_dollar <- NA
  
  c_ = 0
  for(d in unique(df$day)){
    c_ = c_ + 1
    df[which(df$day==d),'price_in_dollar'] <- df[which(df$day==d),'price_in_bit'] * bitcoin_price$Close[which(bitcoin_price$dates==d)]
  }
  return(df)
}


