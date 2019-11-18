country<-function(df){
  
  df=df %>%
    mutate(country_name=str_extract(listing_low,
                                    pattern = "peru|col[ou]+mbian?|bolivian?"))%>% #Aca se pueden añadir paises
    mutate(country_description=str_extract(description_low,
                                           pattern = "peru|col[ou]+mbian?|bolivian?"))%>% #Aca se pueden añadir paises
    mutate(country=ifelse(is.na(country_name),
                          yes =country_description,
                          no =country_name ))%>%
    select(-c(country_name,country_description))%>%
    mutate(country= gsub('col[ou]+mbian?','Colombia',country))%>% #Aca se cambian los nombres de los paises encontrados
    mutate(country= gsub('bolivian?','Bolivia',country))%>%
    mutate(country= gsub('peru','Peru',country))
  }
#Puede que se necesite añadir paises.

