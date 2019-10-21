country<-function(df){
  
  df=df %>%
    mutate(name_listing_low=tolower(name))%>%
    mutate(d_listing_low=tolower(description))%>%
    mutate(country_name=str_extract(name_listing_low,
                                    pattern = "peru|col[ou]+mbian?|bolivian?"))%>% #Aca se pueden añadir paises
    mutate(country_description=str_extract(d_listing_low,
                                           pattern = "peru|col[ou]+mbian?|bolivian?"))%>% #Aca se pueden añadir paises
    mutate(Country=ifelse(is.na(country_name),
                          yes =country_description,
                          no =country_name ))%>%
    select(-c(country_name,country_description,name_listing_low,d_listing_low))%>%
    mutate(Country= gsub('col[ou]+mbian?','Colombia',Country))%>% #Aca se cambian los nombres de los paises encontrados
    mutate(Country= gsub('bolivian?','Bolivia',Country))%>%
    mutate(Country= gsub('peru','Peru',Country))
  }
#Puede que se necesite añadir paises.

