is_crack_points_system<-function(df){
 #Looking for Crack
crack_name_list_1<-"crack|crac|crak|krak|krack|riss"
  
crack_name_list_2<-"candy|rock|candy|base|ball|rocks|nugget|grit| hail|dice|sleet|chemical|tornado|24-7|crumbs|ice cube|sleet"

  #Looking for not crack
not_crack<-"(bills|opioid|opiatewithdrawls|strawberrycough|mdma|e-vape|makerskit|leaves|meth|pipes|speed|
                |amphetamin|prepaidcard|bth|heroin|neurontin|acetone|install|withdraw|tutorial
              |guide|customlistingforjacobscrackers|reviews|cracked|a brief history of
              |cracking|generator|cocainenation:howtheWhitetradetookovertheWorld|handbook|password
              |poker|adobe|thestraightfacts|extendedversion|redcocaine|willdamageyourlife
              |howthewhitetrade|hacker|netflix|instructions|wifi|hacker|wep|howto|crystallization
              |cultivation|synthesis|methamphetamine|lighter|colastash|stashcan|synthesis
              |teaching|connect|pdf|cannabis|chocolate|dream|wax|paralysis|cookies
              |mda|mephedrone|opium|fentanyl|methadone|desoxyn|greencrack|scanner|keylogger
              |nitrous|ketamine|blacktar|tincture|d-isomer|bathslats|lsd|GBL|MDPV
              |ecstacy|GHB|thecompletecultivationandsyntesisof|maskmyip
              |hippycrack|poker|accountcrack|fenixfp3|desktop|smtp|synthacaine|synthetic
              |thecokemachine|pipe|twitter|wi-fi|wanttoknowwho|nitrousoxide|butyrolactone|seed
              |ingest|protection|eztest|forensic|athome|fp3|mda-white|28gmda
              |premiumaccount|ez-test|recipe|puretar|blacktar|nugrun|ofice
              |cocacolapills|ketmin|mxeh|mdpvhcl|puretar|uncuttar|90%h#3|chinawhite|kush|facebook|testosterone
              |8ballofice|gunpowder|patches|mdpv|flubromazepam|diplomas|diploma|book|books|weed|marijuana|Testosterone
              |gold|ritalin|nation|psychosis|resident card|gold|Gold|tsb|login|cloned
              |mdpv|gig|hardsales|amex|mcsc|dmt|cvv|black widow|silver bar|moneypak|steroids|roids|hydroxybutyrate|shamanism|erection|gbl|statement
              |passport|id|pharmacy|mda|oxycodone|miffy|bots|salbutamol|cunningulus|carding|kamagra|boldenone|trenbolone|turinabol|clenbuterol
              |hcg|pregnyl|nandrolone|socks|proxy|mxe|amphetamine|modafinil|coca cola stash can|hydrocodone
              |chocolata|cc|duloxetine|slump buster|cotton candy|benzocaine|diazepam|stash can|mushroom|ballzinator|sildenafil|metabolism|secret stash
              |explosives|tutorial|decline|casinos|3dsiso|triple combination|
                |rolling paper|nude photos|boobs|rescue|com db|uk db|forums|nutrients|purplecrack|bluecrack|greencrack|fakecocaine|clenbuterol|tea|tee|te|lottery|how to make)"

not_crack_name_list_description<-"(tutorial|porn|valid cc|clenbuterol|digital download|download)" #XQ NO METERLAS ARRIBA


  #Function
  df=df %>%
    mutate(name_listing_low=tolower(name))%>%
    mutate(d_listing_low=tolower(description))%>%
    mutate(name_listing_low=gsub("[\r\n]", "", name_listing_low))%>%
    mutate(d_listing_low=gsub("[\r\n]","",d_listing_low))%>%
    mutate(is_crack_name_list1=3*as.numeric(grepl(crack_name_list_1,name_listing_low)))%>%
    mutate(is_crack_name_list2=1.5*as.numeric(grepl(crack_name_list_2, name_listing_low)))%>%
    mutate(is_crack_description_list1=2*as.numeric(grepl(crack_name_list_1, d_listing_low)))%>%
    mutate(is_crack_description_list2=as.numeric(grepl(crack_name_list_2,d_listing_low)))%>%
    mutate(not_crack_name=3*as.numeric(grepl(not_crack, name_listing_low)))%>%
    mutate(not_crack_description=as.numeric(grepl(not_crack_name_list_description, d_listing_low)))%>%
    mutate(is_crack=is_crack_name_list1+is_crack_name_list2+is_crack_description_list1+is_crack_description_list2)%>%
    mutate(not_crack=not_crack_name+not_crack_description)%>%
    filter(is_crack>=4 & not_crack<2)%>%
    select(-c(is_crack_name_list1, is_crack_name_list2, is_crack_description_list1, is_crack_description_list2, not_crack_name, not_crack_description))
  
}

