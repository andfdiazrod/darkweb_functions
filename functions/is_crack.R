
# Is Crack function -------------------------------------------------------
is_crack<-function(df)
{
  #This method return a dataframe that contains only the listings of elements
  # that were crack with all the prevoius columns plus two to other columns
  # called crack and not_crack. 
  
  #Preparing descriptions and listings
  df$description_low<-gsub("[\r\n]", "", df$description_low)
  df$listing_low<-gsub("[\r\n]","",df$listing_low)
  
  crack_name_list_1<-"crack|crac|crak|krak|krack|riss"


  crack_name_list_2<-"candy|rock|candy|base|ball|rocks|nugget|grit|
  hail|dice|sleet|chemical|tornado|24-7|crumbs|ice cube|sleet"
  
  
  
  is_crack_name_list_1<-3*as.numeric(grepl(crack_name_list_1, df$listing_low))
  
  is_crack_name_list_2<-1.5*as.numeric(grepl(crack_name_list_2, df$listing_low))
  
  is_crack_desc_list_1<-2*as.numeric(grepl(crack_name_list_1, df$description_low))
    
  is_crack_desc_list_2<-as.numeric(grepl(crack_name_list_2, df$description_low))
  
  sum(ifelse(is_crack_desc_list_1==T, 1,0))
  
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
  
  
  not_crack_name_list_description<-"(tutorial|porn|valid cc|clenbuterol|digital download|download)"
  
  
  not_crack_listing<-3*as.numeric(grepl(not_crack, df$listing_low))
 
  
  not_crack_description<-as.numeric(grepl(not_crack_name_list_description, df$description_low))
  
  
  df_crack<-df %>% mutate(crack=is_crack_desc_list_2+ is_crack_desc_list_1+is_crack_name_list_1+is_crack_name_list_2, 
                          not_crack= not_crack_listing+not_crack_description) %>% filter(crack>=4 & not_crack<=1)
  return(df_crack)
  
}
