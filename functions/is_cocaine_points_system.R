
is_cocaine_points_system<-function(df)
{
  
  #This method return a dataframe that contains two columns, cocaine and not_cocaine with a certain
  # amount of points were an the more points the observations has, there is more probability that 
  # the observation is a cocaine listing or not_cocaine listing
  
  #Preparing descriptions and listings
  df$description_low<-gsub("[\r\n]", "", df$description_low)
  df$listing_low<-gsub("[\r\n]","",df$listing_low)
  #a. Create a variable indicating everything that can be cocaine
  
  cocaine_name_list_1<-"coke|flake|snow|whitepowder|coca|perico|
  |nose candy|baseball|cocaine|cocain|coke|coka|koka|koks|fishscale|kokain|flakes|kokain|coca|
 |kokaiina|cokaine"
  
  cocaine_name_list_2<-"charlie|bump|big c|caine|coco|c-game|c game|marching powder|toot|base|basa|dust|big rush|pearl|
  |candy|cola|big flakes|baseball|bump| line |rail|stash|yeyo|discoshit|flake|flakes|zip|8ball|coco|blow"
  #Detecting if the listing is cocaine, given a list of names 
  is_cocaine_name_list1<-3*as.numeric(grepl(cocaine_name_list_1,df$listing_low))
  
  
  is_cocaine_name_list2<-1.5*as.numeric(grepl(cocaine_name_list_2,df$listing_low))
  
  
  #Detecting if the description of the listing belongs to a cocaine element, given a list of names 
  is_cocaine_description_name_list_1<-2*as.numeric(grepl(cocaine_name_list_1,df$description_low))
  
  
  is_cocaine_description_name_list_2<-grepl(cocaine_name_list_2,df$description_low)
  
  
  
  # NotCocaine --------------------------------------------------------------
  
  not_cocaine<-'bills|opioid|opiatewithdrawls|strawberrycough|mdma|e-vape|makerskit|leaves|meth|pipes|speed
  |amphetamin|prepaidcard|bth|heroin|neurontin|acetone|install|withdraw|tutorial
  |guide|customlistingforjacobscrackers|reviews|cracked|abriefhistoryofcocaine
  |cracking|generator|cocainenation:howtheWhitetradetookovertheWorld|handbook|password
  |poker|adobe|cocaine:thestraightfacts|extendedversion|redcocaine|cocainewilldamageyourlife
  |howthewhitetrade|hacker|netflix|instructions|wifi|hacker|wep|howto|crystallization|
  |cultivation|synthesis|methamphetamine|lighter|colastash|stashcan|synthesis
  |teaching|connect|pdf|cannabis|chocolate|dream|wax|paralysis|cookies
  |mda|mephedrone|opium|fentanyl|methadone|desoxyn|greencrack|scanner|keylogger
  |nitrous|ketamine|blacktar|tincture|d-isomer|cuttingagent|bathslats|lsd|GBL|MDPV
  |ecstacy|GHB|thecompletecultivationandsyntesisof|maskmyip
  |hippycrack|poker|accountcrack|fenixfp3|desktop|smtp|synthacaine|synthetic
  |thecokemachine|pipe|twitter|wi-fi|wanttoknowwho|nitrousoxide|butyrolactone|seed
  |ingest|protection|eztest|forensic|athome|fp3|mda-white|28gmda
  |premiumaccount|ez-test|recipe|puretar|blacktar|nugrun|ofice
  |cocacolapills|ketmin|mxeh|mdpvhcl|puretar|uncuttar|90%h#3|chinawhite|kush|facebook|testosterone
  |8ballofice|gunpowder|patches|mdpv|flubromazepam|diplomas|diploma|book|books|weed|marijuana|Testosterone
  |gold|ritalin|cocaine nation|psychosis|resident card|crack|gold|Gold|tsb|login|cloned
  |mdpv|gig|hardsales|amex|mcsc|dmt|cvv|black widow|silver bar|moneypak|steroids|roids|hydroxybutyrate|shamanism|erection|gbl|statement
  |passport|id|pharmacy|mda|oxycodone|miffy|bots|salbutamol|cunningulus|carding|kamagra|boldenone|trenbolone|turinabol|clenbuterol
  |hcg|pregnyl|nandrolone|socks|proxy|mxe|amphetamine|modafinil|coca cola stash can|hydrocodone
  |chocolata|cc|duloxetine|slump buster|cotton candy|diazepam|stash can|mushroom|ballzinator|sildenafil|metabolism|secret stash
  |explosives|tutorial|decline|casinos|3dsiso|triple combination|red flag|windows
  |rolling paper|nude photos|boobs|rescue|com db|uk db|forums|nutrients'
  
  not_cocaine_name_list_1<-"( bills |opioid|opiatewithdrawls|strawberrycough|mdma|e-vape|makerskit|leaves|meth|pipes|speed
  |amphetamin|prepaidcard|bth|heroin|neurontin|acetone|install|withdraw|tutorial
  |guide|customlistingforjacobscrackers|reviews|cracked|a brief history of
  |cracking|generator|how the White trade took over the World|handbook|password
  |poker|adobe|the straight facts|extended version|redcocaine|willdamageyourlife
  |howthewhitetrade|hacker|netflix|instructions|wifi|hacker| wep |crystallization
  |cultivation|synthesis|methamphetamine|lighter|colastash|stashcan|synthesis
  |teaching|connect|pdf|cannabis|chocolate|dream|wax|paralysis|cookies
  |mda|mephedrone|opium|fentanyl|methadone|desoxyn|greencrack|scanner|keylogger
  |nitrous|ketamine|blacktar|tincture|d-isomer|bathslats|lsd|GBL|MDPV
  |ecstacy|GHB|thecompletecultivationandsyntesisof|maskmyip
  |hippycrack|poker|account crack|fenixfp3|desktop|smtp|synthacaine|synthetic
  |thecokemachine|pipe|twitter|wi-fi|wanttoknowwho|nitrousoxide|butyrolactone|seed
  |protection|eztest|forensic|athome|fp3|mda-white|28gmda|fenethylline
  |premiumaccount|ez-test|recipe|puretar|blacktar|nugrun|ofice|how make 
  |cocacolapills|ketmin|mxeh|mdpvhcl|puretar|uncuttar|90%h#3|chinawhite|kush|facebook|testosterone
  |8ballofice|gunpowder|patches|mdpv|flubromazepam|diplomas|diploma|book|books|weed|marijuana|Testosterone
  |gold|ritalin|nation|psychosis|resident card|crack|gold|Gold|tsb|login|cloned
  |mdpv| gig |hardsales|amex|mcsc|dmt|cvv|black widow|silver bar|moneypak|steroids|roids|hydroxybutyrate|shamanism|erection|gbl|statement
  | passport |identification|pharmacy|mda|oxycodone|miffy|bots|salbutamol|cunningulus|carding|kamagra|boldenone|trenbolone|turinabol|clenbuterol
  | hcg |pregnyl|nandrolone|socks|proxy|mxe|amphetamine|modafinil|coca cola stash can|hydrocodone|tutorial|snowden
  |chocolata|cc|duloxetine|slump buster|cotton candy|benzocaine|diazepam|stash can|mushroom|ballzinator|sildenafil|metabolism|secret stash
  |explosives|tutorial|decline|casinos|3dsiso|triple combination|history|wash| test | smuggling|valium|coconut oil|seed|captagon
  |rolling paper|nude photos|boobs|rescue|com db |uk db |forums|nutrients|purplecrack|bluecrack|greencrack|fakecocaine|clenbuterol| tea | tee |lottery|how to make|car safe stash|syntethic cocaine)"
  
  
  not_cocaine_name_list_description<-"(tutorial|porn|valid cc|clenbuterol|digital download|download)"
  
  
  not_cocaine_listing_name_list1<-3*as.numeric(grepl(not_cocaine_name_list_1, df$listing_low))
  
  not_cocaine_description_list<-as.numeric(grepl(not_cocaine_name_list_description, df$description_low))
  
  
  # Drug Combos -------------------------------------------------------------
  
  other_drugs<-"valium|mda|weed|mdpv|mdma|meth |methamphetamin|amphetamin|heroin|cannabis|fentanyl|lsd|acid|MDPV|ecstacy|flubromazepam
|marijuana|ritalin|gbl|hydrocodone|mdma"
  
  tutorial<-grepl("tutorial",df$listing_low)
  
  other_drugs<-grepl(other_drugs, df$listing_low)
  

# Sample ------------------------------------------------------------------
  
  sample_list<-" sample| free sample|trial"
  sample<-grepl(sample_list, df$listing_low)  
  
  
  
  # 
  # 
  # DataFrame Construction --------------------------------------------------
  
  
  df<-df %>% mutate(cocaine=is_cocaine_name_list1+ is_cocaine_name_list2+is_cocaine_description_name_list_1+is_cocaine_description_name_list_2, 
                    not_cocaine= not_cocaine_listing_name_list1+not_cocaine_description_list) 
  
  df<-df %>% mutate(combos=cocaine+as.numeric(other_drugs)) 
  df$combos<-ifelse(df$combos>df$cocaine & df$combos>4 & tutorial==0, 1,0)
  df$sample<-sample
  df_cocaine<-df %>% filter(cocaine>=4 & not_cocaine<2)
  df_combos<-df %>% filter(combos==1)
  
  df<-rbind(df_cocaine, df_combos)
  
  
  return(df)
  
  
  
  
}




