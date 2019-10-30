is_crack_points_system<-function(df){
 #Looking for Crack
crack_name_list_1<-"crack|crac|crak|krak|krack|riss"
  
crack_name_list_2<-"rock|base|ball|rocks|nugget|grit| hail|dice|sleet|chemical|tornado|24-7|crumbs|ice cube|sleet"

  #Looking for not crack
not_crack<-"(3 5g 8ball columbian fishscale uncut cocaine|a half ball or 1 75 grams of australia s best damn fucking|waitrose|the millionaire fastlane crack the code to wealth|the julian special kraken and coke mix|driver genius professional 11 final crack|mapb hcl|give us the ballot the modern struggle for voting rights|debt or democracy public money for sustainability|crack99 the takedown of a 100 million dollar chinese so|conejo crack house|microsoft office 2016 professional plus 16 0 x86x64|how turn cocaine into a freebase form|xanor lappuja|piping crunch with aircrack ng|niche marketing|fl studio|google crack search|ebay hackcracktip|hacked user databases info collection|custom for|mapb hcl|budder crack|abbyy finereader 12 best ocr with crack|alien spy rat 5 0 with plugin no crack|hashcat rules blandy toxic ninja rockyou megatron|krissy top shelf|photoshop cs 6 crack by big boss|winzip pro version 19 crack x86 x32 serial|nero burning rom 2014 v15 0 05300 ml incl crack key at|lemon crack|daemon tools pro advanced with crack|blackshades 5 5 1 crack with sql database|avira antivirus pro 2015 serial code and crack|abbyy finereader 12 with crack|custom listing|pure power crack vap juce|windows|craked|geen|aafcrack|megakrakken|password|crissy moon|cracker|product key not a crack|clarissa|buds|rsa 4096|green crack|candy|bills|opioid|opiate withdrawls|strawberry cough|mdma|e-vape|makerskit|leaves|meth|pipes|speed|
                |amphetamin|prepaid card|prepaidcard|bth|heroin|neurontin|acetone|install|withdraw|tutorial
              |guide|custom listing for jacobs crackers|reviews|cracked|a brief history of
              |cracking|generator|cocaine nation:how the white trade took over the World|handbook|password
              |poker|adobe|the straight facts|extended version|red cocaine|will damage your life
              |how the white trade|hacker|netflix|instructions|wifi|hacker|wep|how to|howto|crystallization
              |cultivation|synthesis|methamphetamine|lighter|colastash|stashcan|synthesis
              |teaching|connect|pdf|cannabis|chocolate|dream|wax|paralysis|cookies
              |mda|mephedrone|opium|fentanyl|methadone|desoxyn|greencrack|scanner|key logger|keylogger|
              |nitrous|ketamine|blacktar|tincture|d-isomer|bathslats|lsd|GBL|MDPV
              |ecstacy|GHB|the complete cultivation and syntes is of|maskmyip|mask my ip|
              |hippycrack|hippy crack|poker|accountcrack|account crack|fenixfp3|desktop|smtp|synthacaine|synthetic
              |the coke machine|pipe|twitter|wi-fi|want to know who|nitrousoxide|butyrolactone|seed
              |ingest|protection|eztest|forensic|athome|fp3|mda-white|28gmda
              |premiumaccount|ez-test|recipe|puretar|blacktar|nugrun|ofice
              |coca cola pills|cocacola pills|cocacolapills|ketmin|mxeh|mdpvhcl|puretar|uncuttar|90%h#3|chinawhite|kush|facebook|testosterone
              |8 ball ofice|8ballofice|gunpowder|patches|mdpv|flubromazepam|diplomas|diploma|book|books|weed|marijuana|testosterone
              |gold|ritalin|nation|psychosis|resident card|gold|Gold|tsb|login|cloned
              |mdpv|gig|hardsales|amex|mcsc|dmt|cvv|black widow|silver bar|moneypak|steroids|roids|hydroxybutyrate|shamanism|erection|gbl|statement
              |passport|id|pharmacy|mda|oxycodone|miffy|bots|salbutamol|cunningulus|carding|kamagra|boldenone|trenbolone|turinabol|clenbuterol
              |hcg|pregnyl|nandrolone|socks|proxy|mxe|amphetamine|modafinil|coca cola stash can|hydrocodone
              |chocolata|cc|duloxetine|slump buster|cotton candy|benzocaine|diazepam|stash can|mushroom|ballzinator|sildenafil|metabolism|secret stash
              |explosives|tutorial|decline|casinos|3dsiso|triple combination|
                |rolling paper|nude photos|boobs|rescue|com db|uk db|forums|nutrients|purplecrack|purple crack|bluecrack|blue crack|greencrack|fake cocaine|fakecocaine|clenbuterol|tea|tee|te|lottery|how to make)"

not_crack_name_list_description<-"(this is top of the line product meant for picky users or crack freebase cookers|the quality of freebase is higher than crack|crack base gl gg... this is the shit..|green|password|book|tutorial|porn|valid cc|clenbuterol|digital download|download)" 


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


