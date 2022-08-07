library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(tidyr)
library(extrafont)
library(glue)
library(scales)
library(zoo)
library(DT)
library(shiny)
library(shinydashboard)
library(rsconnect)
library(leaflet)
library(geojsonio)
library(htmlwidgets)
library(htmltools)
library(prettydoc)
library(echarts4r)
library(countrycode)
# -------------- READ DATA

afghanistan <- read_excel("datainput/afghanstrike.xlsx")
pakistan <- read_excel("datainput/pakistanstrike.xlsx")
somalia <- read_excel("datainput/somaliastrike.xlsx")
yemen <- read_excel("datainput/yemenstrike.xlsx")

# -------------- DATA PREPARATION

# Afghanistan
afghanistan <- 
  afghanistan %>% 
  rename(`US Confirmed`="US confirmed?",
         `Minimum Strikes` = "Minimum strikes",
         `Maximum Strikes` = "Maximum strikes",
         `Minimum Total Killed` = "Minimum total people killed",
         `Maximum Total Killed` = "Maximum total people killed",
         `Minimum Civilians Killed` = "Minimum civilians reported killed",
         `Maximum Civilians Killed` = "Maximum civilians reported killed",
         `Minimum Children Killed` = "Minimum children reported killed",
         `Maximum Children Killed` = "Maximum children reported killed",
         `Minimum Injured` = "Minimum reported injured",
         `Maximum Injured` = "Maximum reported injured") %>% 
  mutate(Country = "Afghanistan",
         `US Confirmed` = as.character(`US Confirmed`)) %>%
  select(Date, Country, Province,`US Confirmed`,`Minimum Strikes`,`Maximum Strikes`,`Minimum Total Killed`,
         `Maximum Total Killed`,`Minimum Civilians Killed`,`Maximum Civilians Killed`,`Minimum Children Killed`,
         `Maximum Children Killed`,`Minimum Injured`,`Maximum Injured`)

# Change the Confirmed Value to 1 = Confirmed, 0 = Possible
afghanistan <- 
  afghanistan %>%
  mutate(`US Confirmed` = ifelse(grepl("1", afghanistan$`US Confirmed`) == TRUE, "Confirmed","Possible"))

# Pakistan
pakistan <- 
  pakistan %>% 
  mutate(Province = ifelse(grepl("South Waziristan|North Waziristan|Bajaur Agency|Bannu Frontier Region|Kurram Agency|Orakzai Agency|Khyber Agency|North/South Waziristan|Khyber Pakhtunkhwa province|Balochistan", pakistan$Area) == TRUE, "Khyber Pakhtunkhwa",NA),
         Minimum_strikes = 0,
         Maximum_strikes = 1,
         Country = "Pakistan",
         `US Confirmed` = 0) %>% 
  rename(`Minimum Strikes` = "Minimum_strikes",
         `Maximum Strikes` = "Maximum_strikes",
         `Minimum Total Killed` = "Minimum total people killed",
         `Maximum Total Killed` = "Maximum total people killed",
         `Minimum Civilians Killed` = "Minimum civilians reported killed",
         `Maximum Civilians Killed` = "Maximum civilians reported killed",
         `Minimum Children Killed` = "Minimum children reported killed",
         `Maximum Children Killed` = "Maximum children reported killed",
         `Minimum Injured` = "Minimum reported injured",
         `Maximum Injured` = "Maximum reported injured") %>%
  mutate(`US Confirmed` = as.character(`US Confirmed`)) %>%
  select(Date, Country, Province,`US Confirmed`,`Minimum Strikes`,`Maximum Strikes`,`Minimum Total Killed`,
         `Maximum Total Killed`,`Minimum Civilians Killed`,`Maximum Civilians Killed`,`Minimum Children Killed`,
         `Maximum Children Killed`,`Minimum Injured`,`Maximum Injured`)

# Change the Confirmed Value to 1 = Confirmed, 0 = Possible
pakistan <- 
  pakistan %>%
  mutate(`US Confirmed` = ifelse(grepl("1", pakistan$`US Confirmed`) == TRUE, "Confirmed","Possible"))

# Somalia Region
somalia <- 
  somalia %>% 
  mutate(Province = ifelse(grepl("Banadiir|Mogadishu|Wadajir", somalia$Location) == TRUE, "Banadiir",
                           ifelse(grepl("Dusa Marreb|South-central Somalia|Southern Somalia|Near Gobanale|Gaduug|Gaduud|Awhiigle", somalia$Location) == TRUE, "Galguduud",
                                  ifelse(grepl("Yasooman|Raso Camp|Hiran", somalia$Location) == TRUE, "Hiiraan",
                                         ifelse(grepl("Middle Shabelle", somalia$Location) == TRUE, "Shabeellaha Dhexe",
                                                ifelse(grepl("Barawe|Afgoye|Elasha Biyaha|Lower Shabelle|Guhm|Hawai|Dugule|Balad Amin|Kunyo-Barow|Kuntuwarey|Awdhegle|Dar es Salam village|Kunyo Barrow|Tortoroow|Bariire|Basra village|Idow Jalad|Mubarak|Gandarshe|Beled Amin|Kunyow Barrow|Awdeegle|Janaale|lower Shabelle|Baledogle|Qunyo Barrow|Bacaw", somalia$Location) == TRUE, "Shabeellaha Hoose",
                                                       ifelse(grepl("Bargal|Qandala town|Puntland|Bosasso|Bari", somalia$Location) == TRUE, "Bari",
                                                              ifelse(grepl("Galcayo|Haradere|El Burr|Debatscile|Mudug", somalia$Location) == TRUE, "Mudug",
                                                                     ifelse(grepl("Dinsoor|Bay region", somalia$Location) == TRUE, "Bay",
                                                                            ifelse(grepl("Baardheere", somalia$Location) == TRUE, "Gedo",
                                                                                   ifelse(grepl("Jillib|Sakow|Jilib|Jamecco|Lebede|Basra|Middle Juba|Saakow|Dujuuma|Caliyoow Barrow", somalia$Location) == TRUE, "Jubbada Dhexe",
                                                                                          ifelse(grepl("Ras Kamboni|Hayo|Waldena|Dhobley|Kismayo|Juba|Lower Juba|Caba|Luglaw|Jamaame|Kamsuuma|Lower Juba|Jubaland|Kobon|Bangeeni|Jamaame|Beer Xaani", somalia$Location) == TRUE, "Jubbada Hoose",
                                                                                                 ifelse(grepl("Golis Mountains", somalia$Location) == TRUE, "Togdheer",NA)))))))))))))

# Somalia
somalia <- 
  somalia %>% 
  rename(`US Confirmed`="Confirmed/\r\npossible US strike",
         `Minimum Strikes` = "Minimum strikes",
         `Maximum Strikes` = "Maximum strikes",
         `Minimum Total Killed` = "Minimum people killed",
         `Maximum Total Killed` = "Maximum people killed",
         `Minimum Civilians Killed` = "Minimum civilians killed",
         `Maximum Civilians Killed` = "Maximum civilians killed",
         `Minimum Children Killed` = "Minimum children killed",
         `Maximum Children Killed` = "Maximum children killed",
         `Minimum Injured` = "Minimum people injured",
         `Maximum Injured` = "Maximum people injured") %>% 
  mutate(Country = "Somalia") %>% 
  select(Date, Country, Province,`US Confirmed`,`Minimum Strikes`,`Maximum Strikes`,`Minimum Total Killed`,
         `Maximum Total Killed`,`Minimum Civilians Killed`,`Maximum Civilians Killed`,`Minimum Children Killed`,
         `Maximum Children Killed`,`Minimum Injured`,`Maximum Injured`)


# Yemen
yemen <- 
  yemen %>% 
  rename(`US Confirmed`="Confirmed/\r\npossible US attack?",
         `Minimum Strikes` = "Minimum number of strikes",
         `Maximum Strikes` = "Maximum number of strikes",
         `Minimum Total Killed` = "Minimum people killed",
         `Maximum Total Killed` = "Maximum people killed",
         `Minimum Civilians Killed` = "Minimum civilians reported killed",
         `Maximum Civilians Killed` = "Maximum civilians reported killed",
         `Minimum Children Killed` = "Minimum children reported killed",
         `Maximum Children Killed` = "Maximum children reported killed",
         `Minimum Injured` = "Minimum people injured",
         `Maximum Injured` = "Maximum people injured") %>% 
  mutate(Country = "Yemen") %>% 
  select(Date, Country, Province,`US Confirmed`,`Minimum Strikes`,`Maximum Strikes`,`Minimum Total Killed`,
         `Maximum Total Killed`,`Minimum Civilians Killed`,`Maximum Civilians Killed`,`Minimum Children Killed`,
         `Maximum Children Killed`,`Minimum Injured`,`Maximum Injured`)

# Integrate All Country into one
drone <- rbind(afghanistan,pakistan,yemen,somalia) %>% 
  arrange(Date) %>% 
  mutate( Presidency = case_when(
    Date <= "2009-01-20" ~ "George W. Bush",
    Date <= "2017-01-20" ~ "Barack Obama",
    Date <= "2021-01-20" ~ "Donald J. Trump"))

# -------------------------- PRESIDENCY OVERVIEW

# -------------- Bush

bush.strikes <- 
  drone %>% 
  filter(Presidency %in% "George W. Bush") %>% 
  select(`Maximum Strikes`) %>% 
  summarise(Total = sum(`Maximum Strikes`))

bush.kill <- 
  drone %>% 
  filter(Presidency %in% "George W. Bush") %>% 
  select(`Minimum Total Killed`, `Maximum Total Killed`) %>% 
  summarise(Min = sum(`Minimum Total Killed`),
            Max = sum(`Maximum Total Killed`))

bush.civs <- 
  drone %>% 
  filter(Presidency %in% "George W. Bush") %>% 
  select(`Minimum Civilians Killed`, `Maximum Civilians Killed`) %>% 
  summarise(Min = sum(`Minimum Civilians Killed`),
            Max = sum(`Maximum Civilians Killed`))

bush.child <- 
  drone %>% 
  filter(Presidency %in% "George W. Bush") %>% 
  select(`Minimum Children Killed`, `Maximum Children Killed`) %>% 
  summarise(Min = sum(`Minimum Children Killed`),
            Max = sum(`Maximum Children Killed`))

bush.injured <- 
  drone %>% 
  filter(Presidency %in% "George W. Bush") %>% 
  select(`Minimum Injured`, `Maximum Injured`) %>% 
  summarise(Min = sum(`Minimum Injured`),
            Max = sum(`Maximum Injured`))

# Collateral Damage Rate
bush.rate <- (bush.civs$Max + bush.child$Max) / bush.kill$Max

# -------------- Obama

obama.strikes <- 
  drone %>% 
  filter(Presidency %in% "Barack Obama") %>% 
  select(`Maximum Strikes`) %>% 
  summarise(Total = sum(`Maximum Strikes`))

obama.kill <- 
  drone %>% 
  filter(Presidency %in% "Barack Obama") %>% 
  select(`Minimum Total Killed`, `Maximum Total Killed`) %>% 
  summarise(Min = sum(`Minimum Total Killed`),
            Max = sum(`Maximum Total Killed`))

obama.civs <- 
  drone %>% 
  filter(Presidency %in% "Barack Obama") %>% 
  select(`Minimum Civilians Killed`, `Maximum Civilians Killed`) %>% 
  summarise(Min = sum(`Minimum Civilians Killed`),
            Max = sum(`Maximum Civilians Killed`))

obama.child <- 
  drone %>% 
  filter(Presidency %in% "Barack Obama") %>% 
  select(`Minimum Children Killed`, `Maximum Children Killed`) %>% 
  summarise(Min = sum(`Minimum Children Killed`),
            Max = sum(`Maximum Children Killed`))

obama.injured <- 
  drone %>% 
  filter(Presidency %in% "Barack Obama") %>% 
  select(`Minimum Injured`, `Maximum Injured`) %>% 
  summarise(Min = sum(`Minimum Injured`),
            Max = sum(`Maximum Injured`))

# Collateral Damage Rate
obama.rate <- (obama.civs$Max + obama.child$Max) / obama.kill$Max

# -------------- Trump

trump.strikes <- 
  drone %>% 
  filter(Presidency %in% "Donald J. Trump") %>% 
  select(`Maximum Strikes`) %>% 
  summarise(Total = sum(`Maximum Strikes`))

trump.kill <- 
  drone %>% 
  filter(Presidency %in% "Donald J. Trump") %>% 
  select(`Minimum Total Killed`, `Maximum Total Killed`) %>% 
  summarise(Min = sum(`Minimum Total Killed`),
            Max = sum(`Maximum Total Killed`))

trump.civs <- 
  drone %>% 
  filter(Presidency %in% "Donald J. Trump") %>% 
  select(`Minimum Civilians Killed`, `Maximum Civilians Killed`) %>% 
  summarise(Min = sum(`Minimum Civilians Killed`),
            Max = sum(`Maximum Civilians Killed`))

trump.child <- 
  drone %>% 
  filter(Presidency %in% "Donald J. Trump") %>% 
  select(`Minimum Children Killed`, `Maximum Children Killed`) %>% 
  summarise(Min = sum(`Minimum Children Killed`),
            Max = sum(`Maximum Children Killed`))

trump.injured <- 
  drone %>% 
  filter(Presidency %in% "Donald J. Trump") %>% 
  select(`Minimum Injured`, `Maximum Injured`) %>% 
  summarise(Min = sum(`Minimum Injured`),
            Max = sum(`Maximum Injured`))

# Collateral Damage Rate
trump.rate <- (trump.civs$Max + trump.child$Max) / trump.kill$Max

# -------------- Map Leaflet

afghan <- drone %>% 
  filter(Country %in% "Afghanistan")
pakistan <- drone %>% 
  filter(Country %in% "Pakistan")
somalia <- drone %>% 
  filter(Country %in% "Somalia")
yemen <- drone %>% 
  filter(Country %in% "Yemen")

# Read Data
afghan_json <- geojson_read("datainput/clean/gadm36_AFG_1.json", what = "sp")
pakistan_json <- geojson_read("datainput/clean/gadm36_PAK_1.json", what = "sp")
somalia_json <- geojson_read("datainput/clean/gadm36_SOM_1.json", what = "sp")
yemen_json <- geojson_read("datainput/clean/gadm36_YEM_1.json", what = "sp")

# Change the data type to `sf`
afghan_json_mod <- sf::st_as_sf(afghan_json)
pakistan_json_mod <- sf::st_as_sf(pakistan_json)
somalia_json_mod <- sf::st_as_sf(somalia_json)
yemen_json_mod <- sf::st_as_sf(yemen_json)

# Remove Unecessary Columns
afghan_json_mod <- 
  afghan_json_mod %>% 
  select(-c(id, NL_NAME_1,  VARNAME_1, HASC_1, TYPE_1, ENGTYPE_1))

pakistan_json_mod <- 
  pakistan_json_mod %>% 
  select(-c(id, NL_NAME_1,  VARNAME_1, HASC_1, TYPE_1, ENGTYPE_1))

somalia_json_mod <- 
  somalia_json_mod %>% 
  select(-c(id, NL_NAME_1,  VARNAME_1, HASC_1, TYPE_1, ENGTYPE_1, GID_0, NAME_0))

yemen_json_mod <- 
  yemen_json_mod %>% 
  select(-c(id, NL_NAME_1,  VARNAME_1, HASC_1, TYPE_1, ENGTYPE_1, GID_0, NAME_0))

# Match Province Name with NAME_1 from json_mod
afghan <- 
  afghan %>% 
  mutate( Province = ifelse(grepl("Badakhshan|Badakshan", afghan$Province) == TRUE, "Badakhshan",
                            ifelse(grepl("Badghis", afghan$Province) == TRUE, "Badghis",
                                   ifelse(grepl("Baghlan", afghan$Province) == TRUE, "Baghlan",
                                          ifelse(grepl("Balkh", afghan$Province) == TRUE, "Balkh",
                                                 ifelse(grepl("Farah and Nimroz", afghan$Province) == TRUE, "Farah",
                                                        ifelse(grepl("Ghazni, Helmand and Uruzgan", afghan$Province) == TRUE, "Ghazni",
                                                               ifelse(grepl("Helmand", afghan$Province) == TRUE, "Hilmand",
                                                                      ifelse(grepl("Herat", afghan$Province) == TRUE, "Hirat",
                                                                             ifelse(grepl("Jawzjan|Jowzjan", afghan$Province) == TRUE, "Jawzjan",
                                                                                    ifelse(grepl("Nangarhar and Paktika", afghan$Province) == TRUE, "Nangarhar",
                                                                                           ifelse(grepl("Pakita|Pakitka|Paktia|Paktika", afghan$Province) == TRUE, "Paktika",
                                                                                                  ifelse(grepl("Paktiya", afghan$Province) == TRUE, "Paktya",
                                                                                                         ifelse(grepl("Sar e Pul", afghan$Province) == TRUE, "Sari Pul",
                                                                                                                ifelse(grepl("Unknown|-", afghan$Province) == TRUE, "Balkh",
                                                                                                                       ifelse(grepl("Bamyan", afghan$Province) == TRUE, "Bamyan",
                                                                                                                              ifelse(grepl("Daykundi", afghan$Province) == TRUE, "Daykundi",
                                                                                                                                     ifelse(grepl("Faryab", afghan$Province) == TRUE, "Faryab",
                                                                                                                                            ifelse(grepl("Ghor", afghan$Province) == TRUE, "Ghor",
                                                                                                                                                   ifelse(grepl("Kabul", afghan$Province) == TRUE, "Kabul",
                                                                                                                                                          ifelse(grepl("Kandahar", afghan$Province) == TRUE, "Kandahar",
                                                                                                                                                                 ifelse(grepl("Kapisa", afghan$Province) == TRUE, "Kapisa",
                                                                                                                                                                        ifelse(grepl("Khost", afghan$Province) == TRUE, "Khost",
                                                                                                                                                                               ifelse(grepl("Kunar", afghan$Province) == TRUE, "Kunar",
                                                                                                                                                                                      ifelse(grepl("Kunduz", afghan$Province) == TRUE, "Kunduz",
                                                                                                                                                                                             ifelse(grepl("Laghman", afghan$Province) == TRUE, "Laghman",
                                                                                                                                                                                                    ifelse(grepl("Logar", afghan$Province) == TRUE, "Logar",
                                                                                                                                                                                                           ifelse(grepl("Nangarhar", afghan$Province) == TRUE, "Nangarhar",
                                                                                                                                                                                                                  ifelse(grepl("Nimroz", afghan$Province) == TRUE, "Nimroz",
                                                                                                                                                                                                                         ifelse(grepl("Nuristan", afghan$Province) == TRUE, "Nuristan",
                                                                                                                                                                                                                                ifelse(grepl("Panjshir", afghan$Province) == TRUE, "Panjshir",
                                                                                                                                                                                                                                       ifelse(grepl("Parwan", afghan$Province) == TRUE, "Parwan",
                                                                                                                                                                                                                                              ifelse(grepl("Samangan", afghan$Province) == TRUE, "Samangan",
                                                                                                                                                                                                                                                     ifelse(grepl("Takhar", afghan$Province) == TRUE, "Takhar",
                                                                                                                                                                                                                                                            ifelse(grepl("Uruzgan", afghan$Province) == TRUE, "Uruzgan",
                                                                                                                                                                                                                                                                   ifelse(grepl("Wardak", afghan$Province) == TRUE, "Wardak",
                                                                                                                                                                                                                                                                          ifelse(grepl("Zabul", afghan$Province) == TRUE, "Zabul","Nimroz")))))))))))))))))))))))))))))))))))))

pakistan <- 
  pakistan %>% 
  mutate(Province = ifelse(grepl("Khyber Pakhtunkhwa|Badakshan", pakistan$Province) == TRUE, "F.A.T.A.","N.W.F.P."))

somalia <- 
  somalia %>% 
  mutate(Province = replace_na(Province, replace = "Bari"))

yemen <- 
  yemen %>% 
  mutate( Province = ifelse(grepl("Abyan", yemen$Province) == TRUE, "Abyan",
                            ifelse(grepl("Badya|Bayda|Bayda-Shabwa border", yemen$Province) == TRUE, "Al Bayda",
                                   ifelse(grepl("Across central Yemen|Across Yemen|Between Marib and Shabwa|Central Yemen|Mareb|Marib|Marib/Al Jawf|Multiple provinces|Shabwa/Mareb border", yemen$Province) == TRUE, "Ma'rib",
                                          ifelse(grepl("Damar", yemen$Province) == TRUE, "Dhamar",
                                                 ifelse(grepl("Hadaramout|Hadhramout|Hadramout", yemen$Province) == TRUE, "Hadramawt",
                                                        ifelse(grepl("Jawf|Jawf-Mareb border", yemen$Province) == TRUE, "Al Jawf",
                                                               ifelse(grepl("Lahij|Lahj", yemen$Province) == TRUE, "Lahij",
                                                                      ifelse(grepl("Saada", yemen$Province) == TRUE, "Sa`dah",
                                                                             ifelse(grepl("Sanaa", yemen$Province) == TRUE, "San`a",
                                                                                    ifelse(grepl("Southern Yemen", yemen$Province) == TRUE, "Ta`izz",
                                                                                           ifelse(grepl("Shabwa|Shabwa/Hadramout|Shabwa/Mareb border|Shabwah", yemen$Province) == TRUE, "Shabwah", "Ma'rib"))))))))))))



# Subset the Data, we want to create map based on Strikes intensity
afghan_mod <- afghan %>% 
  group_by(Province) %>% 
  summarize(Strikes = n(),
            Death = sum(`Maximum Total Killed`)) %>% 
  ungroup()

pakistan_mod <- pakistan %>% 
  group_by(Province) %>% 
  summarize(Strikes = n(),
            Death = sum(`Maximum Total Killed`)) %>% 
  ungroup()

somalia_mod <- somalia %>% 
  group_by(Province) %>% 
  summarize(Strikes = n(),
            Death = sum(`Maximum Total Killed`)) %>% 
  ungroup()

yemen_mod <- yemen %>% 
  group_by(Province) %>% 
  summarize(Strikes = n(),
            Death = sum(`Maximum Total Killed`)) %>% 
  ungroup()

# Natural distribution for our Strikes
aq0 <- quantile(afghan_mod$Strikes)[1] 
aq25 <- quantile(afghan_mod$Strikes)[2]
aq50 <- quantile(afghan_mod$Strikes)[3]
aq75 <- quantile(afghan_mod$Strikes)[4]
aq100 <- quantile(afghan_mod$Strikes)[5]

pq0 <- quantile(pakistan_mod$Strikes)[1] 
pq25 <- quantile(pakistan_mod$Strikes)[2]
pq50 <- quantile(pakistan_mod$Strikes)[3]
pq75 <- quantile(pakistan_mod$Strikes)[4]
pq100 <- quantile(pakistan_mod$Strikes)[5]

sq0 <- quantile(somalia_mod$Strikes)[1] 
sq25 <- quantile(somalia_mod$Strikes)[2]
sq50 <- quantile(somalia_mod$Strikes)[3]
sq75 <- quantile(somalia_mod$Strikes)[4]
sq100 <- quantile(somalia_mod$Strikes)[5]

yq0 <- quantile(yemen_mod$Strikes)[1] 
yq25 <- quantile(yemen_mod$Strikes)[2]
yq50 <- quantile(yemen_mod$Strikes)[3]
yq75 <- quantile(yemen_mod$Strikes)[4]
yq100 <- quantile(yemen_mod$Strikes)[5]

convert_strikes_a = function(x){
  if(x <= aq25) {x <- "Low"}
  else if(x <= aq50) {x <- "Medium"}
  else if(x <= aq75) {x <- "High"}
  else {x <- "Very High"}
}

convert_strikes_p = function(x){
  if(x <= pq25) {x <- "Low"}
  else if(x <= pq50) {x <- "Medium"}
  else if(x <= pq75) {x <- "High"}
  else {x <- "Very High"}
}

convert_strikes_s = function(x){
  if(x <= sq25) {x <- "Low"}
  else if(x <= sq50) {x <- "Medium"}
  else if(x <= sq75) {x <- "High"}
  else {x <- "Very High"}
}

convert_strikes_y = function(x){
  if(x <= yq25) {x <- "Low"}
  else if(x <= yq50) {x <- "Medium"}
  else if(x <= yq75) {x <- "High"}
  else {x <- "Very High"}
}

afghan_mod$Intensity <- sapply(X = afghan_mod$Strikes, 
                               FUN = convert_strikes_a)

pakistan_mod$Intensity <- sapply(X = pakistan_mod$Strikes, 
                                 FUN = convert_strikes_p)

somalia_mod$Intensity <- sapply(X = somalia_mod$Strikes, 
                                FUN = convert_strikes_s)

yemen_mod$Intensity <- sapply(X = yemen_mod$Strikes, 
                              FUN = convert_strikes_y)

# Join the json and the data
afghani <- afghan_json_mod %>% 
  left_join(afghan_mod, by = c("NAME_1" = "Province")) %>% 
  complete(NAME_1) %>% 
  mutate(Strikes = replace_na(Strikes, replace = 0),
         Death = replace_na(Death, replace = 0),
         Intensity = replace_na(Intensity, replace = "None")) %>% 
  select(-CC_1)

pakistani <- pakistan_json_mod %>% 
  left_join(pakistan_mod, by = c("NAME_1" = "Province")) %>% 
  complete(NAME_1) %>% 
  mutate(Strikes = replace_na(Strikes, replace = 0),
         Death = replace_na(Death, replace = 0),
         Intensity = replace_na(Intensity, replace = "None")) %>% 
  select(-CC_1)

somaliani <- somalia_json_mod %>% 
  left_join(somalia_mod, by = c("NAME_1" = "Province")) %>% 
  complete(NAME_1) %>% 
  mutate(Strikes = replace_na(Strikes, replace = 0),
         Death = replace_na(Death, replace = 0),
         Intensity = replace_na(Intensity, replace = "None")) %>% 
  select(-CC_1)

yemeni <- yemen_json_mod %>% 
  left_join(yemen_mod, by = c("NAME_1" = "Province")) %>% 
  complete(NAME_1) %>% 
  mutate(Strikes = replace_na(Strikes, replace = 0),
         Death = replace_na(Death, replace = 0),
         Intensity = replace_na(Intensity, replace = "None")) %>% 
  select(-CC_1)

#Convert to sf again
afghan_sf <- afghani %>% sf::st_as_sf()
pakistan_sf <- pakistani %>% sf::st_as_sf()
somalia_sf <- somaliani %>% sf::st_as_sf()
yemen_sf <- yemeni %>% sf::st_as_sf()


#Set the Aesthetics
afghan_sf$Intensity <- as.factor(afghan_sf$Intensity)
afghan_sf$Intensity <- factor(afghan_sf$Intensity,levels = c("Very High", "High", "Medium", "Low", "None"))
factpal.a <- colorFactor(c("#A6655F", "#FABF78", "#F5DB9E", "#C3B299","#5A5981"), afghan_sf$Intensity)

popup.cont.a  <- paste("<h4><b>", afghan_sf$NAME_1, "</b></h4>", 
                       "Intensity: ", afghan_sf$Intensity, "<br>", 
                       "Strikes   : ", afghan_sf$Strikes, "<br>",
                       "Death   : ", afghan_sf$Death, "<br>",
                       sep="")

pakistan_sf$Intensity <- as.factor(pakistan_sf$Intensity)
pakistan_sf$Intensity <- factor(pakistan_sf$Intensity,levels = c("Very High", "High", "Medium", "Low", "None"))
factpal.p <- colorFactor(c("#A6655F", "#FABF78", "#F5DB9E", "#C3B299","#5A5981"), pakistan_sf$Intensity)

popup.cont.p  <- paste("<h4><b>", pakistan_sf$NAME_1, "</b></h4>", 
                       "Intensity: ", pakistan_sf$Intensity, "<br>", 
                       "Strikes   : ", pakistan_sf$Strikes, "<br>",
                       "Death   : ", pakistan_sf$Death, "<br>",
                       sep="")

somalia_sf$Intensity <- as.factor(somalia_sf$Intensity)
somalia_sf$Intensity <- factor(somalia_sf$Intensity,levels = c("Very High", "High", "Medium", "Low", "None"))
factpal.s <- colorFactor(c("#A6655F", "#FABF78", "#F5DB9E", "#C3B299","#5A5981"), somalia_sf$Intensity)

popup.cont.s  <- paste("<h4><b>", somalia_sf$NAME_1, "</b></h4>", 
                       "Intensity: ", somalia_sf$Intensity, "<br>", 
                       "Strikes   : ", somalia_sf$Strikes, "<br>",
                       "Death   : ", somalia_sf$Death, "<br>",
                       sep="")

yemen_sf$Intensity <- as.factor(yemen_sf$Intensity)
yemen_sf$Intensity <- factor(yemen_sf$Intensity,levels = c("Very High", "High", "Medium", "Low", "None"))
factpal.y <- colorFactor(c("#A6655F", "#FABF78", "#F5DB9E", "#C3B299","#5A5981"), yemen_sf$Intensity)

popup.cont.y  <- paste("<h4><b>", yemen_sf$NAME_1, "</b></h4>", 
                       "Intensity: ", yemen_sf$Intensity, "<br>", 
                       "Strikes   : ", yemen_sf$Strikes, "<br>",
                       "Death   : ", yemen_sf$Death, "<br>",
                       sep="")













