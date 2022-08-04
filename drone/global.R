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

# -------------- READ DATA

afghanistan <- read_excel("datainput/afghanstrike.xlsx")
pakistan <- read_excel("datainput/pakistanstrike.xlsx")
somalia <- read_excel("datainput/somaliastrike.xlsx")
yemen <- read_excel("datainput/yemenstrike.xlsx")

# -------------- DATA PREPARATION

#Afghanistan
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

#Change the Confirmed Value to 1 = Confirmed, 0 = Possible
afghanistan <- 
  afghanistan %>%
  mutate(`US Confirmed` = ifelse(grepl("1", afghanistan$`US Confirmed`) == TRUE, "Confirmed","Possible"))

#Pakistan
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

#Change the Confirmed Value to 1 = Confirmed, 0 = Possible
pakistan <- 
  pakistan %>%
  mutate(`US Confirmed` = ifelse(grepl("1", pakistan$`US Confirmed`) == TRUE, "Confirmed","Possible"))

#Somalia Region
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

#Somalia
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


#Yemen
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

#Integrate All Country into one
drone <- rbind(afghanistan,pakistan,yemen,somalia) %>% 
  arrange(Date) %>% 
  mutate( Presidency = case_when(
    Date <= "2009-01-20" ~ "George W. Bush",
    Date <= "2017-01-20" ~ "Barack Obama",
    Date <= "2021-01-20" ~ "Donald J. Trump"))

# -------------- PRESIDENCY OVERVIEW

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

#Collateral Damage Rate
bush.rate <- (bush.civs$Max + bush.child$Max) / bush.kill$Max

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

#Collateral Damage Rate
obama.rate <- (obama.civs$Max + obama.child$Max) / obama.kill$Max

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

#Collateral Damage Rate
trump.rate <- (trump.civs$Max + trump.child$Max) / trump.kill$Max























