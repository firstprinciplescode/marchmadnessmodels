library(readxl)
library(openxlsx)
library(tidyverse)

gamelog_2016 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output16.xlsx")

gamelog_2016$G <- as.numeric(gamelog_2016$G)
gamelog_2016$Date <- as.Date(gamelog_2016$Date)

gamelog_2016 <- gamelog_2016 %>%
  mutate(across(8:41, as.numeric))


str(gamelog_2016)


matchup_reference_2016 <- gamelog_2016 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2016 <- gamelog_2016 %>%
  left_join(matchup_reference_2016,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2016$Location <- ifelse(is.na(gamelog_2016$Location), "H", gamelog_2016$Location)
gamelog_2016$Location <- ifelse(gamelog_2016$Location == "H", "H", "A")

gamelog_2016$OppLocation <- ifelse(is.na(gamelog_2016$OppLocation), "H", gamelog_2016$OppLocation)
gamelog_2016$OppLocation <- ifelse(gamelog_2016$OppLocation == "H", "H", "A")


gamelog_2016$DRB <- gamelog_2016$TRB - gamelog_2016$ORB
gamelog_2016$oppDRB <- gamelog_2016$oppTRB - gamelog_2016$oppORB

gamelog_2016$ORB_perc <- gamelog_2016$ORB / (gamelog_2016$ORB + gamelog_2016$oppDRB)
gamelog_2016$oppORB_perc <- gamelog_2016$oppORB / (gamelog_2016$oppORB + gamelog_2016$DRB)

gamelog_2016$poss <- gamelog_2016$FGA - gamelog_2016$ORB + gamelog_2016$TOV + (.5 * gamelog_2016$FTA)
gamelog_2016$oppposs <- gamelog_2016$oppFGA - gamelog_2016$oppORB + gamelog_2016$oppTOV + (.5 * gamelog_2016$oppFTA)

gamelog_2016$a_fgm <- gamelog_2016$AST / gamelog_2016$FG
gamelog_2016$opp_a_fgm <- gamelog_2016$oppAST / gamelog_2016$oppFG

gamelog_2016$`2P` <- gamelog_2016$FG - gamelog_2016$`3P`
gamelog_2016$`2PA` <- gamelog_2016$FGA - gamelog_2016$`3PA`
gamelog_2016$`2Pperc` <- gamelog_2016$`2P` / gamelog_2016$`2PA`

gamelog_2016$opp2P <- gamelog_2016$oppFG - gamelog_2016$opp3P
gamelog_2016$opp2PA <- gamelog_2016$oppFGA - gamelog_2016$opp3PA
gamelog_2016$opp2Pperc <- gamelog_2016$opp2P / gamelog_2016$opp2PA

gamelog_2016$`3PRate` <- gamelog_2016$`3PA` / gamelog_2016$FGA
gamelog_2016$opp3PRate <- gamelog_2016$opp3PA / gamelog_2016$oppFGA

gamelog_2016$BLK_perc <- gamelog_2016$oppBLK / gamelog_2016$`2PA`
gamelog_2016$oppBLK_perc <- gamelog_2016$BLK / gamelog_2016$`opp2PA`

gamelog_2016$FL_perc <- (gamelog_2016$FG + gamelog_2016$ORB) / (gamelog_2016$FGA + gamelog_2016$TOV)
gamelog_2016$oppFL_perc <- (gamelog_2016$oppFG + gamelog_2016$oppORB) / (gamelog_2016$oppFGA + gamelog_2016$oppTOV)

gamelog_2016$TOV_perc <- gamelog_2016$TOV / gamelog_2016$poss
gamelog_2016$oppTOV_perc <- gamelog_2016$oppTOV / gamelog_2016$oppposs

gamelog_2016$STL_perc <- gamelog_2016$oppSTL / gamelog_2016$poss
gamelog_2016$oppSTL_perc <- gamelog_2016$STL / gamelog_2016$oppposs

gamelog_2016$FTR <- gamelog_2016$FTA / gamelog_2016$FGA
gamelog_2016$oppFTR <- gamelog_2016$oppFTA / gamelog_2016$oppFGA

gamelog_2016$PPP <- gamelog_2016$Tm / gamelog_2016$poss
gamelog_2016$oppPPP <- gamelog_2016$OppScore / gamelog_2016$oppposs

gamelog_2016 <- gamelog_2016 %>% arrange(Conference, Team, G)


gamelog_2016 %>% group_by(GameType) %>%
  dplyr::summarise(minDate = min(Date)) %>% arrange(minDate)
# 2016-03-14


#####
#####
#####



gamelog_2017 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output17.xlsx")

gamelog_2017$G <- as.numeric(gamelog_2017$G)
gamelog_2017$Date <- as.Date(gamelog_2017$Date)

gamelog_2017 <- gamelog_2017 %>%
  mutate(across(8:41, as.numeric))

str(gamelog_2017)


matchup_reference_2017 <- gamelog_2017 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2017 <- gamelog_2017 %>%
  left_join(matchup_reference_2017,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2017$Location <- ifelse(is.na(gamelog_2017$Location), "H", gamelog_2017$Location)
gamelog_2017$Location <- ifelse(gamelog_2017$Location == "H", "H", "A")

gamelog_2017$OppLocation <- ifelse(is.na(gamelog_2017$OppLocation), "H", gamelog_2017$OppLocation)
gamelog_2017$OppLocation <- ifelse(gamelog_2017$OppLocation == "H", "H", "A")


gamelog_2017$DRB <- gamelog_2017$TRB - gamelog_2017$ORB
gamelog_2017$oppDRB <- gamelog_2017$oppTRB - gamelog_2017$oppORB

gamelog_2017$ORB_perc <- gamelog_2017$ORB / (gamelog_2017$ORB + gamelog_2017$oppDRB)
gamelog_2017$oppORB_perc <- gamelog_2017$oppORB / (gamelog_2017$oppORB + gamelog_2017$DRB)

gamelog_2017$poss <- gamelog_2017$FGA - gamelog_2017$ORB + gamelog_2017$TOV + (.5 * gamelog_2017$FTA)
gamelog_2017$oppposs <- gamelog_2017$oppFGA - gamelog_2017$oppORB + gamelog_2017$oppTOV + (.5 * gamelog_2017$oppFTA)

gamelog_2017$a_fgm <- gamelog_2017$AST / gamelog_2017$FG
gamelog_2017$opp_a_fgm <- gamelog_2017$oppAST / gamelog_2017$oppFG

gamelog_2017$`2P` <- gamelog_2017$FG - gamelog_2017$`3P`
gamelog_2017$`2PA` <- gamelog_2017$FGA - gamelog_2017$`3PA`
gamelog_2017$`2Pperc` <- gamelog_2017$`2P` / gamelog_2017$`2PA`

gamelog_2017$opp2P <- gamelog_2017$oppFG - gamelog_2017$opp3P
gamelog_2017$opp2PA <- gamelog_2017$oppFGA - gamelog_2017$opp3PA
gamelog_2017$opp2Pperc <- gamelog_2017$opp2P / gamelog_2017$opp2PA

gamelog_2017$`3PRate` <- gamelog_2017$`3PA` / gamelog_2017$FGA
gamelog_2017$opp3PRate <- gamelog_2017$opp3PA / gamelog_2017$oppFGA

gamelog_2017$BLK_perc <- gamelog_2017$oppBLK / gamelog_2017$`2PA`
gamelog_2017$oppBLK_perc <- gamelog_2017$BLK / gamelog_2017$`opp2PA`

gamelog_2017$FL_perc <- (gamelog_2017$FG + gamelog_2017$ORB) / (gamelog_2017$FGA + gamelog_2017$TOV)
gamelog_2017$oppFL_perc <- (gamelog_2017$oppFG + gamelog_2017$oppORB) / (gamelog_2017$oppFGA + gamelog_2017$oppTOV)

gamelog_2017$TOV_perc <- gamelog_2017$TOV / gamelog_2017$poss
gamelog_2017$oppTOV_perc <- gamelog_2017$oppTOV / gamelog_2017$oppposs

gamelog_2017$STL_perc <- gamelog_2017$oppSTL / gamelog_2017$poss
gamelog_2017$oppSTL_perc <- gamelog_2017$STL / gamelog_2017$oppposs

gamelog_2017$FTR <- gamelog_2017$FTA / gamelog_2017$FGA
gamelog_2017$oppFTR <- gamelog_2017$oppFTA / gamelog_2017$oppFGA

gamelog_2017$PPP <- gamelog_2017$Tm / gamelog_2017$poss
gamelog_2017$oppPPP <- gamelog_2017$OppScore / gamelog_2017$oppposs

gamelog_2017 <- gamelog_2017 %>% arrange(Conference, Team, G)

gamelog_2017 %>% group_by(GameType) %>%
  dplyr::summarise(minDate = min(Date)) %>% arrange(minDate)
# 2017-03-13


#####
#####
#####



gamelog_2018 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output18.xlsx")

gamelog_2018$G <- as.numeric(gamelog_2018$G)
gamelog_2018$Date <- as.Date(gamelog_2018$Date)

gamelog_2018 <- gamelog_2018 %>%
  mutate(across(8:41, as.numeric))


str(gamelog_2018)


matchup_reference_2018 <- gamelog_2018 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2018 <- gamelog_2018 %>%
  left_join(matchup_reference_2018,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2018$Location <- ifelse(is.na(gamelog_2018$Location), "H", gamelog_2018$Location)
gamelog_2018$Location <- ifelse(gamelog_2018$Location == "H", "H", "A")

gamelog_2018$OppLocation <- ifelse(is.na(gamelog_2018$OppLocation), "H", gamelog_2018$OppLocation)
gamelog_2018$OppLocation <- ifelse(gamelog_2018$OppLocation == "H", "H", "A")


gamelog_2018$DRB <- gamelog_2018$TRB - gamelog_2018$ORB
gamelog_2018$oppDRB <- gamelog_2018$oppTRB - gamelog_2018$oppORB

gamelog_2018$ORB_perc <- gamelog_2018$ORB / (gamelog_2018$ORB + gamelog_2018$oppDRB)
gamelog_2018$oppORB_perc <- gamelog_2018$oppORB / (gamelog_2018$oppORB + gamelog_2018$DRB)

gamelog_2018$poss <- gamelog_2018$FGA - gamelog_2018$ORB + gamelog_2018$TOV + (.5 * gamelog_2018$FTA)
gamelog_2018$oppposs <- gamelog_2018$oppFGA - gamelog_2018$oppORB + gamelog_2018$oppTOV + (.5 * gamelog_2018$oppFTA)

gamelog_2018$a_fgm <- gamelog_2018$AST / gamelog_2018$FG
gamelog_2018$opp_a_fgm <- gamelog_2018$oppAST / gamelog_2018$oppFG

gamelog_2018$`2P` <- gamelog_2018$FG - gamelog_2018$`3P`
gamelog_2018$`2PA` <- gamelog_2018$FGA - gamelog_2018$`3PA`
gamelog_2018$`2Pperc` <- gamelog_2018$`2P` / gamelog_2018$`2PA`

gamelog_2018$opp2P <- gamelog_2018$oppFG - gamelog_2018$opp3P
gamelog_2018$opp2PA <- gamelog_2018$oppFGA - gamelog_2018$opp3PA
gamelog_2018$opp2Pperc <- gamelog_2018$opp2P / gamelog_2018$opp2PA

gamelog_2018$`3PRate` <- gamelog_2018$`3PA` / gamelog_2018$FGA
gamelog_2018$opp3PRate <- gamelog_2018$opp3PA / gamelog_2018$oppFGA

gamelog_2018$BLK_perc <- gamelog_2018$oppBLK / gamelog_2018$`2PA`
gamelog_2018$oppBLK_perc <- gamelog_2018$BLK / gamelog_2018$`opp2PA`

gamelog_2018$FL_perc <- (gamelog_2018$FG + gamelog_2018$ORB) / (gamelog_2018$FGA + gamelog_2018$TOV)
gamelog_2018$oppFL_perc <- (gamelog_2018$oppFG + gamelog_2018$oppORB) / (gamelog_2018$oppFGA + gamelog_2018$oppTOV)

gamelog_2018$TOV_perc <- gamelog_2018$TOV / gamelog_2018$poss
gamelog_2018$oppTOV_perc <- gamelog_2018$oppTOV / gamelog_2018$oppposs

gamelog_2018$STL_perc <- gamelog_2018$oppSTL / gamelog_2018$poss
gamelog_2018$oppSTL_perc <- gamelog_2018$STL / gamelog_2018$oppposs

gamelog_2018$FTR <- gamelog_2018$FTA / gamelog_2018$FGA
gamelog_2018$oppFTR <- gamelog_2018$oppFTA / gamelog_2018$oppFGA

gamelog_2018$PPP <- gamelog_2018$Tm / gamelog_2018$poss
gamelog_2018$oppPPP <- gamelog_2018$OppScore / gamelog_2018$oppposs

gamelog_2018 <- gamelog_2018 %>% arrange(Conference, Team, G)

gamelog_2018 %>% group_by(GameType) %>%
  dplyr::summarise(minDate = min(Date)) %>% arrange(minDate)
# 2018-03-12


#####
#####
#####



gamelog_2019 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output19.xlsx")

gamelog_2019$G <- as.numeric(gamelog_2019$G)
gamelog_2019$Date <- as.Date(gamelog_2019$Date)

gamelog_2019 <- gamelog_2019 %>%
  mutate(across(8:41, as.numeric))


str(gamelog_2019)


matchup_reference_2019 <- gamelog_2019 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2019 <- gamelog_2019 %>%
  left_join(matchup_reference_2019,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2019$Location <- ifelse(is.na(gamelog_2019$Location), "H", gamelog_2019$Location)
gamelog_2019$Location <- ifelse(gamelog_2019$Location == "H", "H", "A")

gamelog_2019$OppLocation <- ifelse(is.na(gamelog_2019$OppLocation), "H", gamelog_2019$OppLocation)
gamelog_2019$OppLocation <- ifelse(gamelog_2019$OppLocation == "H", "H", "A")


gamelog_2019$DRB <- gamelog_2019$TRB - gamelog_2019$ORB
gamelog_2019$oppDRB <- gamelog_2019$oppTRB - gamelog_2019$oppORB

gamelog_2019$ORB_perc <- gamelog_2019$ORB / (gamelog_2019$ORB + gamelog_2019$oppDRB)
gamelog_2019$oppORB_perc <- gamelog_2019$oppORB / (gamelog_2019$oppORB + gamelog_2019$DRB)

gamelog_2019$poss <- gamelog_2019$FGA - gamelog_2019$ORB + gamelog_2019$TOV + (.5 * gamelog_2019$FTA)
gamelog_2019$oppposs <- gamelog_2019$oppFGA - gamelog_2019$oppORB + gamelog_2019$oppTOV + (.5 * gamelog_2019$oppFTA)

gamelog_2019$a_fgm <- gamelog_2019$AST / gamelog_2019$FG
gamelog_2019$opp_a_fgm <- gamelog_2019$oppAST / gamelog_2019$oppFG

gamelog_2019$`2P` <- gamelog_2019$FG - gamelog_2019$`3P`
gamelog_2019$`2PA` <- gamelog_2019$FGA - gamelog_2019$`3PA`
gamelog_2019$`2Pperc` <- gamelog_2019$`2P` / gamelog_2019$`2PA`

gamelog_2019$opp2P <- gamelog_2019$oppFG - gamelog_2019$opp3P
gamelog_2019$opp2PA <- gamelog_2019$oppFGA - gamelog_2019$opp3PA
gamelog_2019$opp2Pperc <- gamelog_2019$opp2P / gamelog_2019$opp2PA

gamelog_2019$`3PRate` <- gamelog_2019$`3PA` / gamelog_2019$FGA
gamelog_2019$opp3PRate <- gamelog_2019$opp3PA / gamelog_2019$oppFGA

gamelog_2019$BLK_perc <- gamelog_2019$oppBLK / gamelog_2019$`2PA`
gamelog_2019$oppBLK_perc <- gamelog_2019$BLK / gamelog_2019$`opp2PA`

gamelog_2019$FL_perc <- (gamelog_2019$FG + gamelog_2019$ORB) / (gamelog_2019$FGA + gamelog_2019$TOV)
gamelog_2019$oppFL_perc <- (gamelog_2019$oppFG + gamelog_2019$oppORB) / (gamelog_2019$oppFGA + gamelog_2019$oppTOV)

gamelog_2019$TOV_perc <- gamelog_2019$TOV / gamelog_2019$poss
gamelog_2019$oppTOV_perc <- gamelog_2019$oppTOV / gamelog_2019$oppposs

gamelog_2019$STL_perc <- gamelog_2019$oppSTL / gamelog_2019$poss
gamelog_2019$oppSTL_perc <- gamelog_2019$STL / gamelog_2019$oppposs

gamelog_2019$FTR <- gamelog_2019$FTA / gamelog_2019$FGA
gamelog_2019$oppFTR <- gamelog_2019$oppFTA / gamelog_2019$oppFGA

gamelog_2019$PPP <- gamelog_2019$Tm / gamelog_2019$poss
gamelog_2019$oppPPP <- gamelog_2019$OppScore / gamelog_2019$oppposs

gamelog_2019 <- gamelog_2019 %>% arrange(Conference, Team, G)

gamelog_2019 %>% group_by(GameType) %>%
  dplyr::summarise(minDate = min(Date)) %>% arrange(minDate)
# 2019-03-18


#####
#####
#####



gamelog_2020 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output20.xlsx")

gamelog_2020$G <- as.numeric(gamelog_2020$G)
gamelog_2020$Date <- as.Date(gamelog_2020$Date)

gamelog_2020 <- gamelog_2020 %>%
  mutate(across(8:41, as.numeric))


str(gamelog_2020)


matchup_reference_2020 <- gamelog_2020 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2020 <- gamelog_2020 %>%
  left_join(matchup_reference_2020,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2020$Location <- ifelse(is.na(gamelog_2020$Location), "H", gamelog_2020$Location)
gamelog_2020$Location <- ifelse(gamelog_2020$Location == "H", "H", "A")

gamelog_2020$OppLocation <- ifelse(is.na(gamelog_2020$OppLocation), "H", gamelog_2020$OppLocation)
gamelog_2020$OppLocation <- ifelse(gamelog_2020$OppLocation == "H", "H", "A")


gamelog_2020$DRB <- gamelog_2020$TRB - gamelog_2020$ORB
gamelog_2020$oppDRB <- gamelog_2020$oppTRB - gamelog_2020$oppORB

gamelog_2020$ORB_perc <- gamelog_2020$ORB / (gamelog_2020$ORB + gamelog_2020$oppDRB)
gamelog_2020$oppORB_perc <- gamelog_2020$oppORB / (gamelog_2020$oppORB + gamelog_2020$DRB)

gamelog_2020$poss <- gamelog_2020$FGA - gamelog_2020$ORB + gamelog_2020$TOV + (.5 * gamelog_2020$FTA)
gamelog_2020$oppposs <- gamelog_2020$oppFGA - gamelog_2020$oppORB + gamelog_2020$oppTOV + (.5 * gamelog_2020$oppFTA)

gamelog_2020$a_fgm <- gamelog_2020$AST / gamelog_2020$FG
gamelog_2020$opp_a_fgm <- gamelog_2020$oppAST / gamelog_2020$oppFG

gamelog_2020$`2P` <- gamelog_2020$FG - gamelog_2020$`3P`
gamelog_2020$`2PA` <- gamelog_2020$FGA - gamelog_2020$`3PA`
gamelog_2020$`2Pperc` <- gamelog_2020$`2P` / gamelog_2020$`2PA`

gamelog_2020$opp2P <- gamelog_2020$oppFG - gamelog_2020$opp3P
gamelog_2020$opp2PA <- gamelog_2020$oppFGA - gamelog_2020$opp3PA
gamelog_2020$opp2Pperc <- gamelog_2020$opp2P / gamelog_2020$opp2PA

gamelog_2020$`3PRate` <- gamelog_2020$`3PA` / gamelog_2020$FGA
gamelog_2020$opp3PRate <- gamelog_2020$opp3PA / gamelog_2020$oppFGA

gamelog_2020$BLK_perc <- gamelog_2020$oppBLK / gamelog_2020$`2PA`
gamelog_2020$oppBLK_perc <- gamelog_2020$BLK / gamelog_2020$`opp2PA`

gamelog_2020$FL_perc <- (gamelog_2020$FG + gamelog_2020$ORB) / (gamelog_2020$FGA + gamelog_2020$TOV)
gamelog_2020$oppFL_perc <- (gamelog_2020$oppFG + gamelog_2020$oppORB) / (gamelog_2020$oppFGA + gamelog_2020$oppTOV)

gamelog_2020$TOV_perc <- gamelog_2020$TOV / gamelog_2020$poss
gamelog_2020$oppTOV_perc <- gamelog_2020$oppTOV / gamelog_2020$oppposs

gamelog_2020$STL_perc <- gamelog_2020$oppSTL / gamelog_2020$poss
gamelog_2020$oppSTL_perc <- gamelog_2020$STL / gamelog_2020$oppposs

gamelog_2020$FTR <- gamelog_2020$FTA / gamelog_2020$FGA
gamelog_2020$oppFTR <- gamelog_2020$oppFTA / gamelog_2020$oppFGA

gamelog_2020$PPP <- gamelog_2020$Tm / gamelog_2020$poss
gamelog_2020$oppPPP <- gamelog_2020$OppScore / gamelog_2020$oppposs

gamelog_2020 <- gamelog_2020 %>% arrange(Conference, Team, G)


#####
#####
#####



gamelog_2021 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output21.xlsx")

gamelog_2021$G <- as.numeric(gamelog_2021$G)
gamelog_2021$Date <- as.Date(gamelog_2021$Date)

gamelog_2021 <- gamelog_2021 %>%
  mutate(across(8:41, as.numeric))


str(gamelog_2021)


matchup_reference_2021 <- gamelog_2021 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2021 <- gamelog_2021 %>%
  left_join(matchup_reference_2021,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2021$Location <- ifelse(is.na(gamelog_2021$Location), "H", gamelog_2021$Location)
gamelog_2021$Location <- ifelse(gamelog_2021$Location == "H", "H", "A")

gamelog_2021$OppLocation <- ifelse(is.na(gamelog_2021$OppLocation), "H", gamelog_2021$OppLocation)
gamelog_2021$OppLocation <- ifelse(gamelog_2021$OppLocation == "H", "H", "A")


gamelog_2021$DRB <- gamelog_2021$TRB - gamelog_2021$ORB
gamelog_2021$oppDRB <- gamelog_2021$oppTRB - gamelog_2021$oppORB

gamelog_2021$ORB_perc <- gamelog_2021$ORB / (gamelog_2021$ORB + gamelog_2021$oppDRB)
gamelog_2021$oppORB_perc <- gamelog_2021$oppORB / (gamelog_2021$oppORB + gamelog_2021$DRB)

gamelog_2021$poss <- gamelog_2021$FGA - gamelog_2021$ORB + gamelog_2021$TOV + (.5 * gamelog_2021$FTA)
gamelog_2021$oppposs <- gamelog_2021$oppFGA - gamelog_2021$oppORB + gamelog_2021$oppTOV + (.5 * gamelog_2021$oppFTA)

gamelog_2021$a_fgm <- gamelog_2021$AST / gamelog_2021$FG
gamelog_2021$opp_a_fgm <- gamelog_2021$oppAST / gamelog_2021$oppFG

gamelog_2021$`2P` <- gamelog_2021$FG - gamelog_2021$`3P`
gamelog_2021$`2PA` <- gamelog_2021$FGA - gamelog_2021$`3PA`
gamelog_2021$`2Pperc` <- gamelog_2021$`2P` / gamelog_2021$`2PA`

gamelog_2021$opp2P <- gamelog_2021$oppFG - gamelog_2021$opp3P
gamelog_2021$opp2PA <- gamelog_2021$oppFGA - gamelog_2021$opp3PA
gamelog_2021$opp2Pperc <- gamelog_2021$opp2P / gamelog_2021$opp2PA

gamelog_2021$`3PRate` <- gamelog_2021$`3PA` / gamelog_2021$FGA
gamelog_2021$opp3PRate <- gamelog_2021$opp3PA / gamelog_2021$oppFGA

gamelog_2021$BLK_perc <- gamelog_2021$oppBLK / gamelog_2021$`2PA`
gamelog_2021$oppBLK_perc <- gamelog_2021$BLK / gamelog_2021$`opp2PA`

gamelog_2021$FL_perc <- (gamelog_2021$FG + gamelog_2021$ORB) / (gamelog_2021$FGA + gamelog_2021$TOV)
gamelog_2021$oppFL_perc <- (gamelog_2021$oppFG + gamelog_2021$oppORB) / (gamelog_2021$oppFGA + gamelog_2021$oppTOV)

gamelog_2021$TOV_perc <- gamelog_2021$TOV / gamelog_2021$poss
gamelog_2021$oppTOV_perc <- gamelog_2021$oppTOV / gamelog_2021$oppposs

gamelog_2021$STL_perc <- gamelog_2021$oppSTL / gamelog_2021$poss
gamelog_2021$oppSTL_perc <- gamelog_2021$STL / gamelog_2021$oppposs

gamelog_2021$FTR <- gamelog_2021$FTA / gamelog_2021$FGA
gamelog_2021$oppFTR <- gamelog_2021$oppFTA / gamelog_2021$oppFGA

gamelog_2021$PPP <- gamelog_2021$Tm / gamelog_2021$poss
gamelog_2021$oppPPP <- gamelog_2021$OppScore / gamelog_2021$oppposs

gamelog_2021 <- gamelog_2021 %>% arrange(Conference, Team, G)

gamelog_2021 %>% group_by(GameType) %>%
  dplyr::summarise(minDate = min(Date)) %>% arrange(minDate)
# 2021-03-17


#####
#####
#####



gamelog_2022 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output22.xlsx")

gamelog_2022$G <- as.numeric(gamelog_2022$G)
gamelog_2022$Date <- as.Date(gamelog_2022$Date)

gamelog_2022 <- gamelog_2022 %>%
  mutate(across(8:41, as.numeric))


str(gamelog_2022)


matchup_reference_2022 <- gamelog_2022 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2022 <- gamelog_2022 %>%
  left_join(matchup_reference_2022,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2022$Location <- ifelse(is.na(gamelog_2022$Location), "H", gamelog_2022$Location)
gamelog_2022$Location <- ifelse(gamelog_2022$Location == "H", "H", "A")

gamelog_2022$OppLocation <- ifelse(is.na(gamelog_2022$OppLocation), "H", gamelog_2022$OppLocation)
gamelog_2022$OppLocation <- ifelse(gamelog_2022$OppLocation == "H", "H", "A")


gamelog_2022$DRB <- gamelog_2022$TRB - gamelog_2022$ORB
gamelog_2022$oppDRB <- gamelog_2022$oppTRB - gamelog_2022$oppORB

gamelog_2022$ORB_perc <- gamelog_2022$ORB / (gamelog_2022$ORB + gamelog_2022$oppDRB)
gamelog_2022$oppORB_perc <- gamelog_2022$oppORB / (gamelog_2022$oppORB + gamelog_2022$DRB)

gamelog_2022$poss <- gamelog_2022$FGA - gamelog_2022$ORB + gamelog_2022$TOV + (.5 * gamelog_2022$FTA)
gamelog_2022$oppposs <- gamelog_2022$oppFGA - gamelog_2022$oppORB + gamelog_2022$oppTOV + (.5 * gamelog_2022$oppFTA)

gamelog_2022$a_fgm <- gamelog_2022$AST / gamelog_2022$FG
gamelog_2022$opp_a_fgm <- gamelog_2022$oppAST / gamelog_2022$oppFG

gamelog_2022$`2P` <- gamelog_2022$FG - gamelog_2022$`3P`
gamelog_2022$`2PA` <- gamelog_2022$FGA - gamelog_2022$`3PA`
gamelog_2022$`2Pperc` <- gamelog_2022$`2P` / gamelog_2022$`2PA`

gamelog_2022$opp2P <- gamelog_2022$oppFG - gamelog_2022$opp3P
gamelog_2022$opp2PA <- gamelog_2022$oppFGA - gamelog_2022$opp3PA
gamelog_2022$opp2Pperc <- gamelog_2022$opp2P / gamelog_2022$opp2PA

gamelog_2022$`3PRate` <- gamelog_2022$`3PA` / gamelog_2022$FGA
gamelog_2022$opp3PRate <- gamelog_2022$opp3PA / gamelog_2022$oppFGA

gamelog_2022$BLK_perc <- gamelog_2022$oppBLK / gamelog_2022$`2PA`
gamelog_2022$oppBLK_perc <- gamelog_2022$BLK / gamelog_2022$`opp2PA`

gamelog_2022$FL_perc <- (gamelog_2022$FG + gamelog_2022$ORB) / (gamelog_2022$FGA + gamelog_2022$TOV)
gamelog_2022$oppFL_perc <- (gamelog_2022$oppFG + gamelog_2022$oppORB) / (gamelog_2022$oppFGA + gamelog_2022$oppTOV)

gamelog_2022$TOV_perc <- gamelog_2022$TOV / gamelog_2022$poss
gamelog_2022$oppTOV_perc <- gamelog_2022$oppTOV / gamelog_2022$oppposs

gamelog_2022$STL_perc <- gamelog_2022$oppSTL / gamelog_2022$poss
gamelog_2022$oppSTL_perc <- gamelog_2022$STL / gamelog_2022$oppposs

gamelog_2022$FTR <- gamelog_2022$FTA / gamelog_2022$FGA
gamelog_2022$oppFTR <- gamelog_2022$oppFTA / gamelog_2022$oppFGA

gamelog_2022$PPP <- gamelog_2022$Tm / gamelog_2022$poss
gamelog_2022$oppPPP <- gamelog_2022$OppScore / gamelog_2022$oppposs


gamelog_2022 <- gamelog_2022 %>% arrange(Conference, Team, G)

gamelog_2022 %>% group_by(GameType) %>%
  dplyr::summarise(minDate = min(Date)) %>% arrange(minDate)
# 2022-03-15


#####
#####
#####



gamelog_2023 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output23.xlsx")

gamelog_2023$G <- as.numeric(gamelog_2023$G)
gamelog_2023$Date <- as.Date(gamelog_2023$Date)

gamelog_2023 <- gamelog_2023 %>%
  mutate(across(8:41, as.numeric))


str(gamelog_2023)


matchup_reference_2023 <- gamelog_2023 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2023 <- gamelog_2023 %>%
  left_join(matchup_reference_2023,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2023$Location <- ifelse(is.na(gamelog_2023$Location), "H", gamelog_2023$Location)
gamelog_2023$Location <- ifelse(gamelog_2023$Location == "H", "H", "A")

gamelog_2023$OppLocation <- ifelse(is.na(gamelog_2023$OppLocation), "H", gamelog_2023$OppLocation)
gamelog_2023$OppLocation <- ifelse(gamelog_2023$OppLocation == "H", "H", "A")


gamelog_2023$DRB <- gamelog_2023$TRB - gamelog_2023$ORB
gamelog_2023$oppDRB <- gamelog_2023$oppTRB - gamelog_2023$oppORB

gamelog_2023$ORB_perc <- gamelog_2023$ORB / (gamelog_2023$ORB + gamelog_2023$oppDRB)
gamelog_2023$oppORB_perc <- gamelog_2023$oppORB / (gamelog_2023$oppORB + gamelog_2023$DRB)

gamelog_2023$poss <- gamelog_2023$FGA - gamelog_2023$ORB + gamelog_2023$TOV + (.5 * gamelog_2023$FTA)
gamelog_2023$oppposs <- gamelog_2023$oppFGA - gamelog_2023$oppORB + gamelog_2023$oppTOV + (.5 * gamelog_2023$oppFTA)

gamelog_2023$a_fgm <- gamelog_2023$AST / gamelog_2023$FG
gamelog_2023$opp_a_fgm <- gamelog_2023$oppAST / gamelog_2023$oppFG

gamelog_2023$`2P` <- gamelog_2023$FG - gamelog_2023$`3P`
gamelog_2023$`2PA` <- gamelog_2023$FGA - gamelog_2023$`3PA`
gamelog_2023$`2Pperc` <- gamelog_2023$`2P` / gamelog_2023$`2PA`

gamelog_2023$opp2P <- gamelog_2023$oppFG - gamelog_2023$opp3P
gamelog_2023$opp2PA <- gamelog_2023$oppFGA - gamelog_2023$opp3PA
gamelog_2023$opp2Pperc <- gamelog_2023$opp2P / gamelog_2023$opp2PA

gamelog_2023$`3PRate` <- gamelog_2023$`3PA` / gamelog_2023$FGA
gamelog_2023$opp3PRate <- gamelog_2023$opp3PA / gamelog_2023$oppFGA

gamelog_2023$BLK_perc <- gamelog_2023$oppBLK / gamelog_2023$`2PA`
gamelog_2023$oppBLK_perc <- gamelog_2023$BLK / gamelog_2023$`opp2PA`

gamelog_2023$FL_perc <- (gamelog_2023$FG + gamelog_2023$ORB) / (gamelog_2023$FGA + gamelog_2023$TOV)
gamelog_2023$oppFL_perc <- (gamelog_2023$oppFG + gamelog_2023$oppORB) / (gamelog_2023$oppFGA + gamelog_2023$oppTOV)

gamelog_2023$TOV_perc <- gamelog_2023$TOV / gamelog_2023$poss
gamelog_2023$oppTOV_perc <- gamelog_2023$oppTOV / gamelog_2023$oppposs

gamelog_2023$STL_perc <- gamelog_2023$oppSTL / gamelog_2023$poss
gamelog_2023$oppSTL_perc <- gamelog_2023$STL / gamelog_2023$oppposs

gamelog_2023$FTR <- gamelog_2023$FTA / gamelog_2023$FGA
gamelog_2023$oppFTR <- gamelog_2023$oppFTA / gamelog_2023$oppFGA

gamelog_2023$PPP <- gamelog_2023$Tm / gamelog_2023$poss
gamelog_2023$oppPPP <- gamelog_2023$OppScore / gamelog_2023$oppposs

gamelog_2023 <- gamelog_2023 %>% arrange(Conference, Team, G)

gamelog_2023 %>% group_by(GameType) %>%
  dplyr::summarise(minDate = min(Date)) %>% arrange(minDate)
# 2023-03-14


#####
#####
#####



gamelog_2024 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output24.xlsx")

gamelog_2024$G <- as.numeric(gamelog_2024$G)
gamelog_2024$Date <- as.Date(gamelog_2024$Date)

gamelog_2024 <- gamelog_2024 %>%
  mutate(across(8:41, as.numeric))


str(gamelog_2024)


matchup_reference_2024 <- gamelog_2024 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2024 <- gamelog_2024 %>%
  left_join(matchup_reference_2024,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp")) %>%
  rename(OppLocation = OppLocation)

gamelog_2024$Location <- ifelse(is.na(gamelog_2024$Location), "H", gamelog_2024$Location)
gamelog_2024$Location <- ifelse(gamelog_2024$Location == "H", "H", "A")

gamelog_2024$OppLocation <- ifelse(is.na(gamelog_2024$OppLocation), "H", gamelog_2024$OppLocation)
gamelog_2024$OppLocation <- ifelse(gamelog_2024$OppLocation == "H", "H", "A")


gamelog_2024$DRB <- gamelog_2024$TRB - gamelog_2024$ORB
gamelog_2024$oppDRB <- gamelog_2024$oppTRB - gamelog_2024$oppORB

gamelog_2024$ORB_perc <- gamelog_2024$ORB / (gamelog_2024$ORB + gamelog_2024$oppDRB)
gamelog_2024$oppORB_perc <- gamelog_2024$oppORB / (gamelog_2024$oppORB + gamelog_2024$DRB)

gamelog_2024$poss <- gamelog_2024$FGA - gamelog_2024$ORB + gamelog_2024$TOV + (.5 * gamelog_2024$FTA)
gamelog_2024$oppposs <- gamelog_2024$oppFGA - gamelog_2024$oppORB + gamelog_2024$oppTOV + (.5 * gamelog_2024$oppFTA)

gamelog_2024$a_fgm <- gamelog_2024$AST / gamelog_2024$FG
gamelog_2024$opp_a_fgm <- gamelog_2024$oppAST / gamelog_2024$oppFG

gamelog_2024$`2P` <- gamelog_2024$FG - gamelog_2024$`3P`
gamelog_2024$`2PA` <- gamelog_2024$FGA - gamelog_2024$`3PA`
gamelog_2024$`2Pperc` <- gamelog_2024$`2P` / gamelog_2024$`2PA`

gamelog_2024$opp2P <- gamelog_2024$oppFG - gamelog_2024$opp3P
gamelog_2024$opp2PA <- gamelog_2024$oppFGA - gamelog_2024$opp3PA
gamelog_2024$opp2Pperc <- gamelog_2024$opp2P / gamelog_2024$opp2PA

gamelog_2024$`3PRate` <- gamelog_2024$`3PA` / gamelog_2024$FGA
gamelog_2024$opp3PRate <- gamelog_2024$opp3PA / gamelog_2024$oppFGA

gamelog_2024$BLK_perc <- gamelog_2024$oppBLK / gamelog_2024$`2PA`
gamelog_2024$oppBLK_perc <- gamelog_2024$BLK / gamelog_2024$`opp2PA`

gamelog_2024$FL_perc <- (gamelog_2024$FG + gamelog_2024$ORB) / (gamelog_2024$FGA + gamelog_2024$TOV)
gamelog_2024$oppFL_perc <- (gamelog_2024$oppFG + gamelog_2024$oppORB) / (gamelog_2024$oppFGA + gamelog_2024$oppTOV)

gamelog_2024$TOV_perc <- gamelog_2024$TOV / gamelog_2024$poss
gamelog_2024$oppTOV_perc <- gamelog_2024$oppTOV / gamelog_2024$oppposs

gamelog_2024$STL_perc <- gamelog_2024$oppSTL / gamelog_2024$poss
gamelog_2024$oppSTL_perc <- gamelog_2024$STL / gamelog_2024$oppposs

gamelog_2024$FTR <- gamelog_2024$FTA / gamelog_2024$FGA
gamelog_2024$oppFTR <- gamelog_2024$oppFTA / gamelog_2024$oppFGA

gamelog_2024$PPP <- gamelog_2024$Tm / gamelog_2024$poss
gamelog_2024$oppPPP <- gamelog_2024$OppScore / gamelog_2024$oppposs

gamelog_2024 <- gamelog_2024 %>% arrange(Conference, Team, G)

gamelog_2024 %>% group_by(GameType) %>%
  dplyr::summarise(minDate = min(Date)) %>% arrange(minDate)
# 2024-03-19


#####
#####
#####



gamelog_2025 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/output25.xlsx")
# gamelog_2025 <- read.xlsx("C:/Users/AndLi/Downloads/The Big March One/output25.xlsx")

gamelog_2025$G <- as.numeric(gamelog_2025$G)
gamelog_2025$Date <- as.Date(gamelog_2025$Date)

gamelog_2025[, 8:41] <- lapply(gamelog_2025[, 8:41], as.numeric)
gamelog_2025 <- as.data.frame(gamelog_2025)

# gamelog_2025$Date[c(11445:11450)] <- c("2025-03-24")

str(gamelog_2025)


matchup_reference_2025 <- gamelog_2025 %>%
  select(Date, Team, Opp, Location) %>%
  mutate(Opponent = Opp, Team_as_Opp = Team, OppLocation = Location) %>%
  select(Date, Opponent, Team_as_Opp, OppLocation)

gamelog_2025 <- gamelog_2025 %>%
  left_join(matchup_reference_2025,
            by = c("Date" = "Date", "Team" = "Opponent", "Opp" = "Team_as_Opp"))

gamelog_2025$Location <- ifelse(is.na(gamelog_2025$Location), "H", gamelog_2025$Location)
gamelog_2025$Location <- ifelse(gamelog_2025$Location == "H", "H", "A")

gamelog_2025$OppLocation <- ifelse(is.na(gamelog_2025$OppLocation), "H", gamelog_2025$OppLocation)
gamelog_2025$OppLocation <- ifelse(gamelog_2025$OppLocation == "H", "H", "A")


gamelog_2025$DRB <- gamelog_2025$TRB - gamelog_2025$ORB
gamelog_2025$oppDRB <- gamelog_2025$oppTRB - gamelog_2025$oppORB

gamelog_2025$ORB_perc <- gamelog_2025$ORB / (gamelog_2025$ORB + gamelog_2025$oppDRB)
gamelog_2025$oppORB_perc <- gamelog_2025$oppORB / (gamelog_2025$oppORB + gamelog_2025$DRB)

gamelog_2025$poss <- gamelog_2025$FGA - gamelog_2025$ORB + gamelog_2025$TOV + (.5 * gamelog_2025$FTA)
gamelog_2025$oppposs <- gamelog_2025$oppFGA - gamelog_2025$oppORB + gamelog_2025$oppTOV + (.5 * gamelog_2025$oppFTA)

gamelog_2025$a_fgm <- gamelog_2025$AST / gamelog_2025$FG
gamelog_2025$opp_a_fgm <- gamelog_2025$oppAST / gamelog_2025$oppFG

gamelog_2025$`2P` <- gamelog_2025$FG - gamelog_2025$`3P`
gamelog_2025$`2PA` <- gamelog_2025$FGA - gamelog_2025$`3PA`
gamelog_2025$`2Pperc` <- gamelog_2025$`2P` / gamelog_2025$`2PA`

gamelog_2025$opp2P <- gamelog_2025$oppFG - gamelog_2025$opp3P
gamelog_2025$opp2PA <- gamelog_2025$oppFGA - gamelog_2025$opp3PA
gamelog_2025$opp2Pperc <- gamelog_2025$opp2P / gamelog_2025$opp2PA

gamelog_2025$`3PRate` <- gamelog_2025$`3PA` / gamelog_2025$FGA
gamelog_2025$opp3PRate <- gamelog_2025$opp3PA / gamelog_2025$oppFGA

gamelog_2025$BLK_perc <- gamelog_2025$oppBLK / gamelog_2025$`2PA`
gamelog_2025$oppBLK_perc <- gamelog_2025$BLK / gamelog_2025$`opp2PA`

gamelog_2025$FL_perc <- (gamelog_2025$FG + gamelog_2025$ORB) / (gamelog_2025$FGA + gamelog_2025$TOV)
gamelog_2025$oppFL_perc <- (gamelog_2025$oppFG + gamelog_2025$oppORB) / (gamelog_2025$oppFGA + gamelog_2025$oppTOV)

gamelog_2025$TOV_perc <- gamelog_2025$TOV / gamelog_2025$poss
gamelog_2025$oppTOV_perc <- gamelog_2025$oppTOV / gamelog_2025$oppposs

gamelog_2025$STL_perc <- gamelog_2025$oppSTL / gamelog_2025$poss
gamelog_2025$oppSTL_perc <- gamelog_2025$STL / gamelog_2025$oppposs

gamelog_2025$FTR <- gamelog_2025$FTA / gamelog_2025$FGA
gamelog_2025$oppFTR <- gamelog_2025$oppFTA / gamelog_2025$oppFGA

gamelog_2025$PPP <- gamelog_2025$Tm / gamelog_2025$poss
gamelog_2025$oppPPP <- gamelog_2025$OppScore / gamelog_2025$oppposs

gamelog_2025 <- gamelog_2025 %>% arrange(Conference, Team, G)
