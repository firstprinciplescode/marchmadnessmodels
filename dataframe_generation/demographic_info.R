library(readxl)
library(openxlsx)
library(tidyverse)
library(scales)
library(sqldf)


all_minutes_2016 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2016.xlsx") %>% filter(!is.na(MP))

all_rosters_2016 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2016.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2016$Weight <- as.numeric(all_rosters_2016$Weight)

all_minutes_2016$MP <- as.numeric(all_minutes_2016$MP)

all_rosters_2016 <- all_rosters_2016 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2016 <- left_join(all_minutes_2016, all_rosters_2016 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2016 <- demographics_2016 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )


###
### 2017
###



all_minutes_2017 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2017.xlsx") %>% filter(!is.na(MP))

all_rosters_2017 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2017.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2017$Weight <- as.numeric(all_rosters_2017$Weight)

all_minutes_2017$MP <- as.numeric(all_minutes_2017$MP)

all_rosters_2017 <- all_rosters_2017 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2017 <- left_join(all_minutes_2017, all_rosters_2017 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2017 <- demographics_2017 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )



###
### 2018
###


all_minutes_2018 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2018.xlsx") %>% filter(!is.na(MP))

all_rosters_2018 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2018.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2018$Weight <- as.numeric(all_rosters_2018$Weight)

all_minutes_2018$MP <- as.numeric(all_minutes_2018$MP)

all_rosters_2018 <- all_rosters_2018 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2018 <- left_join(all_minutes_2018, all_rosters_2018 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2018 <- demographics_2018 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )


###
### 2019
###


all_minutes_2019 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2019.xlsx") %>% filter(!is.na(MP))

all_rosters_2019 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2019.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2019$Weight <- as.numeric(all_rosters_2019$Weight)

all_minutes_2019$MP <- as.numeric(all_minutes_2019$MP)

all_rosters_2019 <- all_rosters_2019 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2019 <- left_join(all_minutes_2019, all_rosters_2019 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2019 <- demographics_2019 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )


###
### 2020
###


all_minutes_2020 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2020.xlsx") %>% filter(!is.na(MP))

all_rosters_2020 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2020.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2020$Weight <- as.numeric(all_rosters_2020$Weight)

all_minutes_2020$MP <- as.numeric(all_minutes_2020$MP)

all_rosters_2020 <- all_rosters_2020 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2020 <- left_join(all_minutes_2020, all_rosters_2020 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2020 <- demographics_2020 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )


###
### 2021
###


all_minutes_2021 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2021.xlsx") %>% filter(!is.na(MP))

all_rosters_2021 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2021.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2021$Weight <- as.numeric(all_rosters_2021$Weight)

all_minutes_2021$MP <- as.numeric(all_minutes_2021$MP)

all_rosters_2021 <- all_rosters_2021 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2021 <- left_join(all_minutes_2021, all_rosters_2021 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2021 <- demographics_2021 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )


###
### 2022
###


all_minutes_2022 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2022.xlsx") %>% filter(!is.na(MP))

all_rosters_2022 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2022.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2022$Weight <- as.numeric(all_rosters_2022$Weight)

all_minutes_2022$MP <- as.numeric(all_minutes_2022$MP)

all_rosters_2022 <- all_rosters_2022 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2022 <- left_join(all_minutes_2022, all_rosters_2022 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2022 <- demographics_2022 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )


###
### 2023
###


all_minutes_2023 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2023.xlsx") %>% filter(!is.na(MP))

all_rosters_2023 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2023.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2023$Weight <- as.numeric(all_rosters_2023$Weight)

all_minutes_2023$MP <- as.numeric(all_minutes_2023$MP)

all_rosters_2023 <- all_rosters_2023 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2023 <- left_join(all_minutes_2023, all_rosters_2023 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2023 <- demographics_2023 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )


###
### 2024
###


all_minutes_2024 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2024.xlsx") %>% filter(!is.na(MP))

all_rosters_2024 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2024.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2024$Weight <- as.numeric(all_rosters_2024$Weight)

all_minutes_2024$MP <- as.numeric(all_minutes_2024$MP)

all_rosters_2024 <- all_rosters_2024 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2024 <- left_join(all_minutes_2024, all_rosters_2024 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2024 <- demographics_2024 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )


###
### 2025
###


all_minutes_2025 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_minutes_2025.xlsx") %>% filter(!is.na(MP))

all_rosters_2025 <- read.xlsx("C:/Users/vflre/Downloads/The Big March One/all_team_rosters_2025.xlsx") %>% filter(!is.na(Height) & !is.na(Weight))

all_rosters_2025$Weight <- as.numeric(all_rosters_2025$Weight)

all_minutes_2025$MP <- as.numeric(all_minutes_2025$MP)

all_rosters_2025 <- all_rosters_2025 %>%
  # Convert Class to class_ind
  mutate(class_ind = case_when(
    Class == "FR" ~ 1,
    Class == "SO" ~ 2,
    Class == "JR" ~ 3,
    Class == "SR" ~ 4,
    TRUE ~ NA_real_  # Keeps NA values unchanged
  )) %>%
  # Convert height format (6-3) into inches
  mutate(Height_Trans = str_extract(Height, "^[0-9]+") %>% as.numeric() * 12 + 
           str_extract(Height, "(?<=-)[0-9]+") %>% as.numeric())

demographics_2025 <- left_join(all_minutes_2025, all_rosters_2025 %>% select(Player, Team, Pos, Height_Trans, Weight, class_ind), by = c("Player", "Team")) %>% arrange(Team, MP)

demographics_grouped_2025 <- demographics_2025 %>%
  mutate(PositionGroup = case_when(
    Pos == "G" ~ "G",  # Guards
    Pos != "G" ~ "FC",  # Forwards & Centers
    TRUE ~ NA_character_  # Handles unexpected values
  )) %>%
  filter(!is.na(PositionGroup)) %>%  # Remove any unknown positions
  group_by(Team, Conference, PositionGroup) %>%
  summarise(
    Weighted_Height = sum(MP * Height_Trans, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Weight = sum(MP * Weight, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    Weighted_Class = sum(MP * class_ind, na.rm = TRUE) / sum(MP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = PositionGroup, values_from = c(Weighted_Height, Weighted_Weight, Weighted_Class), 
              names_glue = "{PositionGroup}_{.value}") %>%
  rename(
    G_height = G_Weighted_Height, G_weight = G_Weighted_Weight, G_class = G_Weighted_Class,
    FC_height = FC_Weighted_Height, FC_weight = FC_Weighted_Weight, FC_class = FC_Weighted_Class
  )

