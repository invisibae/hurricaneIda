# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(dplyr)
library(tidycensus)
library(tidygeocoder)
library(zoo)

# Read in reports  --------------------------------------------------------
# Police Reports 
pr_2011 <- read_csv("data/raw/Electronic_Police_Report_2011.csv")
pr_2012 <- read_csv("data/raw/Electronic_Police_Report_2012.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2013 <- read_csv("data/raw/Electronic_Police_Report_2013.csv")%>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2014 <- read_csv("data/raw/Electronic_Police_Report_2014.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2015 <- read_csv("data/raw/Electronic_Police_Report_2015.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2016 <- read_csv("data/raw/Electronic_Police_Report_2016.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2017 <- read_csv("data/raw/Electronic_Police_Report_2017.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2018 <- read_csv("data/raw/Electronic_Police_Report_2018.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2019 <- read_csv("data/raw/Electronic_Police_Report_2019.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2020 <- read_csv("data/raw/Electronic_Police_Report_2020.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2021 <-read_csv("data/raw/Electronic_Police_Report_2021.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

pr_2022 <- read_csv("data/raw/Electronic_Police_Report_2022.csv") %>% 
  select(Item_Number,  
         Occurred_Date_Time, 
         Signal_Type, 
         Signal_Description, 
         Charge_Code, 
         Charge_Description, 
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location)

# Calls for service
cfs_2011 <- read_csv("data/raw/Calls_for_Service_2011.csv") %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2012 <- read_csv("data/raw/Calls_for_Service_2012.csv") %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2013 <- read_csv("data/raw/Calls_for_Service_2013.csv") %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2014 <- read_csv("data/raw/Calls_for_Service_2014.csv") %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2015 <- read_csv("data/raw/Calls_for_Service_2015.csv") %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2016 <- read_csv("data/raw/Calls_for_Service_2016.csv") %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2017 <- read_csv("data/raw/Calls_for_Service_2017.csv") %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2018 <- read_csv("data/raw/Calls_for_Service_2018.csv") %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2019 <- read_csv("data/raw/Calls_for_Service_2019.csv") %>% 
  rename(Type_ = Type) %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2020 <- read_csv("data/raw/Calls_for_Service_2020.csv") %>% 
  rename(Type_ = Type) %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)
cfs_2021 <- read_csv("data/raw/Calls_for_Service_2021.csv")%>% 
  rename(Type_ = Type) %>% 
  select(Type_, TypeText, TimeCreate, TimeDispatch, TimeClosed, DispositionText, BLOCK_ADDRESS, Location)


# Clean -------------------------------------------------------------------
# 2011 -- weird data strcture
pr_2011 <- pr_2011 %>% 
  select(-Location) %>% 
  rename(Item_Number = NOPD_Item,
         Occurred_Date_Time = TimeCreate,
         Signal_Type = Type_,
         Signal_Description = TypeText,
         Location = BLOCK_ADDRESS) %>% 
  mutate(Charge_Code = NA,
         Charge_Description = NA,
         Victim_Fatal_Status = NA,
         Victim_Gender = NA,
         Victim_Race = NA) %>% 
  select(Item_Number,
         Occurred_Date_Time,
         Signal_Type,
         Signal_Description,
         Charge_Code,
         Charge_Description,
         Victim_Fatal_Status,
         Victim_Gender,
         Victim_Race,
         Location) 


# 2011-2022 police reports 

df_list <- list(
          pr_2012,
          pr_2013,
          pr_2014,
          pr_2015,
          pr_2016,
          pr_2017,
          pr_2018,
          pr_2019,
          pr_2020,
          pr_2021,
          pr_2022
  )

pr_12_to_22 <-
  df_list %>% reduce(full_join)

# 2011-2022 police reports 

df_list <- list(
  cfs_2011,
  cfs_2012,
  cfs_2013,
  cfs_2014,
  cfs_2015,
  cfs_2016,
  cfs_2017,
  cfs_2018,
  cfs_2019,
  cfs_2020,
  cfs_2021
)

cfs_11_to_21 <- df_list %>% 
  reduce(full_join)




# Analysis  ---------------------------------------------------------------

# DV Calls by Year 
# Note: Below methodology excludes "Domestic Distubrance" Calls -- Charge Description tends to be NA 

# 2011
pr_2011 %>% 
  filter(str_detect(Signal_Type, "34D|35D|56D|38D|37D")) %>% 
  group_by(Signal_Description) %>% 
  count(sort = T)

pr_2011 %>% 
  filter(str_detect(Signal_Type, "34D|35D|56D|38D|37D")) %>% 
  group_by(Signal_Type, Signal_Description) %>% 
  count(sort =T )

pr_2011 %>% 
  glimpse()

# all other years 
# police reports 
pr_12_to_22 %>% 
  mutate(year = floor_date(Occurred_Date_Time, unit = "year")) %>%
  filter(str_detect(Signal_Type, "34D|35D|56D|38D|37D")) %>% 
  group_by(Charge_Code, Charge_Description) %>% 
  count(sort = T) 


# calls for service 
cfs_11_to_21 %>% 
  mutate(year = mdy_hms(TimeCreate)) %>% 
  mutate(year = floor_date(year, unit = "year")) %>%
  filter(str_detect(Type_, "34D|35D|56D|38D|37D")) %>% 
  group_by(TypeText) %>% 
  count(sort = T) 



# Strangulations by Year 

# 2011 

# All other years 

pr_12_to_22 %>% 
  mutate(year = floor_date(Occurred_Date_Time, unit = "year")) %>%
  filter(str_detect(Charge_Description, "STRANGULATION") | str_detect(Charge_Code, "14 35.3(B)7|14 35.3(b)6|14 35.3(L)|14 34.9(B)5|14 35.3(L)")) %>% 
  group_by(year) %>% 
  count(sort = T) 

# DV and Strangulations around disasters 
# LA natural disasters 
disaster_list_LA <- read_csv("data/disasters_list.csv") %>% 
  filter(state == "LA")

disaster_list_LA 



# Let's break it down
pr_12_to_22_dv_nd <-
pr_12_to_22 %>% 
  mutate(disaster_which = case_when(
                              Occurred_Date_Time >= "2012-08-24" & Occurred_Date_Time < "2012-09-02" ~ "HURRICANE ISAAC",
                              Occurred_Date_Time >= "2016-03-06" & Occurred_Date_Time < "2016-03-14" ~ "SEVERE STORMS AND FLOODING MAR 2016",
                              Occurred_Date_Time >= "2016-08-09" & Occurred_Date_Time < "2016-08-18" ~ "SEVERE STORMS AND FLOODING AUG 2016",
                              Occurred_Date_Time >= "2020-08-20" & Occurred_Date_Time < "2020-08-29" ~ "HURRICANE LAURA",
                              Occurred_Date_Time >= "2021-08-24" & Occurred_Date_Time < "2021-09-02" ~ "HURRICANE IDA",
                              #
                              TRUE ~ "NO NATURAL DISASTER"
                              ),
         disaster_y_n = case_when(
                              Occurred_Date_Time >= "2012-02-12" & Occurred_Date_Time < "2012-08-24" ~ "SIX MONTHS BEFORE",
                              Occurred_Date_Time >= "2015-10-06" & Occurred_Date_Time < "2016-03-06" ~ "SIX MONTHS BEFORE",
                              Occurred_Date_Time >= "2016-02-28" & Occurred_Date_Time < "2016-08-09" ~ "SIX MONTHS BEFORE",
                              Occurred_Date_Time >= "2020-02-08" & Occurred_Date_Time < "2020-08-29" ~ "SIX MONTHS BEFORE",
                              Occurred_Date_Time >= "2021-02-12" & Occurred_Date_Time < "2021-08-24" ~ "SIX MONTHS BEFORE",
                              #
                              Occurred_Date_Time >= "2012-09-02" & Occurred_Date_Time < "2013-02-12" ~ "SIX MONTHS AFTER",
                              Occurred_Date_Time >= "2016-03-14" & Occurred_Date_Time < "2016-09-14" ~ "SIX MONTHS AFTER",
                              Occurred_Date_Time >= "2016-08-18" & Occurred_Date_Time < "2016-02-18" ~ "SIX MONTHS AFTER",
                              Occurred_Date_Time >= "2020-08-29" & Occurred_Date_Time < "2020-02-29" ~ "SIX MONTHS AFTER",
                              Occurred_Date_Time >= "2021-09-02" & Occurred_Date_Time < "2021-03-02" ~ "SIX MONTHS AFTER",
                              #
                              disaster_which == "NO NATURAL DISASTER" ~ "n",
                              #
                              TRUE ~ "y")) 

pr_12_to_22_dv_nd %>% 
  filter(Charge_Description %in% dv_charges$Charge_Description) %>% 
  group_by(disaster_y_n) %>% 
  summarize(n = n()) 

pr_12_to_22_dv_nd %>% 
  filter(Occurred_Date_Time >= "2020-01-01" & Occurred_Date_Time < "2021-01-01")


# for service calls 
cfs_11_to_21_dv_nd <-
  cfs_11_to_21 %>% 
  mutate(TimeDispatch = mdy_hms(TimeDispatch)) %>% 
  mutate(disaster_which = case_when(
    TimeDispatch >= "2012-08-24" & TimeDispatch < "2012-09-02" ~ "HURRICANE ISAAC",
    TimeDispatch >= "2016-03-06" & TimeDispatch < "2016-03-14" ~ "SEVERE STORMS AND FLOODING MAR 2016",
    TimeDispatch >= "2016-08-09" & TimeDispatch < "2016-08-18" ~ "SEVERE STORMS AND FLOODING AUG 2016",
    TimeDispatch >= "2020-08-20" & TimeDispatch < "2020-08-29" ~ "HURRICANE LAURA",
    TimeDispatch >= "2021-08-24" & TimeDispatch < "2021-09-02" ~ "HURRICANE IDA",
    #
    TRUE ~ "NO NATURAL DISASTER"
  ),
  disaster_y_n = case_when(
    TimeDispatch >= "2012-08-12" & TimeDispatch < "2012-08-22" ~ "TEN DAYS BEFORE",
    TimeDispatch >= "2016-02-22" & TimeDispatch < "2016-03-04" ~ "TEN DAYS BEFORE",
    TimeDispatch >= "2016-07-28" & TimeDispatch < "2016-08-07" ~ "TEN DAYS BEFORE",
    TimeDispatch >= "2020-08-08" & TimeDispatch < "2020-08-18" ~ "TEN DAYS BEFORE",
    TimeDispatch >= "2021-08-12" & TimeDispatch < "2021-08-22" ~ "TEN DAYS BEFORE",
    #
    TimeDispatch >= "2012-09-02" & TimeDispatch < "2012-09-12" ~ "TEN DAYS AFTER",
    TimeDispatch >= "2016-03-14" & TimeDispatch < "2016-03-24" ~ "TEN DAYS AFTER",
    TimeDispatch >= "2016-08-18" & TimeDispatch < "2016-08-28" ~ "TEN DAYS AFTER",
    TimeDispatch >= "2020-08-29" & TimeDispatch < "2020-09-08" ~ "TEN DAYS AFTER",
    TimeDispatch >= "2021-09-02" & TimeDispatch < "2021-09-12" ~ "TEN DAYS AFTER",
    #
    disaster_which == "NO NATURAL DISASTER" ~ "n",
    #
    TRUE ~ "y")) %>% 
  mutate(disaster_1_0 = 
           case_when(
             disaster_y_n == "y" ~ 1,
             disaster_y_n != "y" ~ 0
           ))


# How many DV Calls during natural disasters

# police filings 
# "During" is defined as 2 days before + day of disaster + 7 days after
pr_12_to_22_dv_nd %>% 
  filter(str_detect(Signal_Type, "34D|35D|56D|38D|37D|103D")) %>% 
  group_by(disaster_which) %>% 
  count(sort = T)

# calls for service 
cfs_11_to_21_dv_nd %>% 
  filter(str_detect(Type_, "34D|35D|56D|38D|37D|103D")) %>% 
  group_by(disaster_which) %>% 
  count(sort = T)

# Aggregating: Comparing # of DV calls durin natural disasters to 10 day period before/after
# police filings 
pr_12_to_22_dv_nd %>% 
  filter(Charge_Description %in% dv_charges$Charge_Description) %>% 
  group_by(disaster_y_n) %>% 
  count(sort = T) 

# calls for service 
cfs_11_to_21_dv_nd %>% 
  filter(str_detect(Type_, "34D|35D|56D|38D|37D|103D")) %>% 
  group_by(disaster_y_n) %>% 
  count(sort = T) 

# What do noatural disasters do to the rolling average DV cases?
# calls for service:

cfs_11_to_21 %>% 
  filter(str_detect(Type_, "34D|35D|56D|38D|37D|103D")) %>% 
  mutate(TimeCreate = mdy_hms(TimeCreate)) %>% 
  filter(year(TimeCreate) >= 2016) %>% 
  filter(month(TimeCreate) == 8 & day(TimeCreate) >=24 |
           month(TimeCreate) == 9 & day(TimeCreate) < 2) %>% 
  group_by(year = floor_date(TimeCreate, "year")) %>% 
  # group_by(month(TimeCreate), day(TimeCreate)) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(ida = ifelse(year >= ymd("2021-01-01"), "ida", "other")) %>% 
  group_by(ida) %>% 
  summarise(mean = mean(n, na.rm = T))


cfs_by_week <- cfs_11_to_21_dv_nd %>% 
  filter(str_detect(Type_, "34D|35D|56D|38D|37D|103D")) %>% 
  mutate(TimeCreate = mdy_hms(TimeCreate)) %>% 
  group_by(week = floor_date(TimeCreate, unit = "week")) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(ma2=rollapply(n,2,mean,align='right',fill=NA)) %>% 
  mutate(disaster_which = case_when(
    week >= "2012-08-24" & week < "2012-09-02" ~ "HURRICANE ISAAC",
    week >= "2016-03-06" & week < "2016-03-14" ~ "SEVERE STORMS AND FLOODING MAR 2016",
    week >= "2016-08-09" & week < "2016-08-18" ~ "SEVERE STORMS AND FLOODING AUG 2016",
    week >= "2020-08-20" & week < "2020-08-29" ~ "HURRICANE LAURA",
    week >= "2021-08-24" & week < "2021-09-02" ~ "HURRICANE IDA",
    #
    TRUE ~ "NO NATURAL DISASTER"
  ),
  disaster_y_n = case_when(
    week >= "2012-08-12" & week < "2012-08-22" ~ "TEN DAYS BEFORE",
    week >= "2016-02-22" & week < "2016-03-04" ~ "TEN DAYS BEFORE",
    week >= "2016-07-28" & week < "2016-08-07" ~ "TEN DAYS BEFORE",
    week >= "2020-08-08" & week < "2020-08-18" ~ "TEN DAYS BEFORE",
    week >= "2021-08-12" & week < "2021-08-22" ~ "TEN DAYS BEFORE",
    #
    week >= "2012-09-02" & week < "2012-12-12" ~ "TEN DAYS AFTER",
    week >= "2016-03-14" & week < "2016-06-24" ~ "TEN DAYS AFTER",
    week >= "2016-08-18" & week < "2016-11-28" ~ "TEN DAYS AFTER",
    week >= "2020-08-29" & week < "2020-12-08" ~ "TEN DAYS AFTER",
    week >= "2021-09-02" & week < "2021-12-12" ~ "TEN DAYS AFTER",
    #
    disaster_which == "NO NATURAL DISASTER" ~ "n",
    #
    TRUE ~ "y")) %>% 
  mutate(disaster_1_0 = 
           case_when(
             disaster_y_n == "y" ~ 1,
             disaster_y_n != "y" ~ 0
           ))

test <- 
  cfs_by_week %>% 
  group_by(disaster_y_n) %>% 
  summarise(n = n(),
            mean = mean(ma2, na.rm = T)) %>% 
  as.data.frame() 

chisq.test(matrix(c(test$disaster_y_n, test$mean), nrow = 4))


lm(n ~ disaster_y_n, cfs_by_week)


# police filings 

# domestic abuse charges 
dv_charges <- pr_12_to_22 %>% 
  filter(str_detect(Charge_Description, "DOMESTIC")) %>% 
  group_by(Charge_Description) %>% 
  count(sort = T)

# rolling average dv charges 
pr_12_to_22_dv_nd %>% 
  filter(Charge_Description %in% dv_charges$Charge_Description) %>% 
  group_by(week = floor_date(Occurred_Date_Time, unit = "day")) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(ma2=rollapply(n,7,mean,align='right',fill=NA)) %>% 
  mutate(disaster_which = case_when(
    week >= "2012-08-24" & week < "2012-09-02" ~ "HURRICANE ISAAC",
    week >= "2016-03-06" & week < "2016-03-14" ~ "SEVERE STORMS AND FLOODING MAR 2016",
    week >= "2016-08-09" & week < "2016-08-18" ~ "SEVERE STORMS AND FLOODING AUG 2016",
    week >= "2020-08-20" & week < "2020-08-29" ~ "HURRICANE LAURA",
    week >= "2021-08-24" & week < "2021-09-02" ~ "HURRICANE IDA",
    TRUE ~ "NO NATURAL DISASTER"
  )) %>% 
  filter(week >= "2021-01-01" & week < "2022-01-01") %>% 
  write_csv("data/ida_daily_rolling_average_2021.csv")


# now lets isolate a natural disaster 
cfs_11_to_21_dv_nd %>% 
  filter(str_detect(Type_, "34D|35D|56D|38D|37D|103D")) %>% 
  mutate(TimeCreate = mdy_hms(TimeCreate)) %>% 
  group_by(week = floor_date(TimeCreate, unit = "day")) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(ma2=rollapply(n,7,mean,align='right',fill=NA)) %>% 
  mutate(disaster_which = case_when(
    week >= "2012-08-24" & week < "2012-09-02" ~ "HURRICANE ISAAC",
    week >= "2016-03-06" & week < "2016-03-14" ~ "SEVERE STORMS AND FLOODING MAR 2016",
    week >= "2016-08-09" & week < "2016-08-18" ~ "SEVERE STORMS AND FLOODING AUG 2016",
    week >= "2020-08-20" & week < "2020-08-29" ~ "HURRICANE LAURA",
    week >= "2021-08-24" & week < "2021-09-02" ~ "HURRICANE IDA",
    TRUE ~ "NO NATURAL DISASTER"
  )) %>% 
  # filter(week >= "2021-01-01" & week < "2022-01-01") %>% 
  filter(week >= "2021-07-01" & week < "2021-09-30") %>% 
  write_csv("data/ida_daily_rolling_average_cfs_during_ida.csv")

  filter(week >= "2021-07-01" & week < "2021-09-30") 


# How many strangulation charges during natural disasters?
pr_12_to_22_dv_nd %>% 
  filter(str_detect(Charge_Description, "STRANGULATION") | str_detect(Charge_Code, "14 35.3(B)7|14 35.3(b)6|14 35.3(L)|14 34.9(B)5|14 35.3(L)")) %>% 
  group_by(disaster_which) %>% 
  count(sort = T)

# Aggregating: Comparing # of strangulation charges durin natural disasters to 10 day period before/after
pr_12_to_22_dv_nd %>% 
  filter(str_detect(Charge_Description, "STRANGULATION") | str_detect(Charge_Code, "14 35.3(B)7|14 35.3(b)6|14 35.3(L)|14 34.9(B)5|14 35.3(L)")) %>% 
  group_by(disaster_y_n) %>% 
  count(sort = T) 


## 
pr_12_to_22_dv_nd %>% 
  filter(str_detect(Signal_Type, "34D|35D|56D|38D|37D|103D")) %>% 
  group_by(day = floor_date(Occurred_Date_Time, unit = "day")) %>% 
  count(sort = T) %>% 
  arrange(day) 

