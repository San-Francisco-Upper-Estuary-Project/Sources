#Key contaminanat and water quality parameter central tendency, min, max, sd
#############
# Merge precipitation, CEDEN, SURF, and DAYFLOW
#############
#Libraries
#############
library(kableExtra)
library(dplyr)
library(tidyverse)
library(data.table)
library(xts)
library(sf)
library(here)
library(readxl)
library(RColorBrewer)
library(zoo)
library(lubridate)
library(ggplot2)

#############
#DAYFLOW
#############
df2009 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2009.csv")
df2009.new <- df2009
df2009.new <- df2009.new %>%
  mutate_if(is.integer, as.numeric)
df2009.ts <- xts(df2009.new, order.by = as.Date(df2009$Date, "%m/%d/%Y"))
df2009.zoo <-as.zoo(df2009.ts)

df2010 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2010.csv")
df2010.new <- df2010
df2010.new <- df2010.new %>%
  mutate_if(is.integer, as.numeric)
df2010.ts <- xts(df2010.new, order.by = as.Date(df2010$Date, "%m/%d/%Y"))
df2010.zoo <-as.zoo(df2010.ts)

df2011 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2011.csv")
df2011.new <- df2011
df2011.new <- df2011.new %>%
  mutate_if(is.integer, as.numeric)
df2011.ts <- xts(df2011.new, order.by = as.Date(df2011$Date, "%m/%d/%Y"))
df2011.zoo <-as.zoo(df2011.ts)

df2012 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2012-05-23-2013.csv")
df2012.new <- df2012
df2012.new <- df2012.new %>%
  mutate_if(is.integer, as.numeric)
df2012.ts <- xts(df2012.new, order.by = as.Date(df2012$Date, "%m/%d/%Y"))
df2012.zoo <-as.zoo(df2012.ts)

df2013 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2013_12-13-2013.csv")
df2013.new <- df2013
df2013.new <- df2013.new %>%
  mutate_if(is.integer, as.numeric)
df2013.ts <- xts(df2013.new, order.by = as.Date(df2013$Date, "%m/%d/%Y"))
df2013.zoo <-as.zoo(df2013.ts)

df2014 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2014_11-24-2014.csv")
df2014.new <- df2014
df2014.new <- df2014.new %>%
  mutate_if(is.integer, as.numeric)
df2014.ts <- xts(df2014.new, order.by = as.Date(df2014$Date, "%m/%d/%Y"))
df2014.zoo <-as.zoo(df2014.ts)

df2015 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2015.csv")
df2015.new <- df2015
df2015.new <- df2015.new %>%
  mutate_if(is.integer, as.numeric)
df2015.ts <- xts(df2015.new, order.by = as.Date(df2015$Date, "%m/%d/%Y"))
df2015.zoo <-as.zoo(df2015.ts)

df2016 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2016.csv")
df2016.new <- df2016
df2016.new <- df2016.new %>%
  mutate_if(is.integer, as.numeric)
df2016.ts <- xts(df2016.new, order.by = as.Date(df2016$Date, "%m/%d/%Y"))
df2016.zoo <-as.zoo(df2016.ts)

df2017 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2017.csv")
df2017.new <- df2017
df2017.new <- df2017.new %>%
  mutate_if(is.integer, as.numeric)
df2017.ts <- xts(df2017.new, order.by = as.Date(df2017$Date, "%m/%d/%Y"))
df2017.zoo <-as.zoo(df2017.ts)

df2018 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2018.csv")
df2018.new <- df2018
df2018.new <- df2018.new %>%
  mutate_if(is.integer, as.numeric)
df2018.ts <- xts(df2018.new, order.by = as.Date(df2018$Date, "%m/%d/%Y"))
df2018.zoo <-as.zoo(df2018.ts)

df2019 <- read.csv("G:\\Upper San Francisco Project\\Data\\DAYFLOW\\Data\\Formatted_for_R\\dayflowcalculations2019.csv")
df2019.new <- df2019
df2019.new <- df2019.new %>%
  mutate_if(is.integer, as.numeric)
df2019.ts <- xts(df2019.new, order.by = as.Date(df2019$Date, "%m/%d/%Y"))
df2019.zoo <-as.zoo(df2019.ts)

# Each water year is now its own individual vector
# Need to combine all water years using rbind

DAYFLOW_AllWY <- rbind(df2010.new, df2011.new, df2012.new, df2013.new,
                       df2014.new, df2015.new, df2016.new, df2017.new, df2018.new, df2019.new)

DAYFLOW_AllWY$Date <- as.Date(DAYFLOW_AllWY$Date, format = "%m/%d/%Y")

DAYFLOW_AllWY_OUT <- as.data.frame(cbind(DAYFLOW_AllWY$Date, DAYFLOW_AllWY$OUT))

names(DAYFLOW_AllWY_OUT)[names(DAYFLOW_AllWY_OUT)=="V1"] <- "Date"
names(DAYFLOW_AllWY_OUT)[names(DAYFLOW_AllWY_OUT)=="V2"] <- "OUT"

DAYFLOW_AllWY_OUT$Date <- as.Date(DAYFLOW_AllWY_OUT$Date, format = "%m/%d/%Y")
DAYFLOW_AllWY_OUT$OUT <- as.numeric(DAYFLOW_AllWY_OUT$OUT)

#############
# PRISM DATA
#############
PRISM_AllWY <- read.csv("G:\\Upper San Francisco Project\\Data\\PRISM_Precipitation\\PRISM\\PRISM_Region_Join_AllWY.csv")
AllWY_Dates <- as.Date(PRISM_AllWY$Date, format = "%m/%d/%Y")
PRISM_AllWY$Date <- as.Date(PRISM_AllWY$Date, format = "%m/%d/%Y")
AllWY_max <- PRISM_AllWY %>%
  group_by(Date, Subregion, WaterYear) %>%
  summarize(max_precip = max(ppt_inches))
AllWY_max.df <- as.data.frame(AllWY_max)
names(AllWY_max.df)[names(AllWY_max.df)=="Subregion"] <- "Region"
AllWY_max.df <- AllWY_max.df %>% 
  mutate(Region = str_replace(Region, "Central Delta", "Central")) %>%
  mutate(Region = str_replace(Region, "North Delta", "North")) %>%
  mutate(Region = str_replace(Region, "Sacramento River", "Sacramento")) %>%
  mutate(Region = str_replace(Region, "South Delta", "South")) %>%
  mutate(Region = str_replace(Region, "Suisun Bay", "Suisun"))

AllWY_max.df <- as.data.frame(AllWY_max)

#############
# Fish DATA
#############
# Chinook
#############
fish <- read.csv("G:\\Upper San Francisco Project\\Data\\Fish\\Delta_Fish_Last_10_WY\\For_R\\AllFishData_NO_NAs_20200831.csv")

CHN <- subset(fish,Organism=="CHN")
CHN.all.ts <- xts(CHN$Count, as.Date(CHN$Date, "%Y-%m-%d"))

# convert daily data
CHN.all.ts_d = apply.daily(CHN.all.ts, FUN=sum)

#str(CHN.all.ts_m);head(CHN.all.ts_m)
CHN.df <- as.data.frame(CHN.all.ts_d)
CHN.df["Date"] <- NA
CHN.df["Date"] <- as.Date(index(CHN.all.ts_d), format = "%Y-%m-%d")
names(CHN.df)[names(CHN.df)=="V1"] <- "CountCHN"

#############
# Delta Smelt
#############
DSM <- subset(fish,Organism=="DSM")
DSM.all.ts <- xts(DSM$Count, as.Date(DSM$Date, "%Y-%m-%d"))
DSM.all.ts_d = apply.daily(DSM.all.ts, FUN=sum)
DSM.df <- as.data.frame(DSM.all.ts_d)
DSM.df["Date"] <- NA
DSM.df["Date"] <- as.Date(index(DSM.all.ts_d), format = "%Y-%m-%d")
names(DSM.df)[names(DSM.df)=="V1"] <- "CountDSM"





#############
# CEDEN and SURF data
#############

CEDEN <- read.csv("G:\\Upper San Francisco Project\\Data\\CEDEN_SURF_Data\\All_CEDEN_w_RegionID.csv")
SURF <- read.csv("G:\\Upper San Francisco Project\\Data\\CEDEN_SURF_Data\\All_SURF_w_RegionID.csv")
CEDEN.subset <- CEDEN[, c("Region", "Analyte", "Result", "MDL", "SampleDate", "TargetLatitude",
                          "TargetLongitude", "CollectionMethodName", "Unit")]
SURF.subset <- SURF[, c("Region", "Chemical_name", "Concentration__ppb_", "Method_detection_level__ppb_",
                        "Sample_date", "Latitude", "Longitude", "Sample_type")]
SURF.subset["Unit"] <- "ug/L"
CEDEN.subset["Source"] <- "CEDEN"
SURF.subset["Source"] <- "SURF"
CEDEN.subset$SampleDate <- as.Date(CEDEN$SampleDate, format = "%m/%d/%Y")
names(CEDEN)[names(CEDEN)=="SampleDate"] <- "Date"
SURF.subset$Sample_date <- as.Date(SURF$Sample_date, format = "%d-%b-%y")
names(SURF)[names(SURF)=="Sample_date"] <- "Date"
new.col.names <- c("Region", "Analyte", "Result", "MDL", "Date", "Latitude", "Longitude",
                   "CollectionMethod", "Unit", "Source")
CEDEN.colnames <- colnames(CEDEN.subset)
SURF.colnames <- colnames(SURF.subset)
colnames(CEDEN.subset) <- new.col.names
colnames(SURF.subset) <- new.col.names
CEDEN_SURF_comb <- rbind(CEDEN.subset, SURF.subset)

#############
#PRISM DATA
#############

PRISM_AllWY <- read.csv("G:\\Upper San Francisco Project\\Data\\PRISM_Precipitation\\PRISM\\PRISM_Region_Join_AllWY.csv")
AllWY_Dates <- as.Date(PRISM_AllWY$Date, format = "%m/%d/%Y")
PRISM_AllWY$Date <- as.Date(PRISM_AllWY$Date, format = "%m/%d/%Y")
AllWY_max <- PRISM_AllWY %>%
  group_by(Date, Subregion, WaterYear) %>%
  summarize(max_precip = max(ppt_inches))
AllWY_max.df <- as.data.frame(AllWY_max)
names(AllWY_max.df)[names(AllWY_max.df)=="Subregion"] <- "Region"
AllWY_max.df <- AllWY_max.df %>% 
  mutate(Region = str_replace(Region, "Central Delta", "Central")) %>%
  mutate(Region = str_replace(Region, "North Delta", "North")) %>%
  mutate(Region = str_replace(Region, "Sacramento River", "Sacramento")) %>%
  mutate(Region = str_replace(Region, "South Delta", "South")) %>%
  mutate(Region = str_replace(Region, "Suisun Bay", "Suisun"))
#############################
#############################
#############################
CEDEN_SURF_PRISM <- merge(CEDEN_SURF_comb, AllWY_max.df[,c("Date", "max_precip", "Region", "WaterYear")], by = c("Date", "Region"))
#############################
#############################
#############################
# 'Wet' events
wet_event.25 <- subset(AllWY_max.df, max_precip >= 0.25 & max_precip < 0.5)
oneweek.25 <- wet_event.25 %>% 
  mutate(Date = as.Date(Date)) %>% 
  group_by(Date, Region) %>% 
  complete(Date = seq.Date((Date), (Date+7), by = 'days'))
CEDEN_SURF.25 <- subset(CEDEN_SURF_PRISM, Date %in% oneweek.25$Date)
CEDEN_SURF.25["Precipitation"] <- "0.25"

wet_event.5 <- subset(AllWY_max.df, max_precip >= 0.5)
oneweek.5 <- wet_event.5 %>% 
  mutate(Date = as.Date(Date)) %>% 
  group_by(Date, Region) %>% 
  complete(Date = seq.Date((Date), (Date+7), by = 'days'))
CEDEN_SURF.5 <- subset(CEDEN_SURF_PRISM, Date %in% oneweek.5$Date)
CEDEN_SURF.5["Precipitation"] <- "0.5"
# Dry days; all other dates not included in 'wet' events +7 days
CEDEN_SURF_dry <- subset(CEDEN_SURF_PRISM, !(Date %in% oneweek.25$Date))
CEDEN_SURF_dry["Precipitation"] <- "Dry"

CEDEN_SURF <- rbind(CEDEN_SURF_dry, CEDEN_SURF.25, CEDEN_SURF.5)

#Fish_precip_Q: delta smelt, chinook, and DAYFLOW$OUT
#DAYFLOW_AllWY: all DAYFLOW
#Need to merge all DAYFLOW data with PRISM, fish counts, CEDEN and SURF

DAYFLOW_PRISM.merge <- merge(DAYFLOW_AllWY, AllWY_max.df, by = "Date", all = TRUE) #merge DAYFLOW and PRISM
Fishcounts.merge <- merge(CHN.df, DSM.df, by ="Date", all = TRUE) #merge CHN & DSM
DAYFLOW_PRISM_Fish.merge <- merge(DAYFLOW_PRISM.merge, Fishcounts.merge, by = "Date", all = TRUE) #merge DAYFLOW, PRISM, DSM & CHN

#View(Fishcounts.merge)

DAYFLOW_PRISM_Fish.subset <- DAYFLOW_PRISM_Fish.merge[,c(1,4:31,33:34)]

#CEDEN_SURF_DAYFLOW_PRISM_Fish abbreviated as: C_S_D_P_F
C_S_D_P_F.merge <- merge(CEDEN_SURF, DAYFLOW_PRISM_Fish.subset,
                         by = c("Date", "Region", "WaterYear"))

df.test <-  C_S_D_P_F.merge %>%
  mutate(month = month(Date),
         year = year(Date)) %>%
  group_by(year, month, Region) %>%
  summarise(n = n(),
            max_Malathion = max(Result[Analyte == "Malathion, Total" & Unit == "ug/L"| Analyte == "malathion"], na.rm = TRUE),
            max_Diazinon = max(Result[Analyte == "Diazinon, Total" & Unit == "ug/L"| Analyte == "diazinon"], na.rm = TRUE),
            max_Chlorpyrifos = max(Result[Analyte == "Chlorpyrifos, Total" & Unit == "ug/L"| Analyte == "chlorpyrifos"], na.rm = TRUE),
            max_Mercury_Total = max(Result[Analyte == "Mercury, Total" & Unit == "ug/L"|Analyte == "Mercury, Total" & Unit == "ng/L"], na.rm = TRUE),
            max_Mercury_Methyl_Total = max(Result[Analyte == "Mercury, Methyl, Total" & Unit == "ng/L"], na.rm = TRUE),
            max_Mercury_Dissolved = max(Result[Analyte == "Mercury, Methyl, Dissolved" & Unit == "ng/L"], na.rm = TRUE),
            max_Selenium_Total = max(Result[Analyte == "Selenium, Total" & Unit == "ug/L"], na.rm = TRUE),
            max_Selenium_Dissolved = max(Result[Analyte == "Selenium, Dissolved" & Unit == "ug/L"], na.rm = TRUE),
            max_Fipronil = max(Result[Analyte == "Fipronil, Dissolved" & Unit == "ng/L"| Analyte == "fipronil" & Unit == "ug/L"], na.rm = TRUE),
            max_Imidacloprid_Total = max(Result[Analyte == "Imidacloprid, Total" & Unit == "ug/L"| Analyte == "imidacloprid"], na.rm = TRUE),
            max_Imidacloprid_Dissolved = max(Result[Analyte == "Imidacloprid, Dissolved" & Unit == "ng/L"], na.rm = TRUE),
            max_Atrazine = max(Result[Analyte == "Atrazine, Total" & Unit == "ug/L"| Analyte == "atrazine"], na.rm = TRUE),
            max_N_Total = max(Result[Analyte == "Nitrate as N, Total"], na.rm = TRUE),
            max_N_Dissolved = max(Result[Analyte == "Nitrate as N, Dissolved"], na.rm = TRUE),
            max_NO2_NO3_Total = max(Result[Analyte == "Nitrate + Nitrite as N, Total"], na.rm = TRUE),
            max_Phosphorus_Total = max(Result[Analyte == "Phosphorus as P, Total" & Unit == "mg/L"], na.rm = TRUE),
            max_Phosphorus_Total = max(Result[Analyte == "Phosphorus as P, Dissolved" & Unit == "mg/L"], na.rm = TRUE),
            max_DO = max(Result[Analyte == "Oxygen, Dissolved" & Unit == "mg/L"], na.rm = TRUE),
            max_Temperature = max(Result[Analyte == "Temperature"], na.rm = TRUE),
            max_Turbidity = max(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            max_Salinity = max(Result[Analyte == "Salinity, Total" & Unit == "mg/L"], na.rm = TRUE),
            TOT = sum(TOT, na.rm = TRUE)
  )
df.test <- df.test %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
  mutate_if(is.numeric, list(~na_if(., "NaN"))) %>%
  mutate_if(is.numeric, list(~na_if(., NaN)))

df.test[is.na(df.test)] <- 0
View(df.test)
# Possibly another way to do this using lapply?
#e.g
#lapply(dat, FUN=apply.monthly, sum)
#(x1[,4:24]) <- lapply(as.data.frame(x1), FUN=apply.monthly, sum)
#na.rm(df.test)
