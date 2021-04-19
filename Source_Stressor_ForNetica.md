---
title: "Combining Source and Stressor"
author: "Erika W"
date: "4/19/2021"
output:
  html_document:
    code_download: true
    keep_md: true
    toc: true
    toc_float:
      toc_collapsed: true
    toc_depth: 3
    theme: lumen
---




```r
rm(list = ls())
library(tidyverse)
library(data.table)
library(sf)
library(here)
library(ggplot2)
```

## Load Data

#### USFE Risk Regions

```r
# Risk Regions from GitHub CEDEN repository (change if moved)

USFE.RiskRegions.z <- "https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/USFE_RiskRegions_9292020.zip"

unzip_shape <- function(InputShapeZip){
  dl.temp <- tempfile() # Create local temp file for zipped shapefile
  dl.temp2 <- tempfile() # Create a second local temp file to store unzipped shapefile
  download.file(InputShapeZip, dl.temp, quiet=T) # Downloads zip file from InputShape
  unzip(zip = dl.temp, exdir = dl.temp2) # Unzips zip file
  shapefile.out <-list.files(dl.temp2, pattern = ".shp$",full.names=TRUE) # stores file path of files with .shp ext in dl.temp2
  sf::st_read(shapefile.out) # Reads shapefile as sf object
}

USFE.RiskRegions <- unzip_shape(USFE.RiskRegions.z) # CRS is WGS 84
```

```
## Reading layer `RiskRegions_DWSC_Update_9292020' from data source `C:\Users\Erika\AppData\Local\Temp\Rtmp2nLPcD\file24a8305a57d5\RiskRegions_DWSC_Update_9292020.shp' using driver `ESRI Shapefile'
## Simple feature collection with 6 features and 6 fields
## geometry type:  POLYGON
## dimension:      XYZ
## bbox:           xmin: -122.1431 ymin: 37.62499 xmax: -121.1967 ymax: 38.58916
## z_range:        zmin: 0 zmax: 0
## geographic CRS: WGS 84
```

#### Source Data

```r
# Precipitation Data from PRISM from Seasons repo

AllWY_max <-  fread("https://github.com/WWU-IETC-R-Collab/Seasonal/raw/master/Data/Output/USFE_Precip.csv")
  
# Load Land Use Data from GitHub LULC repo

LULC <- fread("https://github.com/WWU-IETC-R-Collab/LULC/raw/EW_Learning/Output/NLCD_LULC.csv") %>%
  select(Subregion, PercHighIntens, PercAgri)
```

#### Stressor Data


```r
# CEDENSURF Sediment Data (wide format)
Sed_Wide <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/Allsed.Wide.csv") 

# CEDENSURF Water Data (wide format)
Water_Wide <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/Allwater.Wide.csv")
```

## Combine Source tables

Adds column with % of subregion with land use "Developed - high intensity", and a column for % of subregion having land use "cultivated crops" 


```r
# Join LULC to AllWY_max.
Source <- merge(AllWY_max, LULC, by = "Subregion")
```

## Combine Source-Stressor tables, & prepare for NETICA {.tabset}

Tabs below have the following process repeated for three datasets: two files prepared for the final NETICA - one df for water and one df for sediment, and a small subset used to test the format compatibility with Netica.

The process used to prepare the data is:

1. Join Seasons data with the wide format CEDENSURF (created via steps documented in the data-splitting rmd). 

2. Reformat to meet requirements for Netica

    Values should match how they will appear in NETICA
    
    * Region - change to names w/o spaces
    * NA --> *

3. Save as csv, then save-as tab delimited txt file (*.txt)

(Writing directly to txt file does not play nice with Netica... Save as CSV then "save-as" txt)

### Sediment


```r
# Join CEDENSURF to Source
CS_PRISM<- merge(Sed_Wide, Source, by = c("Date", "Subregion"))
```


```r
# Merge with season data
CS_PRISM<- merge(Sed_Wide, Source, by = c("Date", "Subregion"))

# Rename regions

CS_PRISM <- CS_PRISM %>% 
  mutate(Region = Subregion) %>%
  mutate(Region = str_replace(Region, "Central Delta", "Central")) %>%
  mutate(Region = str_replace(Region, "North Delta", "North")) %>%
  mutate(Region = str_replace(Region, "Sacramento River", "Sacramento")) %>%
  mutate(Region = str_replace(Region, "South Delta", "South")) %>%
  mutate(Region = str_replace(Region, "Suisun Bay", "Suisun"))

# Remove unnecessary columns

ForNetica <- CS_PRISM %>% select(!c(Subregion, Latitude, Longitude, Date, WaterYear, max_precip, d14_precipavg))

# Replace NA with *, which is how Netica deals with NA

ForNetica <- mutate_all(ForNetica, ~replace(., is.na(.), "*"))

# Save

write.csv(x = ForNetica, file = "Data/Output/AllSed_ForNetica.csv", 
          row.names = F)
```

### Water 

```r
# Join CEDENSURF to Source
CS_PRISM<- merge(Water_Wide, Source, by = c("Date", "Subregion"))
```


```r
# Merge with season data

CS_PRISM<- merge(Water_Wide, Source, by = c("Date", "Subregion"))

# Rename regions

CS_PRISM <- CS_PRISM %>% 
  mutate(Region = Subregion) %>%
  mutate(Region = str_replace(Region, "Central Delta", "Central")) %>%
  mutate(Region = str_replace(Region, "North Delta", "North")) %>%
  mutate(Region = str_replace(Region, "Sacramento River", "Sacramento")) %>%
  mutate(Region = str_replace(Region, "South Delta", "South")) %>%
  mutate(Region = str_replace(Region, "Suisun Bay", "Suisun"))

# Remove unnecessary columns

ForNetica <- CS_PRISM %>% select(!c(Subregion, Latitude, Longitude, Date, WaterYear, max_precip, d14_precipavg))

# Replace NA with *, which is how Netica deals with NA

ForNetica <- mutate_all(ForNetica, ~replace(., is.na(.), "*"))

# Save

write.csv(x = ForNetica, file = "Data/Output/AllWater_ForNetica.csv", 
          row.names = F)

# Writing directly to txt file does not play nice with Netica... Save as CSV then "save-as" txt

# write.table(x = ForNetica, file = "Data/Output/AllWater_ForNetica.txt", sep = "")
```

### Mini 

```r
## For example using Water Quality Parameters Subset and my limited-mode netica, I used just the WQP.Wide.water dataset:

# Load CEDENSURF Data (wide format)
WQ_Wide <- fread("https://github.com/WWU-IETC-R-Collab/CEDENSURF-mod/raw/main/Data/Output/WideSubsets/WQP.Wide.water.csv") 
    
# Join CEDENSURF to Source
CS_PRISM<- merge(WQ_Wide, Source, by = c("Date", "Subregion"))
```


```r
# Merge with season data

CS_PRISM<- merge(WQ_Wide, Source, by = c("Date", "Subregion"))

# Rename regions

CS_PRISM <- CS_PRISM %>% 
  mutate(Region = Subregion) %>%
  mutate(Region = str_replace(Region, "Central Delta", "Central")) %>%
  mutate(Region = str_replace(Region, "North Delta", "North")) %>%
  mutate(Region = str_replace(Region, "Sacramento River", "Sacramento")) %>%
  mutate(Region = str_replace(Region, "South Delta", "South")) %>%
  mutate(Region = str_replace(Region, "Suisun Bay", "Suisun"))

# Remove unnecessary columns

ForNetica <- CS_PRISM %>% select(!c(Subregion, Latitude, Longitude, Date, WaterYear, max_precip, d14_precipavg))

# Replace NA with *, which is how Netica deals with NA

ForNetica <- mutate_all(ForNetica, ~replace(., is.na(.), "*"))

# Save

write.csv(x = ForNetica, file = "Data/Output/WQP_ForNetica.csv", 
          row.names = F)

# Writing directly to txt file does not play nice with Netica... Save as CSV then "save-as" txt

# write.table(x = ForNetica, file = "Data/Output/WQP_ForNetica.txt", sep = "")
```

**On a Windows, Netica should be able to use the csv file directly. I found that in order for it to allocate continuously variables appropriately into the designated bins (ie: when MANY nodes were in one document), it needed to be a txt file. Re-saving the csv as txt worked fine, I also added code here to write directly to txt. **
