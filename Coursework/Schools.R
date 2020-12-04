library(sf)
library(tidyverse)
library(here)
library(stringr)
library(ggplot2)
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)

#Reading in all schools basefile
AllSchools <- read.csv(here::here("edubasealldata.csv"))

#Filter


str(AllSchools)

#FILTERING OUT FOR 16_18 students

#First, get all the schools that are operating (schools that are open (as of 2019))

OpenSchools <- AllSchools %>% 
  dplyr::filter(str_detect(EstablishmentStatus..name., "Open"))

hassixthform <- OpenSchools %>% 
  dplyr::filter(OfficialSixthForm..code.==1)

#filter by phase of education (16 plus and all through)
sixteenplus <- OpenSchools %>% 
  dplyr::filter(str_detect(PhaseOfEducation..name.,"16 plus|All-through"))

#Get secondary schools that have greater than 16 as statutory high age
SecondarySchools <- OpenSchools %>% 
  dplyr::filter(str_detect(PhaseOfEducation..name.,"Secondary")) %>% 
  dplyr::filter(StatutoryHighAge > 16)

#Get colleges
Colleges <- OpenSchools %>% 
  dplyr::filter(str_detect(EstablishmentTypeGroup..name.,"Colleges"))

#Merging these three to get all the schools i want to look at                
UK16plus <- rbind(SecondarySchools,sixteenplus,hassixthform,Colleges) %>% 
  distinct()

#Now filter for LONDON
LondonSchools <- UK16plus %>% 
  dplyr::filter(str_detect(DistrictAdministrative..code., "^E09"))


#plot
#Create a simplefeatures object out of the LondonSchools
LondonSchools_sf <- LondonSchools %>% 
  st_as_sf(., coords = c("Easting", "Northing")) %>% 
  st_set_crs(27700)
#Plot
ggplot() +
  geom_sf(data = LondonSchools_sf) +
  ggtitle("Map of School Locations within London")

#Now load London LSOA shapefile
LSOA <- st_read(here::here("statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")) %>% 
  st_transform(27700)


#Remove points outside of London
LondonSchools_sf <- LondonSchools_sf[LSOA,]


st_crs(LondonSchools_sf)
st_crs(LSOA)

#check to see that we're looking at the right thing
tmap_mode("view")
tm_shape(LSOA) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(LondonSchools_sf) +
  tm_dots(col = "green")

#read in the catchment flow CSV
Catchment <- read.csv(here::here("Catchments_SecSchootoLSOA_2016_LDS.csv"))

#Getting unique values just to check what schools are there
CatchmentDistinct <- unique(Catchment$ï..Secondary_School_URN)

#Now filter out the ones so we only get the ones with flows
FinalSchools <- filter(LondonSchools_sf, URN %in% CatchmentDistinct)

tmap_mode("view")
tm_shape(LSOA) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(LondonSchools_sf) +
  tm_dots(col = "green")+
  tm_shape(FinalSchools) + 
  tm_dots(col="blue")

#Filter the catchment data to only the ones we want
URNs <- unique(FinalSchools$URN)
FinalCatchment <- subset(Catchment, ï..Secondary_School_URN %in% URNs)

#Cleaning the data (remove unecessary columns):
FinalCatchment <- dplyr::select(FinalCatchment, -c(Secondary2LSOA_Flow_No., LSOA_NAME))
FinalCatchment <- FinalCatchment %>% rename(URN="ï..Secondary_School_URN")

#Merge geometry column from schools to flow dataframe
CatchmentWithGeometry <- dplyr::left_join(FinalSchools,FinalCatchment,by="URN")

#Simple table
FlowsWithGeometry <- dplyr::select(CatchmentWithGeometry, c(Secondary_School_Name,LSOA_CODE, Pupil_count,geometry))
#Need to add LSOA geometry
LSOA %>% rename(LSOA_CODE="LSOA11CD")
FlowsWithGeometry <-st_join(FlowsWithGeometry,LSOA,by="LSOA_CODE")


#take centroid of LSOA areas (st_centroid)
st_write(LSOA,"LSOA.shp")

#School location as a sf point object and a separate sf object with LSOA centroids
