
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)


github_url <-"https://raw.githubusercontent.com/dsmehak/capstone/refs/heads/main/combined_data.csv"
combined_df <- read_csv(github_url)


#check for duplicates
duplicates <- duplicated(combined_df$`Crash Instance`)
combined_df[duplicates, ]

#clean time errors in data
combined_df$`Time of Day` <- gsub("12:00 Noon", "12:00 PM", combined_df$`Time of Day`)  
combined_df$`Time of Day` <- sub("^(\\d{1,2}:\\d{2} [APM]+).*", "\\1", combined_df$`Time of Day`)

unique(combined_df$`Time of Day`)

#check if times are all correct
time_levels <- c("12:00 AM", "1:00 AM", "2:00 AM", "3:00 AM", 
                 "4:00 AM", "5:00 AM", "6:00 AM", "7:00 AM",
                 "8:00 AM", "9:00 AM", "10:00 AM", "11:00 AM",
                 "12:00 PM", "1:00 PM", "2:00 PM", "3:00 PM",
                 "4:00 PM", "5:00 PM", "6:00 PM", "7:00 PM",
                 "8:00 PM", "9:00 PM", "10:00 PM", "11:00 PM", "12:00 PM")

unexpected_times <- combined_df$`Time of Day`[!combined_df$`Time of Day` %in% time_levels]
print(unexpected_times)

combined_df$`Time of Day` <- gsub("12:00 M", "12:00 PM", combined_df$`Time of Day`)

#now check again
unexpected_times <- combined_df$`Time of Day`[!combined_df$`Time of Day` %in% time_levels]
print(unexpected_times)

#filter unknown time of day
cleaned_df <- combined_df %>%
  filter(`Time of Day` != "Unknown" & !is.na(`Time of Day`))

#filter all NA values 
cleaned_df <- cleaned_df %>% 
  filter(
    !is.na(`Crash Latitude`), 
    !is.na(`Crash Longitude`), 
    !is.na(`Time of Day`), 
    !is.na(`Day of Week`), 
    !is.na(`Weather Conditions (2016+)`), 
    !is.na(`Lighting Conditions`),
    !is.na(`Road Conditions`),
    !is.na(`Crash: Pedestrian`),
    !is.na(`Crash: Drinking`),
    !is.na(`Crash: Drug Use`),
    !is.na(`Crash: Fatal Crash`),
    !is.na(`Crash: Injury Crash`),
  )

#filter NA latitudes and longitudes
cleaned_df <- cleaned_df %>% filter(!is.na(`Crash Latitude`), !is.na(`Crash Longitude`))


#------------------------------------------------------
#subset cleaned_df
cleaned_subdf <- cleaned_df[, c("Time of Day", "Day of Week", "Road Conditions", 
                                "Weather Conditions (2016+)", "Lighting Conditions", 
                                "Crash Latitude", "Crash Longitude", "Crash: Drinking", "Crash: Drug Use", "Crash: Older Driver", "Crash: Young Driver", "Crash: Fatal Crash", "Crash: Injury Crash","Crash: Pedestrian")]

cleaned_df <- cleaned_df %>%
  mutate(
    `Time of Day` = as.factor(`Time of Day`),
    `Day of Week` = as.factor(`Day of Week`),
    `Weather Conditions (2016+)` = as.factor(`Weather Conditions (2016+)`),
    `Lighting Conditions` = as.factor(`Lighting Conditions`),
    `Road Conditions` = as.factor(`Road Conditions`),
    `Crash Latitude` = as.numeric(`Crash Latitude`),
    `Crash Longitude` = as.numeric(`Crash Longitude`)
  )

colnames(cleaned_subdf) <- c("Time_of_Day", "Day_of_Week", "Road_Conditions", 
                             "Weather_Conditions", "Lighting_Conditions", 
                             "Crash_Latitude", "Crash_Longitude", "Drinking", "Drug_Use", "Older_Driver", "Young_Driver", "Fatal", "Injury", "Pedestrian")
colnames(cleaned_subdf)

#----------------------------------------------------------
heatmap_subdf <- cleaned_subdf

#convert 'yes'/'no' 
heatmap_subdf$Drinking <- ifelse(heatmap_subdf$Drinking == "Drinking Involved", 1, 
                                 ifelse(heatmap_subdf$Drinking == "No Drinking Involved", 0, heatmap_subdf$Drinking))

heatmap_subdf$Pedestrian <- ifelse(heatmap_subdf$Pedestrian == "Pedestrian Involved", 1, 
                                   ifelse(heatmap_subdf$Pedestrian == "No Pedestrian Involved", 0, heatmap_subdf$Pedestrian))

heatmap_subdf$Drug_Use <- ifelse(heatmap_subdf$Drug_Use == "Drugs Involved", 1, 
                                 ifelse(heatmap_subdf$Drug_Use == "No Drugs Involved", 0, heatmap_subdf$Drug_Use))
heatmap_subdf$Injury <- ifelse(heatmap_subdf$Injury == "Injury Crash (no fatalities)", 1, 
                               ifelse(heatmap_subdf$Injury == "No Injuries (may be fatalities)", 0, heatmap_subdf$Injury))

#Copy the columns from cleaned_df to heatmap_subdf
heatmap_subdf$Young_Driver <- cleaned_df$`Crash: Young Driver`
heatmap_subdf$Older_Driver <- cleaned_df$`Crash: Older Driver`
heatmap_subdf$`Worst_Injury` <- cleaned_df$`Worst Injury in Crash`

#Young_Driver to "Yes" or "No"
heatmap_subdf <- heatmap_subdf %>%
  mutate(
    Young_Driver = ifelse(
      Young_Driver %in% c("Driver Age 15", "Driver Age 16", "Driver Age 17"),
      1,
      0
    ),
    Older_Driver = ifelse(
      Older_Driver %in% c("Driver Age 75-84", "Driver Age 85 or Over"),
      1,
      0
    )
  )

#injury in crash order: K = 5, A = 4, B = 3, C = 2, 0 = 1
heatmap_subdf$Worst_Injury_Num <- case_when(
  heatmap_subdf$Worst_Injury == "Fatal Injury (K)" ~ 5,
  heatmap_subdf$Worst_Injury == "Suspected Serious Injury (A)" ~ 4,
  heatmap_subdf$Worst_Injury == "Suspected Minor Injury (B)" ~ 3,
  heatmap_subdf$Worst_Injury == "Possible Injury (C)" ~ 2,
  heatmap_subdf$Worst_Injury == "No Injury (O)" ~ 1,
  TRUE ~ NA_real_  
)

table(heatmap_subdf$Worst_Injury_Num)

table(heatmap_subdf$Injury)
table(heatmap_subdf$Drinking)
table(heatmap_subdf$Drug_Use)
table(heatmap_subdf$Pedestrian)
table(heatmap_subdf$Older_Driver)
table(heatmap_subdf$Young_Driver)

#----------------------------------------------------
#map_subdf
map_subdf <- heatmap_subdf 
#put in the code for creating the 'unsafe drivers' column
# Add 'Unsafe_Driver' column: 1 if any of the specified columns have a value of 1, otherwise 0
map_subdf <- map_subdf %>% 
  mutate(Unsafe_Driver = ifelse(Drinking == 1 | Drug_Use == 1 | Young_Driver == 1 | Older_Driver == 1, 1, 0))

#Add 'Unsafe_Conditions' column
map_subdf <- map_subdf %>%
  mutate(Unsafe_Conditions = ifelse(
    (Road_Conditions %in% c("Snow", "Mud, Dirt, Gravel", "Slush", "Ice", "Water (standing/moving)")) |
      (Weather_Conditions %in% c("Blowing Sand, Soil, Dirt", "Blowing Snow", "Severe Crosswinds", "Sleet/Hail", "Snow")) |
      (Lighting_Conditions == "Dark - Unlighted"), 1, 0))
  #fix col names
  
  #add urba/rural column to map_subdf
  map_subdf$`Rural_Urban` <- cleaned_df$`Rural/Urban Area (2016+)`
  
  #convert time_of_day into numeric (1 to 24)
  map_subdf$Time_of_Day <- as.POSIXct(map_subdf$Time_of_Day, format = "%I:%M %p")
  map_subdf$Time_of_Day <- as.numeric(format(map_subdf$Time_of_Day, "%H")) + 
    as.numeric(format(map_subdf$Time_of_Day, "%M")) / 60
  
  #convert other variables to factors
  map_subdf$Day_of_Week <- factor(map_subdf$Day_of_Week)
  map_subdf$Road_Conditions <- factor(map_subdf$Road_Conditions)
  map_subdf$Weather_Conditions <- factor(map_subdf$Weather_Conditions)
  map_subdf$Rural_Urban <- factor(map_subdf$Rural_Urban)

  #pedestrian should be binary
  map_subdf$Pedestrian <- as.numeric(map_subdf$Pedestrian)
  #numeric
  map_subdf$Worst_Injury_Num <- as.numeric(map_subdf$Worst_Injury_Num)
#-----------------------------------
# filter map_subdf to keep within the boundaries of Washtenaw County
#lat_min <- 42.08560
#lat_max <- 42.43517
#lon_min <- -84.13113
#lon_max <- -83.55190

#this will actually exclude a huge number of accidents,
#bc for some reason there's so many recorded that are outside this! so maybe, just for this, 
#expand the border and let it include farther

filtered_data <- map_subdf %>%
  filter(!is.na(Crash_Latitude), Crash_Latitude != 0,
         !is.na(Crash_Longitude), Crash_Longitude != 0)

print(min(filtered_data$Crash_Latitude, na.rm = TRUE))
print(max(filtered_data$Crash_Latitude, na.rm = TRUE))

print(min(filtered_data$Crash_Longitude, na.rm = TRUE))
print(max(filtered_data$Crash_Longitude, na.rm = TRUE))


#new min and max from min and max in the data, rather than the geographical boundaries
lat_min <- 42.0717
lat_max <- 42.43517
lon_min <- -84.13388
lon_max <- -83.55190

#map_subdf <- map_subdf %>% filter(Latitude >= lat_min & Latitude <= lat_max, Longitude >= lon_min & Longitude <= lon_max)

#---------------------------------------------------------------------
#try adding the grid thing


#https://gis.stackexchange.com/questions/442457/how-to-produce-a-gridded-map-in-r-where-each-grid-cell-represents-the-sum-of-poi

#create new 2d dat frame with stored grid of correct dimensions
#for washtenaw county, width = 29 miles, height = 25.07 miles

#https://www.meridianoutpost.com/resources/etools/calculators/calculator-latitude-longitude-distance.php?

#this is the old distance from the old boundaries: latitude = 0.362793, longitude = 0.579596
#new distances based on min and max of data: latitude = 0.36347, longitude = 0.58198
#divide each distance by 5, latitude = 0.072694, longitude = 0.116396

latitudes <- 42.0717 + (0:4) * 0.072694
print(latitudes)

longitudes <- -84.13388 + (0:4) * 0.116396
print(longitudes)

make_grid <- expand.grid(latitude = latitudes, longitude = longitudes)

make_grid$grid_letter <- LETTERS[1:25]

make_grid <- make_grid[, c("grid_letter", "longitude", "latitude")]

print(make_grid)

#yayyyy celebration emoji
#----------------------------------
#use grid to assign each row the correct value based on the crash location
#just loop through, check if: accident latitude is between make_grid latitude and latitude+0.0725586,
                              #and if accident longitude is between longitude and longitude+0.1159192

assign_grid <- function(lat, lon, grid_df) {
  match <- grid_df %>%
    filter(
      #use between bc the <= and < was rounding things
      between(lat, latitude, latitude + 0.072694), 
      between(lon, longitude, longitude + 0.116396) 
    )
  
  if (nrow(match) > 0) {
    return(match$grid_letter[1])
  } else {
    return(NA) 
  }
}

#rw by row use rowwise
map_subdf <- map_subdf %>%
  rowwise() %>%
  mutate(Grid_ID = assign_grid(Crash_Latitude, Crash_Longitude, make_grid)) %>%
  ungroup()

table(map_subdf$Grid_ID, useNA = "always")

#ok FINALLY the stupid thing worked ugh
map_subdf$Grid_ID <- factor(map_subdf$Grid_ID)

#-------------------------------------
