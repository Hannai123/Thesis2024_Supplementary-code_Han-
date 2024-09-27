rm(list = ls())
setwd("/Users/littlelai/Library/CloudStorage/OneDrive-WageningenUniversity&Research/6_Master Thesis/5 DataSet/1 Firm Data_")
getwd()

'
install.packages("tidyverse")
install.packages("tidygeocoder")
install.packages("janitor")
install.packages("readxl")
install.packages("readr")
install.packages("ggmap")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("data.table")
install.packages("reshape2")
install.packages("Hmisc")
install.packages("lubridate")
install.packages("plyr")
install.packages("esmisc")
install.packages("sandwich")
install.packages("MASS")
install.packages("e1071")
install.packages("terra")
install.packages("raster")
install.packages("sf")
install.packages("eurostat")
install.packages("remotes")
install.packages("ggspatial")
install.packages("prettymapr")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("weathermetrics")
install.packages("ncdf4")
install.packages("chron")
install.packages("RColorBrewer")
install.packages("lattice")
install.packages("viridis")
install.packages("R.utils")
install.packages("usethis")
'

library(tidygeocoder)
library(reshape2)
library(readxl)
library(readr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(gridExtra)
library(raster)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(terra)
library(raster)
library(janitor)
library(sf)
library(eurostat)
library(remotes)
library(ggspatial)
library(prettymapr)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridisLite)
library(weathermetrics)
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
library(viridis)
library(readxl)

# ======================================================================================================
#      Preparation Firm Data
# ======================================================================================================
# Read the file
data.1 <- data.frame(read_excel("240403_Wine1.xlsx"))
data.2 <- data.frame(read_excel("240403_Wine2.xlsx"))
data.3 <- data.frame(read_excel("240403_Wine3.xlsx"))

# Merge the data into ONE 
df_wine_raw <- bind_rows(data.1, data.2, data.3)
df_wine_raw <- df_wine_raw[ , -c(1, 129:195)]
 #str(df_wine_raw)

# Change columns name
df_wine_raw <- df_wine_raw %>% dplyr::rename(Company = Company.name.Latin.alphabet)
df_wine_raw <- df_wine_raw %>% dplyr::rename(NACE.Core = NACE.Rev..2..core.code..4.digits.)

# find duplicates 
duplicates_raw <- df_wine_raw %>%
  group_by(Company) %>%
  filter(n() > 1)

# Label duplicates
df_wine_raw <- df_wine_raw %>%
  group_by(Company) %>%
  dplyr:: mutate(Company = if_else(row_number() > 1, str_c(Company, row_number()), Company)) %>%
  ungroup()

# Delete Irrelevant columns 
df_wine <- df_wine_raw[, !(names(df_wine_raw) %in% c("Quoted","Fax.number", "Domain", "Website.address", "E.mail.address",
                                             "BvD.ID.number", "Telephone.number", "Consolidation.code",
                                             "Country..in.UK..Latin.Alphabet", "Woco", "Branch", "World.region",
                                             "Standardized.verification.code", "OwnData", "County..in.UK..Latin.Alphabet"))]

#class(df_wine)
str(df_wine)

#Clean the Company name 
df_wine <- df_wine %>%
  dplyr::mutate(
    Company = str_replace_all(Company, "[^A-Za-z0-9 ]", " ")) 

# Replace all n.a.(chr) and 0 value to NA (missing value)
df_wine <- df_wine %>%
  mutate_at(vars(5:64), ~ na_if(., 'n.a.')) %>%
  mutate_at(vars(5:64), ~ na_if(., '0'))

#Data Cleaning 
#Select Inactiveness 
filter_data <- function(data, column_name, filter_value){
  data %>% filter(get(column_name) == filter_value) }

###### Active: 20172--> 15676
df_wine <- filter_data(data = df_wine,
            column_name = "Inactive", 
            filter_value = "No")

##### NACE_Core Code: 
df_wine <- filter_data(data = df_wine,
                       column_name = "NACE.Core", 
                       filter_value = "1102")

colSums(is.na(df_wine))

# Check if columns from 6th to 65th are ALL NA for each row
all_na <- apply(df_wine[,5:64], 1, function(row) all(is.na(row)))

na_rows <- which(all_na)
na_rows #found 6059

# Remove the rows that are fully NA in the specified columns --> 9388 remained 
df_cleaned.firms <- df_wine[-na_rows, ]  

str(df_cleaned.firms)

# ======================================================================================================
#                                         Location Data Match   
# ======================================================================================================
#Split the dataframe df_location into two dataframes:  N/A values and one without
df_location_not_NA <- df_cleaned.firms %>% 
  filter(!is.na(Latitude) & !is.na(Longitude)) #-->6002 firms 

df_location_NA <- df_cleaned.firms %>% 
  filter(is.na(Latitude) | is.na(Longitude))  #-->3386 firms  

  #only keep the first 5 space in the column NUTS3 in df_location_NA --> to extract coordinates accoording to NUTS3
df_location_NA$NUTS3 <- substr(df_location_NA$NUTS3, 1, 5)

na.nuts3 <- which(is.na(df_location_NA$NUTS3)) # --> 88 firms do not have NUTS3 information

#===============================
# Get the NUTS3 level shapefiles
nuts3_sf <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = 3)

# Convert the sf object to a data frame for merging
nuts3_df <- as.data.frame(nuts3_sf)

# Perform a left join to keep all observations from df_location_NA
df_location_NA <- df_location_NA %>%
  left_join(nuts3_df, by = c("NUTS3" = "NUTS_ID"))

# Convert to centroids
nuts_centroids <- st_centroid(df_location_NA$geometry)

# Extract the coordinates
nuts_coordinates <- st_coordinates(nuts_centroids)

#Merge the coordinates into the df_location_NA
df_location_NA$Latitude <- nuts_coordinates[, 2]
df_location_NA$Longitude <- nuts_coordinates[, 1]

#find 88 N/A values in Latitude and Longitude --> Still 88 unfound 
na_rows.location <- which(is.na(df_location_NA$Latitude) & is.na(df_location_NA$Longitude))

#extract these 88 rows from df_location_NA #88
Still_missing_values_location <- df_location_NA[na_rows.location, ] #--> 88 firms 
#delete those 88 rows from df_location_NA
df_location_NA <- df_location_NA[-na_rows.location, ]


#=========Full fill 88 firms location by Google Maps=========
df_88firms <- data.frame(read_xls("geocoded_addresses.xls"))

# Ensure the column is character type
df_88firms$x.Latitude.Longitude <- as.character(df_88firms$x.Latitude.Longitude)

# Determine the maximum number of columns needed
max_columns <- max(sapply(strsplit(df_88firms$x.Latitude.Longitude, ","), length))

# Generate column names dynamically
column_names <- paste0("Part", 1:max_columns)

# Separate the column into multiple columns
df_88found <- df_88firms %>%
  separate(x.Latitude.Longitude, into = column_names, sep = ",", fill = "left", extra = "merge")

Still_missing_values_location <- Still_missing_values_location %>%
  mutate(Latitude = df_88found$Part10,
         Longitude = df_88found$Part11)

#===========================================================================

#In df_location_not_NA Define the function to convert DMS to decimal degrees
dms_to_decimal <- function(dms) {
  # Extract degrees, minutes, seconds, and direction using regular expression
  matches <- regmatches(dms, regexec("(-?\\d+)° (\\d+)' (\\d+(\\.\\d+)?)\" ([NSEW])", dms))[[1]]
  if (length(matches) == 6) {
    degrees <- as.numeric(matches[2])
    minutes <- as.numeric(matches[3])
    seconds <- as.numeric(matches[4])
    direction <- matches[6]
    
    decimal <- degrees + minutes / 60 + seconds / 3600
    if (direction == "S" || direction == "W") {
      decimal <- -decimal
    }
    return(decimal)
  } else {
    return(NA)
  }
}

# Convert Latitude and Longitude columns to decimal degrees
df_location_not_NA <- df_location_not_NA %>%
  mutate(
    Latitude_decimal = sapply(Latitude, dms_to_decimal),
    Longitude_decimal = sapply(Longitude, dms_to_decimal)
  )

df_location_not_NA <-  df_location_not_NA %>%
  dplyr::select(-Latitude, -Longitude)

df_location_not_NA <- df_location_not_NA %>%
  dplyr::rename(Latitude = Latitude_decimal, Longitude = Longitude_decimal)

#===============================
#Merge three Dataframes together
colnames(Still_missing_values_location) <- gsub("\\.\\.\\.\\d+", "", colnames(Still_missing_values_location))
colnames(df_location_NA) <- gsub("\\.\\.\\.\\d+", "", colnames(df_location_NA))

df_NA_ALLFound <- rbind(Still_missing_values_location, df_location_NA)

df_NA_ALLFound <- df_NA_ALLFound %>% 
  dplyr::select(1:66, 77:78)

#=========
df_location_not_NA <- df_location_not_NA %>% 
  dplyr::select(1:66, 113:114)

colnames(df_location_not_NA) <- gsub("\\.\\.\\.\\d+", "", colnames(df_location_not_NA))

df_combined <- rbind(df_NA_ALLFound, df_location_not_NA)
str(df_combined)
# ======================================================================================================
#                       Transform the data set structure to a "Long" data set   
# ======================================================================================================
long_data <- reshape2::melt(df_combined, id.vars = c("Company","Inactive", "Country.ISO.code", "NACE.Core", "NACE.Rev..2..primary.code.s.","NACE.Rev..2..secondary.code.s.",
                                                     "Latitude","Longitude"),
                                       variable.name = "FP", value.name = "EURO") %>% arrange(Company) 


#Separate the year 
long_data <- separate(long_data,FP, sep = ".th.EUR.", into = c("FiancialPer", "Year"))

df_Panel <- long_data %>%
  pivot_wider(names_from = FiancialPer,
    values_from = EURO)

# Change the variable names 
df_Panel <- df_Panel %>% dplyr::rename(Operating.Revenue = Operating.revenue..Turnover..)
df_Panel <- df_Panel %>% dplyr::rename(Total.assets = Total.assets.)
df_Panel <- df_Panel %>% dplyr::rename(Shareholders.funds = Shareholders.funds.)
df_Panel <- df_Panel %>% dplyr::rename(Operating.profit = Operating.profit..loss...EBIT..)
df_Panel <- df_Panel %>% dplyr::rename(Material.costs = Material.costs.)
df_Panel <- df_Panel %>% dplyr::rename(Export.revenue = Export.revenue.)
df_Panel <- df_Panel %>% dplyr::rename(NACE.Core.primary = NACE.Rev..2..primary.code.s.)
df_Panel <- df_Panel %>% dplyr::rename(NACE.Core.secondary = NACE.Rev..2..secondary.code.s.)


# Convert the specified columns
df_Panel <- df_Panel %>%
  dplyr::mutate(across(all_of(c("NACE.Core", "NACE.Core.primary", "NACE.Core.secondary", "Year",
                                "Operating.Revenue", "Total.assets", "Shareholders.funds", "Operating.profit",
                         "Material.costs", "Export.revenue","Latitude", "Longitude")), as.numeric))
str(df_Panel)
#=======================================
# Drop rows with any NA values --> 49680
df_Panel.na <- df_Panel %>% drop_na(Operating.Revenue, Operating.profit, Material.costs)

#####
# Count occurrences of each company --> 7019
counts.na <- count(df_Panel.na$Company)

#Filter out 0 and negative values --> 7007 
df_Panel.above.0 <- df_Panel.na %>%
  filter(Operating.Revenue > 0, Material.costs >0)

counts.above.0 <- count(df_Panel.above.0$Company)


# ======================================================================================================
#                                               Temperature data
# ======================================================================================================
# Define start & end date of panel (pick first and las day of year, respectively)
start_panel <- as.Date("2013-01-01", format ="%Y-%m-%d")
end_panel   <- as.Date("2022-12-31", format="%Y-%m-%d")

# Path to the NetCDF file
Tmax <- stack("tx_ens_mean_0.1deg_reg_v29.0e.nc")

# Each layer in Tmax is a year.
first_layer_date <- as.Date(substring(Tmax[[1]]@data@names,2), format = "%Y.%m.%d")
last_layer_date  <- as.Date(substring(Tmax[[dim(Tmax)[3]]]@data@names,2), format = "%Y.%m.%d")
seq_layers_dates <- seq(as.Date(first_layer_date), as.Date(last_layer_date), "days")
sub_layers_panel <- which(seq_layers_dates >= start_panel & seq_layers_dates <= end_panel)

sub_Tmax <- subset(Tmax, sub_layers_panel)   # Save sub_Tmax to a text file
{          
  sink("sub_Tmax_output.txt")
  print(sub_Tmax)
  sink()
}

# Remove large dataset from global environment
rm(Tmax, first_layer_date,last_layer_date)

# -----------------------------------------
# Match farm locations with weather data --> Temperature 
# -----------------------------------------
# Define the extent of the raster data
raster_extent <- extent(sub_Tmax)

# Check if the farm locations are within the extent of the raster data
firm_in_range <- df_Panel.above.0[df_Panel.above.0$Longitude >= xmin(raster_extent) &
                                    df_Panel.above.0$Longitude <= xmax(raster_extent) &
                                    df_Panel.above.0$Latitude >= ymin(raster_extent) &
                                    df_Panel.above.0$Latitude <= ymax(raster_extent), ]

# Find the location that out of the range 
firm_out_of_range <- df_Panel.above.0[!(df_Panel.above.0$Longitude >= xmin(raster_extent) &
                                          df_Panel.above.0$Longitude <= xmax(raster_extent) &
                                          df_Panel.above.0$Latitude >= ymin(raster_extent) &
                                          df_Panel.above.0$Latitude <= ymax(raster_extent)), ]

# How many firms in the range --> 6997
nrow(firm_in_range)
range.in <- count(firm_in_range$Company) 

# Haw many firms are not in the range --> 10 
nrow(firm_out_of_range)
range.out <- count(firm_out_of_range$Company) 

#=======================================
# Create a SpatialPointsDataFrame object
coordinates(firm_in_range) <- ~ Longitude + Latitude
proj4string(firm_in_range) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

Tmax_firm <- data.frame(Latitude = firm_in_range$Latitude,
                        Longitude = firm_in_range$Longitude,
                        Company = firm_in_range$Company,
                        matrix(NA, ncol = length(seq_layers_dates[sub_layers_panel]), nrow = nrow(firm_in_range)))

# Rename columns to the DATE 
colnames(Tmax_firm)[4:ncol(Tmax_firm)] <- paste(seq_layers_dates[sub_layers_panel], sep = "_")

#Store data in Tmax_firm data frame
for (t in 1:length(sub_layers_panel)) {
  Tmax_firm[, t + 3] <- raster::extract(sub_Tmax[[t]], firm_in_range)
}

#==================================================================
#Only keep the Complete row from March to November (03-01 to 11-01)
dates_to_keep <- sapply(seq_layers_dates[sub_layers_panel], function(date) {
  month(date) >= 3 & (month(date) < 11 | (month(date) == 11 & day(date) == 1))
})
dates_to_keep <- which(dates_to_keep)

Tmax_firm_filtered <- Tmax_firm[, c(1, 2, 3, dates_to_keep + 3)]

Tmax_firm_filtered <- Tmax_firm_filtered[complete.cases(Tmax_firm_filtered), ] #Here, we only keep the complete cases thus the NA values are removed

#a <- count(Tmax_firm_filtered$Company) #6843 

write.csv(Tmax_firm_filtered, "Tmax_firm_output(3-11).csv", row.names = FALSE)
#==================================================================
Tmax_firm_filtered <- read_csv("Tmax_firm_output(3-11).csv")
#Calculate the New Threshold 
#str(Tmax_firm_filtered)

# Ensure that the column names are correctly formatted
colnames(Tmax_firm_filtered) <- gsub("X", "", colnames(Tmax_firm_filtered))  # Remove 'X' prefix if present

# Define identifier columns (replace with actual identifiers in your data)
identifier_columns <- c("firm_id")

# Extract unique years from column names
years <- unique(substr(colnames(Tmax_firm_filtered)[-(1:length(identifier_columns))], 1, 4))

# Initialize a dataframe to store the yearly averages
yearly_avg <- Tmax_firm_filtered %>%
  rowwise() %>%
  mutate(across(
    .cols = -all_of(identifier_columns), 
    .fns = list(year = ~mean(c_across(starts_with(substr(cur_column(), 1, 4))), na.rm = TRUE)), 
    .names = "{substr(.col, 1, 4)}_avg"
  ))

# Combine the yearly averages with the original data
data_with_avg <- bind_cols(Tmax_firm_filtered, yearly_avg)

##Calculate the number of days above 35 degrees
above_threshold_counts <- integer(nrow(Tmax_firm_filtered))

#====================
#Define the threshold
threshold <- 35
#35/37/39

# Extract column names
date_columns <- colnames(Tmax_firm_filtered)[4:2463]

# Extract years from column names
years <- substr(date_columns, 1, 4)
unique_years <- unique(years)

# Initialize a dataframe to store the counts
above_threshold_counts <- data.frame(matrix(nrow = nrow(Tmax_firm_filtered), ncol = length(unique_years)))
colnames(above_threshold_counts) <- paste0("above_35_", unique_years)

# Loop over each year
for (year in unique_years) {
  # Get the columns for the current year
  year_columns <- date_columns[years == year]
  
  # Calculate the count for each row
  for (i in 1:nrow(Tmax_firm_filtered)) {
    row_values <- Tmax_firm_filtered[i, year_columns, drop = FALSE]
    above_threshold_counts[i, paste0("above_35_", year)] <- sum(row_values > threshold)
  }
}

# Combine the original dataframe with the counts dataframe
Tmax_firm_filtered <- cbind(Tmax_firm_filtered, above_threshold_counts)
 
#===================================================================== 

# 1. Delete the single date temperature data
Tmax_Above.35 <- Tmax_firm_filtered[, -c(4:2463)]
write.csv(Tmax_Above.35, "Tmax_Above.35.Simplify_output.csv", row.names = FALSE)

#===========================================
#I manually change the threshold to 35 and 37, 39, respectively in line 437. 
#Therefore repeat the above steps to get the number of days above 35, 37, 39 degrees and save the output as csv. file

Tmax_Above.33 <- read_csv("Tmax_Above.33.Simplify_output.csv")
Tmax_Above.37 <- read_csv("Tmax_Above.37.Simplify_output.csv")
Tmax_Above.39 <- read_csv("Tmax_Above.39.Simplify_output.csv")

#============================================================
a <- count(Tmax_Above.35$Company) #6843 firms
a.filter <- a$x

Tmax.match.year <- df_Panel.above.0 %>%
  filter(Company %in% a.filter)

Tmax_merged <- Tmax.match.year %>%
  left_join(Tmax_Above.35, by = c("Company"))

Tmax_merged_unique <- Tmax_merged %>%
  distinct(Company, Year, .keep_all = TRUE) %>%  # Remove duplicates
  dplyr::select (-Latitude.y, -Longitude.y)  

#Convert to the Long data set 
Tmax.Panel.Firms <- reshape2::melt(Tmax_merged_unique, id.vars = c("Company","Inactive","Country.ISO.code", "NACE.Core", "NACE.Core.primary","NACE.Core.secondary",
                                                     "Latitude.x","Longitude.x", "Year", "Operating.Revenue", "Total.assets", "Shareholders.funds", "Operating.profit","Material.costs","Export.revenue"),
                            variable.name = "Year.1", value.name = "Days.Above35") %>% arrange(Company, Year) 
# Define the sequence from 2013 to 2022
years <- 2013:2022

# Create a repeating sequence to match the number of rows in the dataframe
year_sequence <- rep(years, length.out = nrow(Tmax.Panel.Firms))

# Replace the YearColumn with the repeating sequence
Tmax.Panel.Firms <- Tmax.Panel.Firms %>%
  mutate(Year.1 = year_sequence)

# Filter the rows where Year matches Days.Above35_Year
Tmax.Panel.Firms.35 <- Tmax.Panel.Firms %>%
  filter(Year.1 == Year) 

write.csv(Tmax.Panel.Firms.1, "Tmax.Panel.Firms.1", row.names = FALSE)
#==========================================================
# Filter out companies that do not appear more than 3 times
# Step 1: Count the occurrences of each company
b <- count(Tmax.Panel.Firms.35$Company) 

# Step 2: Filter companies with at least 3 occurrences
companies_to_keep <- b %>%
  filter(freq >= 3) 

# Step 3: Filter the original dataframe based on these companies
df_cleaned.35 <- Tmax.Panel.Firms.35 %>%
  filter(Company %in% companies_to_keep$x)

a <- count(df_cleaned.35 $Company)

write.csv(df_cleaned, file = "df_cleaned.csv", row.names = FALSE)

#==========================================
df_cleaned <- read_csv("df_cleaned.csv")
a <- count(df_cleaned$Company)

#==========================================
#Add precipitation data 
#I used the same CODE as the temperature (>35) data to extract the precipitation data, and export it as the csv. file
TRain <- read_csv("TRain_firm_output(3-11).csv")

years <- substr(names(TRain)[4:2463], 1, 4)

# Initialize an empty list to store the yearly sums
yearly_sums <- list()

# Loop through each unique year and calculate the row-wise sums for that year
for (year in unique(years)) {
  # Find columns that correspond to the current year
  columns_for_year <- which(years == year) + 3  # Adjusting the index because years starts from the 4th column
  # Calculate the row-wise sum for the current year
  yearly_sums[[year]] <- rowSums(TRain[, columns_for_year], na.rm = TRUE)
}

# Combine the yearly sums into a new dataframe
TRain_yearly <- as.data.frame(yearly_sums)

# Combine the yearly sums with the initial dataframe
TRain_combined <- cbind(TRain, TRain_yearly)

# Remove intermediate columns with dates
TRain_combined <- TRain_combined %>%
  dplyr::select(-c(4:2463))

#Rename the columns
names(TRain_combined) <- c("Latitude.x","Longitude.x", "Company", 
                           "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

str(TRain_combined)

#Transfer long TRain data to short form 
TRain <- reshape2::melt(TRain_combined, id.vars = c("Company", "Latitude.x","Longitude.x"), 
                              variable.name = "Year", value.name = "Sum_Precipitation") %>% arrange (Company)

#delete duplicate rows 
TRain_unique <- unique(TRain)
b <-count(TRain_unique$Company)

#Merge two dataframes together 
TRain_unique$Year <- as.numeric(as.character(TRain_unique$Year))

df_Panel <- df_cleaned %>%
  left_join(TRain_unique, by = c("Company", "Latitude.x", "Longitude.x", "Year"))

d <- count(TRain_merge$Company) #5864


write.csv(df_Panel, file = "df_Panel.csv", row.names = FALSE)
# ======================================================================================================
#                                         Descriptive statistics 
# ======================================================================================================
summary(df_Panel)

sd(df_Panel$Total.assets, na.rm = TRUE)

str(df_Panel)

rows_with_na <- df_Panel %>% filter(is.na(Total.assets))



# ======================================================================================================
#                              Picture->Summary Country Info in EUROSTAT  
# ======================================================================================================
#Withdraw the column of "Company", "Country" from df_cleaned
c <- df_cleaned[, c("Company", "Country.ISO.code")]

country_code <- read_xlsx("Country_code.xlsx")

#merge c and country_code together by Country.ISO.code
c <- merge(c, country_code, by = "Country.ISO.code")

country_freq <- c %>%
  distinct() %>%  # Remove duplicates across both columns
  group_by(Country.ISO.code) 

c.1 <- count(country_freq$Country)

sum(c.1$Frequency)


names(c.1) <- c("Country", "Frequency")

# Sort by frequency
country_freq <- c.1[order(c.1$Frequency, decreasing = TRUE), ]

# Create a ggplot
p1 <- ggplot(country_freq, aes(x = reorder(Country, Frequency), y = Frequency, fill = Frequency)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = 0.2, hjust = 0.18, size = 2.8, color = "black", fontface = "bold") +
  labs(title = "Number of Wine Processors by Country in Europe in 2020\nSource:ORBIS", x = "Country", y = "Frequency") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 5)) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7),  # Change font size for y-axis labels
        axis.text.x = element_text(vjust = 1, hjust = 1)) +
  coord_flip()  # Flip the plot horizontally
p1
################################# EUROSTAT Compare 
df_EUROSTAT <- read_excel("sbs_na_ind_r2_page_spreadsheet.xlsx", sheet = "Split") 

df_EUROSTAT <- df_EUROSTAT %>%
  mutate(Number = as.numeric(Number))  # Rename

df_EUROSTAT <- df_EUROSTAT[order(df_EUROSTAT$Number, decreasing = TRUE), ]

# Create a ggplot
p2 <- ggplot(df_EUROSTAT , aes(x = reorder(Country, Number), y = Number, fill = Number)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Number), vjust = 0.1, hjust = 0.18, size = 2.8, color = "black", fontface = "bold") + # Change font size and style for country labels
  scale_fill_gradient(high = "darkgreen", low = "lightgreen") + # Gradient from dark orange to light orange
  labs(title = "Number of Manufacture of Wine from Grape in 2020\nSource:EUROSTAT", x = "Country", y = "Frequency") +
  theme(plot.title = element_text(size = 10)) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7),  # Change font size for y-axis labels
        axis.text.x = element_text(size = 10)) +  # Change font size for x-axis labels
  coord_flip()  # Flip the plot horizontally
p2
grid.arrange(p1, p2, ncol = 2)



# ======================================================================================================
#                              Visualize the Firm location in MAP   
# ======================================================================================================
# Convert the cleaned data frame to an sf object
firm_sf <- st_as_sf(combined_df.location, coords = c("Longitude", "Latitude"), crs = 4326)

#================================================
world <- ne_countries(scale = "medium", returnclass = "sf")

#Only Select the countries of interest from the 
countries_of_interest <- unique(combined_df.location$Country)
world_filtered <- world %>% filter(name_long %in% countries_of_interest)

# 获取每个国家的质心（用于显示国家名字）
centroids <- st_point_on_surface(world_filtered)

# Set the bounding box for Europe
europe_bbox <- st_bbox(c(xmin = -10, xmax = 30, ymin = 35, ymax = 70), crs = st_crs(4326))

ggplot() +
  geom_sf(data = world, fill = "gray80", color = "white") +  # 绘制所有国家，颜色为灰色
  geom_sf(data = world_filtered, aes(fill = name_long), color = "white") +  # 仅绘制公司所在国家，颜色根据国家名称填充
  geom_sf(data = firm_sf, aes(color = Country), size = 2) +
  geom_sf_text(data = world_filtered, aes(label = name_long), size = 2.4, color = "black") +  # 添加国家名字
  coord_sf(xlim = c(europe_bbox$xmin, europe_bbox$xmax), ylim = c(europe_bbox$ymin, europe_bbox$ymax), expand = FALSE) +
  scale_fill_viridis_d(option = "C", guide = FALSE) +  # 你可以选择一个颜色调色板，并且隐藏图例
  scale_color_viridis_d(option = "C") + # 你可以选择一个颜色调色板，并且隐藏图例
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", title = "The map of European Wine Processor's Location") +  # Set axis labels and title
  theme(
    plot.title = element_text(hjust = -1),  # Move title to the left
    legend.position = "right"
  )

#############
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Convert the cleaned data frame to an sf object
firm_sf <- st_as_sf(combined_df.location, coords = c("Longitude", "Latitude"), crs = 4326)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Select the countries of interest from the data
countries_of_interest <- unique(combined_df.location$Country)
world_filtered <- world %>% filter(name_long %in% countries_of_interest)

# Get centroids of the countries (for displaying country names)
centroids <- st_point_on_surface(world_filtered)

# Set the bounding box for Europe
europe_bbox <- st_bbox(c(xmin = -10, xmax = 30, ymin = 35, ymax = 70), crs = st_crs(4326))

# Create the plot
ggplot() +
  geom_sf(data = world, fill = "gray80", color = "white") +  # Draw all countries in gray
  geom_sf(data = world_filtered, fill = "lightblue", color = "white") +  # Draw selected countries in light blue
  geom_sf(data = firm_sf, color = "gray50", size = 0.7) +  # Plot firm locations in red
  geom_sf_text(data = world_filtered, aes(label = name_long), size = 2.4, color = "black") +  # Add country names
  coord_sf(xlim = c(europe_bbox$xmin, europe_bbox$xmax), ylim = c(europe_bbox$ymin, europe_bbox$ymax), expand = FALSE) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", title = "The map of European Wine Processor's Location") +  # Set axis labels and title
  theme(
    plot.title = element_text(hjust = -1),  # Move title to the left
    legend.position = "none"  # Hide the legend
  )







