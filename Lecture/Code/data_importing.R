setwd("D:/OneDrive/Postdoctor/Quantitative_course/Week5") # this is the path I store my code and data, you need to change

# read CSV file
fish_data_csv <- read.csv("Data/fish_data.csv")
fish_data_csv

# read Excel file
library(readxl)
lake_data_xlsx <- read_excel("Data/lake_env_data.xlsx",sheet = 2)
lake_data_xlsx

# read R-specific format (.rds)
fish_data_rds <- readRDS("Data/fish_data.rds")
fish_data_rds

# read Shapefile
library(sf)
lake_erie_shapefile <- sf::st_read("Data/lake_erie_shape/hydro_p_LakeErie.shp")
class(lake_erie_shapefile)
View(lake_erie_shapefile)
plot(lake_erie_shapefile$geometry)

# read nc 
library(ncdf4)
lake_temp_nc <- nc_open("Data/demo_lake_temp_3d.nc")
# Inspect the structure
print(lake_temp_nc)

# Read coordinate (dimension) variables
depth <- ncvar_get(lake_temp_nc , "depth")    # numeric (m)
time  <- ncvar_get(lake_temp_nc , "time")     # seconds since 1970-01-01 (as defined)
lake <- ncvar_get(lake_temp_nc, "lake")
# Convert time to POSIXct for readability
time <- as.POSIXct(time, origin = "1970-01-01")

# Extract the temperature variable
temp <- ncvar_get(lake_temp_nc, "temperature")

plot(temp[1,1,])

### Reading Multiple Files at Once
# List all files 
all_files <- list.files("Data/fish_range", full.names = TRUE)
all_files

# Read them all into a list
all_list_csv <- lapply(all_files, read.csv)
class(all_list)
View(all_list)

# Combine into one data frame
all_data <- dplyr::bind_rows(all_list)


# List all files that contain "Cottus" in their name
cottus_files <- list.files("Data/fish_range",
                           pattern = "Cottus",
                           full.names = TRUE)
cottus_files

# Read them all into a list
cottus_list <- lapply(cottus_files, read.csv)

# Combine into one data frame
cottus_data <- dplyr::bind_rows(cottus_list)

