# R script to create surface and bottom dataset for WGINOSE regions.

# Download a dataset from the oceanographic database at
# https:\\ocean.ices.dk\core\odb covering
# the spatial extent of the WGINOSE regions and
# the temporal extent that you want to analyse

# For 2020
# Latitude: 48 to 61.2
# Longitde: -5 to 12.6
# Year: 1984 to 2019  

# Install and load R packages ---------------------------------------------
# Check to see if packages are installed. Install them if they are not,
# then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "ggplot2")
ipak(packages)

# Unzip regions
unzip("WGINOSE_2020.zip")

# Read regions 
regions <- st_read("WGINOSE_coastline_union_refactored_uten_DK.shp", query = "SELECT area_numbr AS Region FROM WGINOSE_coastline_union_refactored_uten_DK ORDER BY area_numbr") %>% st_sf()

# Unzip data
unzip("05033209.zip")  

# Read data
dt <- fread("05033209.txt", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)

# Filter data
dt <- dt[, list(
  Cruise,
  Station,
  Year,
  Month,
  Day,
  Hour,
  Minute,
  Latitude..degrees_north.,
  Longitude..degrees_east.,
  Bot..Depth..m.,
  Depth..m.,
  Temperature..degC.,Practical.Salinity..dmnless.,
  Dissolved.Oxygen..ml.l.,
  Phosphate.Phosphorus..PO4.P...umol.l.,
  Total.Phosphorus..P...umol.l.,
  Silicate.Silicon..SiO4.Si...umol.l.,
  Nitrate.Nitrogen..NO3.N...umol.l.,
  Nitrite.Nitrogen..NO2.N...umol.l.,
  Ammonium.Nitrogen..NH4.N...umol.l.,
  Total.Nitrogen..N...umol.l.,
  Hydrogen.Sulphide..H2S.S...umol.l.,
  Hydrogen.Ion.Concentration..pH...pH.,
  Alkalinity..mEq.l.,
  Chlorophyll.a..ug.l.
  )]

# Find values at surface i.e. 0-5 meters
dts <- dt[
  Depth..m. <= 5,
  head(.SD, 1),
  keyby=list(
    Cruise,
    Station,
    Year,
    Month,
    Day,
    Hour,
    Minute,
    Latitude..degrees_north.,
    Longitude..degrees_east.,
    Bot..Depth..m.
    )
  ]

dts <- dt[
  Depth..m. <= 5,
  .SD[1L],
  keyby=list(
    Cruise,
    Station,
    Year,
    Month,
    Day,
    Hour,
    Minute,
    Latitude..degrees_north.,
    Longitude..degrees_east.,
    Bot..Depth..m.
  )
  ]

# Make stations spatial keeping original latitude/longitude
dts <- st_as_sf(dts, coords = c("Longitude..degrees_east.", "Latitude..degrees_north."), remove = FALSE, crs = 4326)

# Classify stations into regions
dts <- st_join(dts, regions, join=st_intersects, left = FALSE)

# Remove spatial column
dts <- st_set_geometry(dts, NULL)

dts <- as.data.table(dts)

# Calculate bottom parameters mean and standard deviation
dts_ <- dts[, as.list(unlist(lapply(.SD, function(x) list(mn = mean(x, na.rm = TRUE), std = sd(x, na.rm = TRUE))))), by = .(Region, Year), .SDcols=c(names(dts[, 11:(ncol(dts) - 1)]))] %>%
  setkey(Region, Year)



# Find values at bottom i.e. 20 meters from bottom or 50 meters from bottom if depth > 100 meters
dtb <- dt[
  ifelse(
    Bot..Depth..m. > 100,
    Depth..m. > (Bot..Depth..m. - 50),     
    Depth..m. > (Bot..Depth..m. - 20)
    ),
  tail(.SD,1),
  keyby=list(
    Cruise,
    Station,
    Year,
    Month,
    Day,
    Hour,
    Minute,
    Latitude..degrees_north.,
    Longitude..degrees_east.,
    Bot..Depth..m.
    )
  ]

# Make stations spatial keeping original latitude/longitude
dtb <- st_as_sf(dtb, coords = c("Longitude..degrees_east.", "Latitude..degrees_north."), remove = FALSE, crs = 4326)

# Classify stations into regions
dtb <- st_join(dtb, regions, join=st_intersects, left = FALSE)

# Remove spatial column
dtb <- st_set_geometry(dtb, NULL)

dtb <- as.data.table(dtb)

# Calculate bottom parameters mean and standard deviation
dtb_ <- dtb[, as.list(unlist(lapply(.SD, function(x) list(mn = mean(x, na.rm = TRUE), std=sd(x, na.rm = TRUE))))), by = .(Region, Year), .SDcols=c(names(dtb[, 11:(ncol(dtb) - 1)]))] %>%
  setkey(Region, Year)

# Add Surface and Bottom to names
names(dts_)[-c(1:2)] <- paste("Surface", names(dts_)[-c(1:2)], sep = ".")
names(dtb_)[-c(1:2)] <- paste("Bottom", names(dtb_)[-c(1:2)], sep = ".")

# Merge surface and bottom parameters
dtsb_ <- merge(dts_, dtb_, all = TRUE, sort = TRUE)

# Write output
fwrite(dtsb_, "OceanographySurfaceBottom.csv")
