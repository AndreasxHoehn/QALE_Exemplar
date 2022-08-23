### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2022-08-22
# About: The Main Control File

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Preparation ###

# deep clean work space
rm(list = ls())  
gc(full = TRUE) 

# set seed 
set.seed(20220819) 

# start benchmarking time 
benchmark_time <- list() 
benchmark_time$start <- Sys.time()

# initialize definitions
definitions <- list()

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Libraries ###

# List of Required Packages 
RequiredPackages <- c("here",                               # Folder Structure
                      "data.table",                         # Dialect 
                      "svDialogs",                          # User Input
                      "nomisr","readstata13","HMDHFDplus",  # Data Queries
                      "splines",                            # Analysis
                      "ggplot2",                            # Data Visualization
                      "maps","rgeos","rgdal","maptools",    # Mapping
                      "rmarkdown","tinytex","knitr")        # Report

# ensure all packages are installed and loaded 
.EnsurePackages <- function(packages_vector) {
  new_package <- packages_vector[!(packages_vector %in% 
                                     installed.packages()[, "Package"])]
  if (length(new_package) != 0) {
    install.packages(new_package) }
  sapply(packages_vector, suppressPackageStartupMessages(require),
         character.only = TRUE)
}
.EnsurePackages(RequiredPackages)

# initialize relative paths 
here <- here::here 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Definitions: Dialogs ###

# HMD Account 
# register beforehand on website https://former.mortality.org/ 
definitions$HMD_access <- c("","")       # Username and PW
definitions$HMD_access[1] <- dlgInput("Enter HMD Username:", Sys.info()["user"])$res
definitions$HMD_access[2] <- dlgInput("Enter HMD PW:", Sys.info()["user"])$res

# US Download
definitions$US_download <- dlgInput(paste0("Have you downloaded Understanding ",
    "Society Survey data and stored the data in the respective folder (yes/no):"),
    Sys.info()["user"])$res

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Definitions: Study Design ###

# Study-Specific Parameters
definitions$geo_type         <- "TYPE432" # = LA district boundaries of '21 
definitions$xsection_year    <- 2019      # = year for Xsectional LE/ QALE results
definitions$US_wave          <- "j"       # = default Wave of US | "j" = 2019 .. "e" = 2014
definitions$excluding_geographies <- TRUE # exclusion of some geographies is recommended 
definitions$excluding_geographies_list <- c("City of London",  # list of excluded 
                                            "Isles of Scilly") # geographies    
definitions$mort_example_LA  <- "Shetland Islands" # for mortality smoothing example  
definitions$mort_example_sex <- "Male"           # for mortality smoothing example 

# Plots and Maps
definitions$point_size <- 1.0
definitions$line_size  <- 0.5
definitions$fig_width  <- 25  # = width of output figures in cm
definitions$fig_height <- 20  # = height of output figures in cm
definitions$map_cols   <- colorRampPalette(c("#559999","grey80","#BB650B"))(3)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Source Code Files ###

# always run
source("RCode/02_user_functions.R")

# run analysis
source("RCode/03_estimate_LTB.R")

if (definitions$US_download == "yes") {
  print(paste0("You have downloaded Understanding Society. Program continues ", 
               "with wave: ", definitions$US_wave))
  source("RCode/04_sf12_eq5d_mapping.R")
} else {print(paste0("Understanding Society not downloaded - proceeding with ",
                      "default and pre-estimated wave j health data."))}

source("RCode/05_estimate_QALE.R")
source("RCode/06_create_maps.R")

# Render readme - html Format #
rmarkdown::render(input = "RCode/07_draft_readme.Rmd",
            output_file = here::here("readme.html"),
         output_format = "html_document")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Benchmark Time ###

benchmark_time$end <- Sys.time()
print("Duration of Program:")
print(round(benchmark_time$end - benchmark_time$start), 
      definitions$rounding_results)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #