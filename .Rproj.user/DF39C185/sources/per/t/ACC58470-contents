### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2022-08-22
# About: file contains code to access NOMIS and NRS online resources

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Query Death Data ### 

## initialize a vector corresponding to query years defined 
definitions$query_years      <- c(2013,2020) # = longest overlapping period for query
definitions$query_years_long  <- rep(definitions$query_years[1]:
                                     definitions$query_years[2], times = 2)

## England and Wales ###

# access nomis 
LA_deaths_EW    <- data.table::data.table(nomisr::nomis_get_data(id = "NM_161_1",
          geography = definitions$geo_type, cause_of_death = 0, gender = c(1,2),
          time = c(definitions$query_years[1]:definitions$query_years[2]), 
          age = seq(1:20), measure = 1, measures = 20100, tidy = TRUE))

# introduce ctr_code
LA_deaths_EW[, ctr_code := substr(geography_code, 1, 1)]

# reduce data 
LA_deaths_EW <- LA_deaths_EW[, c("date_name", "ctr_code", "geography_name",
           "geography_code","gender_name","age_name","obs_value"), with = FALSE]

# rename columns
setnames(LA_deaths_EW, old = "gender_name", new = "sex_name")
setnames(LA_deaths_EW, old = "obs_value", new = "Dx")

# assure correct variable type
LA_deaths_EW[, date_name      := as.numeric(date_name)]
LA_deaths_EW[, geography_name := as.character(geography_name)]
LA_deaths_EW[, geography_code := as.character(geography_code)]
LA_deaths_EW[, sex_name       := as.character(sex_name)]
LA_deaths_EW[, age_name       := as.character(age_name)]
LA_deaths_EW[, Dx             := as.numeric(Dx)]

# ---------- #

## Scotland ##

# access zip files online
download.file(url = file.path(paste0("https://www.nrscotland.gov.uk/files//",
          "statistics/time-series/death-20/deaths-time-series-20-dt.13.zip")), 
              destfile = file.path("RData/ScotlandDownload/sco_deaths.zip"), mode = "wb")

# unzip and store locally 
unzip(zipfile = "RData/ScotlandDownload/sco_deaths.zip",
        exdir = "RData/ScotlandDownload/")

# run function and append to death data from E&W
LA_deaths_SCO <- vector(mode = "list",
    length = length(definitions$query_years_long))

for (i in 1 : (length(LA_deaths_SCO)/2)) {
  LA_deaths_SCO[[i]] <- .CleanSco(input_year = definitions$query_years_long[i],
                    input_age_name = LA_deaths_EW$age_name, input_sex = "Female")
}
for (i in ((length(LA_deaths_SCO)/2)+1) : length(LA_deaths_SCO)) {
  LA_deaths_SCO[[i]] <- .CleanSco(input_year = definitions$query_years_long[i],
                    input_age_name = LA_deaths_EW$age_name, input_sex = "Male")
}

LA_deaths_SCO <- data.table::rbindlist(LA_deaths_SCO)

# ---------- #

## Merge Deaths #

LA_deaths <- copy(rbind(LA_deaths_EW, LA_deaths_SCO))
rm(LA_deaths_EW, LA_deaths_SCO)

# ---------- #

## Make it Match Population Data Set Format ##
                
# ensure last age group is "Aged 85 and over" as it is in the population data
LA_deaths[age_name == "Aged 90 and over" | age_name == "Aged 85-89", 
       Dx_new := sum(Dx), by = c("date_name","geography_name","sex_name")]
LA_deaths <- copy(LA_deaths[age_name != "Aged 90 and over", ])
LA_deaths[age_name == "Aged 85-89", Dx := Dx_new][, Dx_new := NULL]         
LA_deaths[age_name == "Aged 85-89", age_name := "Aged 85 and over"]      

# define age code names
LA_deaths[, age_code := 99][
  age_name == "Aged under 1", age_code := 0][
  age_name == "Aged 1 to 4", age_code := 4][
  age_name == "Aged 5 to 9", age_code := 9][
  age_name == "Aged 10-14", age_code := 14][
  age_name == "Aged 15-19", age_code := 19][
  age_name == "Aged 20-24", age_code := 24][
  age_name == "Aged 25-29", age_code := 29][
  age_name == "Aged 30-34", age_code := 34][
  age_name == "Aged 35-39", age_code := 39][
  age_name == "Aged 40-44", age_code := 44][
  age_name == "Aged 45-49", age_code := 49][
  age_name == "Aged 50-54", age_code := 54][
  age_name == "Aged 55-59", age_code := 59][
  age_name == "Aged 60-64", age_code := 64][
  age_name == "Aged 65-69", age_code := 69][
  age_name == "Aged 70-74", age_code := 74][
  age_name == "Aged 75-79", age_code := 79][
  age_name == "Aged 80-84", age_code := 84][
  age_name == "Aged 85 and over", age_code := 85][, age_name := NULL]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Query Population Data ### 

# Query Population Data: For ALL of the UK (incl. SCO and Northern Ireland)
LA_population <- data.table::data.table(nomisr::nomis_get_data(id = "NM_31_1",
      geography = definitions$geo_type, sex = c(5,6), age = seq(1:20), 
      time = c(definitions$query_years[1]:definitions$query_years[2]), 
      measures = 20100, tidy = TRUE))

# introduce ctr_code & remove Northern Ireland
LA_population[, ctr_code := substr(geography_code, 1, 1)]
LA_population <- copy(LA_population[ctr_code != "N"])
LA_population <- copy(LA_population[age_name != "Aged 16 - 59/64"])

# reduce data 
LA_population <- copy(LA_population[, c("date_name", "ctr_code", "geography_name",
            "geography_code", "sex_name","age_name","obs_value"), with = FALSE])

# rename into Nx
setnames(LA_population, old = "obs_value", new = "Nx")

# define age_code names
LA_population[, age_code := 99][
  age_name == "Aged under 1 year", age_code := 0][
  age_name == "Aged 1 - 4 years", age_code := 4][
  age_name == "Aged 5 - 9 years", age_code := 9][
  age_name == "Aged 10 - 14 years", age_code := 14][
  age_name == "Aged 15 - 19 years", age_code := 19][
  age_name == "Aged 20 - 24 years", age_code := 24][
  age_name == "Aged 25 - 29 years", age_code := 29][
  age_name == "Aged 30 - 34 years", age_code := 34][
  age_name == "Aged 35 - 39 years", age_code := 39][
  age_name == "Aged 40 - 44 years", age_code := 44][
  age_name == "Aged 45 - 49 years", age_code := 49][
  age_name == "Aged 50 - 54 years", age_code := 54][
  age_name == "Aged 55 - 59 years", age_code := 59][
  age_name == "Aged 60 - 64 years", age_code := 64][
  age_name == "Aged 65 - 69 years", age_code := 69][
  age_name == "Aged 70 - 74 years", age_code := 74][
  age_name == "Aged 75 - 79 years", age_code := 79][
  age_name == "Aged 80 - 84 years", age_code := 84][
  age_name == "Aged 85 and over", age_code := 85][, age_name := NULL]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Match Deaths and Population Estimates ###

# logical check: ensure unique geography names match
LA_names <- data.table(deaths = sort(unique(LA_deaths$geography_name)),
                    pop = sort(unique(LA_population$geography_name)))
LA_names[deaths == pop, check := TRUE]
table(LA_names$check)

# prepare merging: ensure identical keys before merging 
setkey(LA_population, geography_name, sex_name, date_name, age_code)
setkey(LA_deaths, geography_name, sex_name, date_name, age_code)

# merge deaths and population data set 
LA_population_deaths <- merge(LA_population, LA_deaths,
        by = c("date_name","ctr_code","geography_name","sex_name","age_code"))

# assert correct name for geography code 
setnames(LA_population_deaths, old = "geography_code.x", new = "geography_code")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### 3-Year Rolling Average Mortality Rates ###

# set correct key for creating subset IDs
setkey(LA_population_deaths, geography_name, sex_name, date_name, age_code)

# initialize an empty results list for 3-year rolling averages
LA_population_deaths_3y <- vector(mode = "list",
                     length = definitions$query_years[2] - 
                              definitions$query_years[1] - 1)

# apply 3-year rolling average 
for (i in 1:length(LA_population_deaths_3y)) {
  LA_population_deaths_3y[[i]] <- .Rolling3YearAverage(
              input_data = LA_population_deaths,
              year_lower = definitions$query_years_long[i],
              year_upper = definitions$query_years_long[i+2],
              definitions = definitions)
}

# make one long format table
LA_population_deaths_3y <- data.table::rbindlist(LA_population_deaths_3y)

# assign an ID for vectorized handling later on
LA_population_deaths_3y[, ID := .GRP, by = c("geography_name","sex_name","year")]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Exclude Geographies Previously Identified ###

if (definitions$excluding_geographies == TRUE) {
  # identify unique names 
  LA_excluded <- LA_population_deaths_3y[excluded_geography == 1, ]
  LA_excluded <- unique(LA_excluded$geography_name)
  # subset 
  LA_population_deaths_3y <- LA_population_deaths_3y[!(geography_name %in% LA_excluded), ] 
  # new ID after subset
  LA_population_deaths_3y[, ID := .GRP, by = c("geography_name","sex_name","year")]
} else {LA_excluded <- "none"}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Estimate RAW Life Table ###

# apply Chiang 1984 Method #
LA_life_tables_raw <- vector(mode = "list",
                             length = max(LA_population_deaths_3y$ID))
for (i in 1:max(LA_population_deaths_3y$ID)) {
  LA_life_tables_raw[[i]] <- .LTBChiang5y(LA_population_deaths_3y, input_ID = i)

}

# translate into one long format dt 
LA_life_tables_raw <- data.table::rbindlist(LA_life_tables_raw,
                                         fill = FALSE, idcol = NULL)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Query Standard Population Schedules ###

# Query Std Schedule: single-year log mortality rates from HMD website
# ! ensure you have registered ! 
std_pop_list <- list(
  .ReadStdPop(country = "GBR_SCO", item = "mltper_1x10", year = 2010),
  .ReadStdPop(country = "GBR_SCO", item = "fltper_1x10", year = 2010),
  .ReadStdPop(country = "GBRTENW", item = "mltper_1x10", year = 2010), 
  .ReadStdPop(country = "GBRTENW", item = "fltper_1x10", year = 2010)) 
  
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### From Estimated Life Tables: run TOPALS Smoothing and Kannisto Model ###

# create empty list
LA_life_tables_topals_kannisto <- vector(mode = "list",
                                       length = max(LA_life_tables_raw$ID))

# fill list and make data.table
for (i in 1:max(LA_life_tables_raw$ID)) {
  LA_life_tables_topals_kannisto[[i]] <- .RunTOPALSandKannisto(
    data_input = LA_life_tables_raw[ID == i, ], std_pops = std_pop_list)
}
LA_life_tables_topals_kannisto <- data.table::rbindlist(
  LA_life_tables_topals_kannisto, fill = FALSE, idcol = NULL)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### PLOT TOPALS Kannisto vs. Raw Comparison ###

LA_model_comparison_example_raw <- LA_life_tables_raw[
      geography_name == definitions$mort_example_LA & 
      year == definitions$xsection_year & 
      sex_name == definitions$mort_example_sex,
  .(geography_name, year, sex_name, age_code, mx, Nx, Dx)]
LA_model_comparison_example_raw[, age_code := as.numeric(age_code)]
LA_model_comparison_example_raw[, x_min := age_code - 4]
LA_model_comparison_example_raw[, x_max := age_code + 1]
LA_model_comparison_example_raw[age_code == 0, x_min := 0]
LA_model_comparison_example_raw[age_code == 0, x_max := 1]
LA_model_comparison_example_raw[age_code == 4, x_min := 1]
LA_model_comparison_example_raw[age_code == 4, x_max := 5]
LA_model_comparison_example_raw[age_code == 85, x_min := 85]
LA_model_comparison_example_raw[age_code == 85, x_max := 110]
LA_model_comparison_example_raw[, mx := log(mx)]

LA_model_comparison_example <- 
 LA_life_tables_topals_kannisto[geography_name == definitions$mort_example_LA & 
  year == definitions$xsection_year & sex_name == definitions$mort_example_sex,
    .(geography_name, year, sex_name, age_code,
      mx_topals, mx_topals_kannisto, mx_std)]

LA_model_comparison_example <- melt(LA_model_comparison_example,
    id.vars = c("geography_name", "year", "sex_name", "age_code"),
    measure.vars = c("mx_topals", "mx_topals_kannisto", "mx_std"))   

LA_model_comparison_example[, value := log(value)]

LA_plot_model_comparison_example <- ggplot(LA_model_comparison_example, 
     aes(x = age_code, y = value, colour = variable)) + 
  geom_point(size = definitions$point_size) + 
  geom_line(size = definitions$line_size) + 
  geom_segment(data = LA_model_comparison_example_raw,
               aes(x = x_min, xend = x_max, y = mx, yend = mx),
               inherit.aes = FALSE, size = 1.5) +
  labs(x = "Age", y = "log Mortality Rate", colour = "Modelling Approach", 
       caption =  paste0(definitions$mort_example_sex,"; ",
                       definitions$mort_example_LA,"; ",
                       definitions$xsection_year)) +
  geom_text(data = LA_model_comparison_example_raw,
            aes(x = (x_min+x_max)/2, y = mx + log(1.5), label = paste0("D=",Dx)),
            vjust = "inward", hjust = "inward",  size = 3,
            show.legend = FALSE,  inherit.aes = FALSE) +
  geom_text(aes(x = 0, y = -3.0,
                label = "Observed log m(x) and Observed D(x)"), size = 6,
            vjust = "inward", hjust = "inward",
            show.legend = FALSE,  inherit.aes = FALSE) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "bottom", legend.key.size = unit(1.25, "cm"), 
        legend.text = element_text(size = 12))
ggsave(plot = LA_plot_model_comparison_example,
       filename = "ROutput/plot_model_comparison_example.jpeg",
       width =  definitions$fig_width,
       height = definitions$fig_height, units = "cm")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Load and Merge with ONS LookUp Files for Geography ###

# unzip lookup file and store locally 
unzip(zipfile = "RData/ShapeLookup/Local_Authority_Districts_(May_2021)_UK_BFE.zip",
      exdir = "RData/ShapeLookup/")

# Load Look Up File
# Source: https://geoportal.statistics.gov.uk/search?q=LOcal%20Authority%20Region%202021
LA_LookUp_Geography <- data.table::data.table(read.csv(
  file = "RData/ShapeLookup/Local_Authority_District_to_Region_April_2021.csv"))

# LA Look Up
LA_LookUp_Geography <- copy(LA_LookUp_Geography[, .(LAD21CD, RGN21NM)])
LA_LookUp_Geography <- unique(LA_LookUp_Geography)

# LA - Topals + Kannisto # 
LA_life_tables_topals_kannisto <- merge(LA_life_tables_topals_kannisto,
                                        LA_LookUp_Geography,
              by.x = "geography_code", by.y = "LAD21CD", all.x = TRUE)
LA_life_tables_topals_kannisto[ctr_code == "W", RGN21NM := "Wales"]
LA_life_tables_topals_kannisto[ctr_code == "S", RGN21NM := "Scotland"]

# logical check included/ excluded geographies
if (definitions$excluding_geographies == TRUE) {
stopifnot(dim(LA_names)[1] - length(LA_excluded) == 
            length(unique(LA_life_tables_topals_kannisto$geography_code)))
  } else{stopifnot(dim(LA_names)[1] - length(LA_excluded) + 1 == 
              length(unique(LA_life_tables_topals_kannisto$geography_code)))}

# LA - Raw # 
LA_life_tables_raw <- merge(LA_life_tables_raw, LA_LookUp_Geography,
                            by.x = "geography_code", by.y = "LAD21CD", all.x = TRUE)
LA_life_tables_raw[ctr_code == "W", RGN21NM := "Wales"]
LA_life_tables_raw[ctr_code == "S", RGN21NM := "Scotland"]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
