### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2022-08-22
# About: this file maps sf12 to eq5d and estimates utility scores 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Work Off Understanding Society ###

# ! in order to be able to run this stage, you need access to the free 
# general license version of understanding society. the files "X_indresp.dta",
# wave e to j need to be in the respective folder

# read data 
data_US <- data.table::data.table(
  readstata13::read.dta13(paste0("RData/UnderstandingSociety/",
  definitions$US_wave,"_indresp.dta")))
setnames(data_US, old = paste0(definitions$US_wave,"_ivfio"), new = "ivfio")
setnames(data_US, old = paste0(definitions$US_wave,"_sex"),   new = "sex")
setnames(data_US, old = paste0(definitions$US_wave,"_dvage"),   new = "dvage")
setnames(data_US, old = paste0(definitions$US_wave,"_gor_dv"), new = "gor_dv")
setnames(data_US, old = paste0(definitions$US_wave,"_sf12pcs_dv"), new = "sf12pcs_dv")
setnames(data_US, old = paste0(definitions$US_wave,"_sf12mcs_dv"), new = "sf12mcs_dv")
setnames(data_US, old = paste0(definitions$US_wave,"_indscub_xw"), new = "indscub_xw")

# keep only what we need
data_US <- data_US[, c("pidp","ivfio","sex","dvage","gor_dv",
                       "sf12pcs_dv","sf12mcs_dv","indscub_xw"), with = FALSE]

# ensure variable type is character
data_US[, ivfio  := as.character(ivfio)]
data_US[, sex    := as.character(sex)]
data_US[, gor_dv := as.character(gor_dv)]

# Inclusion/Exclusion Criteria --> original variable type #
data_US <- data_US[dvage >= 0, ]
data_US <- data_US[sex == "male" | sex == "female", ]
data_US <- data_US[gor_dv != "missing" & !is.na(gor_dv), ]
data_US <- data_US[sf12pcs_dv >= 0, ]
data_US <- data_US[sf12mcs_dv >= 0, ]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Re-Coding Survey Variables ###

# age break & age code 
data_US[, age_name := as.factor(cut(dvage,
  breaks = c(0, 19, 24, 29, 35, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
  labels = c("under_20", "20-24", "25-29", "30-34", "35-39", "40-44","45-49",
             "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
             "85_plus")))]

# define age code names
data_US[, age_code := 99][
  age_name == "under_20", age_code := 20][
  age_name == "20-24", age_code := 24][
  age_name == "25-29", age_code := 29][
  age_name == "30-34", age_code := 34][
  age_name == "35-39", age_code := 39][
  age_name == "40-44", age_code := 44][
  age_name == "45-49", age_code := 49][
  age_name == "50-54", age_code := 54][
  age_name == "55-59", age_code := 59][
  age_name == "60-64", age_code := 64][
  age_name == "65-69", age_code := 69][
  age_name == "70-74", age_code := 74][
  age_name == "75-79", age_code := 79][
  age_name == "80-84", age_code := 84][
  age_name == "85_plus", age_code := 85]

# introduce N 
data_US[, N := 1]

# gor_dv -> geography_name
data_US[, geography_name := gor_dv]

# sex  -> sex_name
data_US[, sex_name := sex]
data_US[sex_name == "female", sex_name := "Female"]
data_US[sex_name == "male",   sex_name := "Male"]

# "Yorkshire and the Humber"
data_US[geography_name == "Yorkshire and the Humber",
        geography_name := "Yorkshire and The Humber"]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Weight and Aggregate ###

# different survey weights are available, we use "indscub_xw"

# setkey 
setkey(data_US, geography_name, sex_name, age_code)

# apply weighting 
data_US[, wt_sum := sum(indscub_xw),
         by = c("geography_name", "sex_name", "age_name")]
data_US[, wt_new := (1/wt_sum) * indscub_xw]
data_US[, sf12pcs_dv_wt := sf12pcs_dv *  wt_new]
data_US[, sf12mcs_dv_wt := sf12mcs_dv *  wt_new]

# aggregate
data_US_utils <- copy(data_US[, .(year = definitions$xsection_year,
                                N = sum(N),
                            source   = "US",
                            mean_pcs    = mean(sf12pcs_dv),
                            mean_mcs    = mean(sf12mcs_dv),
                            mean_pcs_wt = sum(sf12pcs_dv_wt),
                            mean_mcs_wt = sum(sf12mcs_dv_wt)),
              by = c("geography_name","sex_name", "age_code", "age_name")])

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Mapping ###

# we use the predictive models proposes in Lawrence and Fleishmann (2004)
# "Predicting EuroQoL EQ-5D Preference Scores from the SF-12 Health Survey ..."
# we use their 6 variable model, others models are also available

# model with 6 variables - with survey weights 
data_US_utils[, eq5d_util_lf_6v_wt := - 1.6984 + (mean_pcs_wt * 0.07927) + 
                (mean_mcs_wt * 0.02859) + 
              ((mean_pcs_wt * mean_mcs_wt) * -0.000126) + 
              ((mean_pcs_wt * mean_pcs_wt) * -0.00141) +
              ((mean_mcs_wt * mean_mcs_wt) * -0.00014) +
              ((mean_pcs_wt * mean_pcs_wt * mean_pcs_wt) * 0.0000107)]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Save Utility Scores in Data Folder # 
data.table::fwrite(x = data_US_utils,
                   file = "RData/UnderstandingSociety/data_US_utils.csv",
                   row.names = FALSE, quote = TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
