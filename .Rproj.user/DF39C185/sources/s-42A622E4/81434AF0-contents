### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2022-08-30
# About: this file combines life tables and utility scores to estimate QALE 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Load Data ### 

# Load Mapped Utility Scores 
data_US_utils <- data.table::fread(
  file = "RData/UnderstandingSociety/data_US_utils.csv")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Plot EQ-5D Utils ###

# position of label 
data_US_utils[sex_name == "Male", pos_text := 0.25]
data_US_utils[sex_name == "Female", pos_text := 0.50]

data_US_utils <- copy(data_US_utils[, 
                              .(geography_name, year, sex_name,
                                age_code, N, pos_text, eq5d_util_lf_6v_wt)])
# plot 6v weighted utils
US_mapped_utils <- ggplot(data_US_utils,
  aes(x = age_code, y = eq5d_util_lf_6v_wt, colour = as.factor(sex_name))) + 
  labs(y = "EQ-5D Utility Score", x = "Age Group", colour = "",
       caption =  paste0("Understanding Society SF12 to EQ-5D Mapping; Wave: ",
                         definitions$US_wave)) +
  geom_point(size = definitions$point_size) + 
  geom_line(size = definitions$line_size) + 
  facet_wrap(~ geography_name, ncol = 3) + 
  ylim(0.5, 1) + 
  theme_classic() 
ggsave(plot = US_mapped_utils,
       filename = "ROutput/US_mapped_utils.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Logical Checks for Life Tables ###

# Local Authority Level #
stopifnot(dim(LA_life_tables_topals_kannisto[is.na(RGN21NM), ])[[1]] == 0)
stopifnot(dim(LA_life_tables_topals_kannisto[is.na(geography_name), ])[[1]] == 0)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Cross-Sectional Analysis: Subset Year ###

LA_life_tables_topals_kannisto  <- 
  copy(LA_life_tables_topals_kannisto[year == definitions$xsection_year, ])

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Ensure Correct Age Format and Merge ###

data_US_utils[, age_code_survey := age_code][, age_code := NULL]

LA_life_tables_topals_kannisto  <- 
  .MappingAgesSurveyNOMIS(LA_life_tables_topals_kannisto)

LA_life_tables_topals_kannisto <- merge(x = LA_life_tables_topals_kannisto,
                                        y = data_US_utils,
            by.x = c("year", "RGN21NM", "sex_name", "age_code_survey"),
            by.y = c("year", "geography_name", "sex_name", "age_code_survey"),
            all.x = TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Estimate QALE via Sullivan Method ### 

# LA QALE # 
LA_life_tables_topals_kannisto[, p_Lx := eq5d_util_lf_6v_wt * Lx]
LA_life_tables_topals_kannisto[, p_Tx := rev(cumsum(rev(p_Lx))), by = "ID"]
LA_life_tables_topals_kannisto[, QALE := p_Tx / lx]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Write Final Results in Output Folder ###

# LE and QALE Results 
data.table::fwrite(x = LA_life_tables_topals_kannisto[age_code == 0, 
      .(year, ctr_code, RGN21NM, geography_code,geography_name,
      sex_name, age_code, ex, e_dagger, h, QALE)], 
                   file = "ROutput/data_area_metrics.csv",
                   row.names = FALSE, quote = TRUE)

# Save Table with all LA Names
data.table::fwrite(x = LA_names, file = "ROutput/data_names.csv",
                   row.names = FALSE, quote = TRUE)

# Save Table with excluded LA Names
data.table::fwrite(x = data.table::data.table(LA_excluded = LA_excluded),
        file = "ROutput/data_excluded.csv", row.names = FALSE, quote = TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
