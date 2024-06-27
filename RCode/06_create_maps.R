### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2022-08-30
# About: this file makes nice maps 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Merge with Geography Shape File ### 

# source: https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2021-uk-bfe/about
LA_shapefile <- rgdal::readOGR("RData/ShapeLookup/LAD_MAY_2021_UK_BFE_V2.shp")
LA_shapefile <- fortify(LA_shapefile, region = "LAD21CD")

# merge results with geography shapefile
QALE_LE_mapdata <- data.table::data.table(merge(
  LA_shapefile,
  LA_life_tables_topals_kannisto[age_code == 0,
                  .(year, ctr_code, RGN21NM, geography_code,
                    geography_name, sex_name, ex, e_dagger, h, QALE)],
  by.x = "id", by.y = "geography_code",
  all.x = FALSE))
QALE_LE_mapdata  <- setkey(QALE_LE_mapdata , order)

# re-coding country 
LA_life_tables_topals_kannisto[ctr_code == "E", ctr_code := "England"]
LA_life_tables_topals_kannisto[ctr_code == "S", ctr_code := "Scotland"]
LA_life_tables_topals_kannisto[ctr_code == "W", ctr_code := "Wales"]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Plot Association: LE and QALE ###

plot_LE_QALE_association <- ggplot(LA_life_tables_topals_kannisto[age_code == 0],
    aes(x = ex, y = QALE, colour = as.factor(ctr_code))) + 
  geom_point() + 
  labs(y = "QALE", x = "Life Expectancy",
       colour = "Country",
       caption = paste0("Reflecting Mortality of: ",definitions$xsection_year,"\n",
                        "Reflecting Health of Understanding Society Wave: ",
                        definitions$US_wave,"\n")) + 
  geom_text(aes(x = ex, y = QALE,label = geography_name),
            vjust = "inward", hjust = "inward",  size = 3,
            show.legend = FALSE,  inherit.aes = TRUE) +
  facet_wrap(~ sex_name) + 
  theme_minimal() 
ggsave(plot = plot_LE_QALE_association,
       filename = "ROutput/plot_LE_QALE_association.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Plot Association: Lifespan Variation and QALE ###

plot_LE_VAR_QALE_association <- ggplot(LA_life_tables_topals_kannisto[age_code == 0],
     aes(x = e_dagger, y = QALE, colour = as.factor(ctr_code))) + 
  geom_point() + 
  labs(y = "QALE", x = "Lifespan Variation (e_dagger)",
       colour = "Country",
       caption = paste0("Reflecting Mortality of: ",definitions$xsection_year,"\n",
                        "Reflecting Health of Understanding Society Wave: ",
                        definitions$US_wave,"\n")) + 
  geom_text(aes(x = e_dagger, y = QALE,label = geography_name),
            vjust = "inward", hjust = "inward",  size = 3,
            show.legend = FALSE,  inherit.aes = TRUE) +
  facet_wrap(~ sex_name) + 
  theme_minimal() 
ggsave(plot = plot_LE_VAR_QALE_association,
       filename = "ROutput/plot_LE_VAR_QALE_association.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Visualize Association between Life Expectancy and Lifespan Variation ###

plot_LE_LE_VAR_association <- ggplot(LA_life_tables_topals_kannisto[age_code == 0],
                              aes(x = ex, y = e_dagger, colour = as.factor(ctr_code))) + 
  geom_point() + 
  labs(y = "Lifespan Variation (e_dagger)", x = "Life Expectancy",
       colour = "Country",
       caption = paste0("Reflecting Mortality of: ",definitions$xsection_year,"\n")) + 
  geom_text(aes(x = ex, y = e_dagger,label = geography_name),
            vjust = "inward", hjust = "inward",  size = 3,
            show.legend = FALSE,  inherit.aes = TRUE) +
  facet_wrap(~ sex_name) + 
  theme_minimal() 
ggsave(plot = plot_LE_LE_VAR_association,
       filename = "ROutput/plot_LE_LE_VAR_association.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### GENERATE MAPS: Life Expectancy ###

map_LE <- ggplot(data = QALE_LE_mapdata,
  aes(x = long, y = lat, fill = ex, group = group)) + 
  geom_polygon() + 
  coord_equal() + 
  theme_void() +
  labs(fill = "Life Expectancy \n at Birth", 
       caption = paste0("Reflecting Mortality of: ",definitions$xsection_year,"\n",
                       "Reflecting Health of Understanding Society Wave: ",
                      definitions$US_wave,"\n")) + 
  scale_fill_gradientn(colours = definitions$map_cols,
                       values = scales::rescale(c(
                         round(min(QALE_LE_mapdata$ex),0),
                         round(median(QALE_LE_mapdata$ex),0),
                         round(max(QALE_LE_mapdata$ex),0)))) +
  facet_wrap(~ sex_name, ncol = 2) 
ggsave(plot = map_LE,
       filename = "ROutput/map_LE.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### GENERATE MAPS: Lifespan Variation ###

map_LE_VAR <- ggplot(data = QALE_LE_mapdata,
                 aes(x = long, y = lat, fill = e_dagger, group = group)) + 
  geom_polygon() + 
  coord_equal() + 
  theme_void() +
  labs(fill = "Lifespan Variation\n (e_dagger) at Birth", 
       caption = paste0("Reflecting Mortality of: ",definitions$xsection_year,"\n")) + 
  scale_fill_gradientn(colours = definitions$map_cols,
                       values = scales::rescale(c(
                         round(min(QALE_LE_mapdata$e_dagger),0),
                         round(median(QALE_LE_mapdata$e_dagger),0),
                         round(max(QALE_LE_mapdata$e_dagger),0)))) +
  facet_wrap(~ sex_name, ncol = 2) 
ggsave(plot = map_LE_VAR,
       filename = "ROutput/map_LE_VAR.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### GENERATE MAPS: QALE ###

map_QALE <- ggplot(data = QALE_LE_mapdata, 
                   aes(x = long, y = lat, fill = QALE, group = group)) + 
  geom_polygon() + 
  coord_equal() + 
  theme_void() +
  labs(fill = "QALE\n at Birth", 
       caption = paste0("Reflecting Mortality of: ",definitions$xsection_year,"\n",
                        "Reflecting Health of Understanding Society Wave: ",
                        definitions$US_wave,"\n")) + 
  scale_fill_gradientn(colours = definitions$map_cols,
                       values = scales::rescale(c(
                         round(min(QALE_LE_mapdata$QALE),0),
                         round(median(QALE_LE_mapdata$QALE),0),
                         round(max(QALE_LE_mapdata$QALE),0)))) +
  facet_wrap(~ sex_name, ncol = 2) 
ggsave(plot = map_QALE,
       filename = "ROutput/map_QALE.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
