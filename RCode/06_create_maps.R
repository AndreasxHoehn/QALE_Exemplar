### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2022-08-22
# About: this file makes nice maps 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Merge with Geography Shape File ### 

# source: https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2021-uk-bfe/about
LA_shapefile <- readOGR("RData/ShapeLookup/LAD_MAY_2021_UK_BFE_V2.shp")
LA_shapefile <- fortify(LA_shapefile, region = "LAD21CD")

# merge results with geography shapefile
QALE_LE_mapdata <- data.table::data.table(merge(
  LA_shapefile,
  LA_life_tables_topals_kannisto[age_code == 0,
                  .(year, ctr_code, RGN21NM, geography_code,
                    geography_name, sex_name, LE = ex, QALE)],
  by.x = "id", by.y = "geography_code",
  all.x = FALSE))
QALE_LE_mapdata  <- setkey(QALE_LE_mapdata , order)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Visualize Association between LE and QALE ###

LA_life_tables_topals_kannisto[ctr_code == "E", ctr_code := "England"]
LA_life_tables_topals_kannisto[ctr_code == "S", ctr_code := "Scotland"]
LA_life_tables_topals_kannisto[ctr_code == "W", ctr_code := "Wales"]

plot_LA_LE_QALE_association <- ggplot(LA_life_tables_topals_kannisto[age_code == 0],
    aes(x = ex, y = QALE, colour = as.factor(ctr_code))) + 
  geom_point() + 
  labs(y = "QALE", x = "LE",
       colour = "Country",
       caption = paste0("Reflecting Mortality of: ",definitions$xsection_year,"\n",
                        "Reflecting Health of Understanding Society Wave: ",
                        definitions$US_wave,"\n")) + 
  geom_text(aes(x = ex, y = QALE,label = geography_name),
            vjust = "inward", hjust = "inward",  size = 3,
            show.legend = FALSE,  inherit.aes = TRUE) +
  facet_wrap(~ sex_name) + 
  theme_minimal() 
ggsave(plot = plot_LA_LE_QALE_association,
       filename = "ROutput/plot_LE_QALE_association.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### GENERATE MAPS: LA - LE ###

map_LE <- ggplot(data = QALE_LE_mapdata,
  aes(x = long, y = lat, fill = LE, group = group)) + 
  geom_polygon() + 
  coord_equal() + 
  theme_void() +
  labs(fill = "LE at Birth", 
       caption = paste0("Reflecting Mortality of: ",definitions$xsection_year,"\n",
                       "Reflecting Health of Understanding Society Wave: ",
                      definitions$US_wave,"\n")) + 
  scale_fill_gradientn(colours = definitions$map_cols,
                       values = scales::rescale(c(
                         round(min(QALE_LE_mapdata$LE),0),
                         round(median(QALE_LE_mapdata$LE),0),
                         round(max(QALE_LE_mapdata$LE),0)))) +
  facet_wrap(~ sex_name, ncol = 2) 
ggsave(plot = map_LE,
       filename = "ROutput/map_LE.jpeg", units = "cm",
       width =  definitions$fig_width, height = definitions$fig_height)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### GENERATE MAPS: LA - QALE ###

map_QALE <- ggplot(data = QALE_LE_mapdata, 
                   aes(x = long, y = lat, fill = QALE, group = group)) + 
  geom_polygon() + 
  coord_equal() + 
  theme_void() +
  labs(fill = "QALE at Birth", 
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
