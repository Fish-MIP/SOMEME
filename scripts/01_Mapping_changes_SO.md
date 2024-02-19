Mapping changes to exploitable fish biomass in the Southern Ocean
================
Denisse Fierro Arcos
2/19/24

- <a href="#changes-in-total-consumer-biomass-in-the-southern-ocean"
  id="toc-changes-in-total-consumer-biomass-in-the-southern-ocean">Changes
  in total consumer biomass in the Southern Ocean</a>
  - <a href="#loading-libraries" id="toc-loading-libraries">Loading
    libraries</a>
  - <a href="#setting-up-notebook" id="toc-setting-up-notebook">Setting up
    notebook</a>
  - <a href="#loading-data" id="toc-loading-data">Loading data</a>
  - <a
    href="#calculating-percentage-change-in-fish-biomass-estimates-from-global-fishmip-models"
    id="toc-calculating-percentage-change-in-fish-biomass-estimates-from-global-fishmip-models">Calculating
    percentage change in fish biomass estimates from global FishMIP
    models</a>
    - <a href="#calculating-ensemble-mean-and-standard-deviation"
      id="toc-calculating-ensemble-mean-and-standard-deviation">Calculating
      ensemble mean and standard deviation</a>
  - <a href="#world-base-map" id="toc-world-base-map">World base map</a>
  - <a href="#plotting-exploitable-fish-biomass"
    id="toc-plotting-exploitable-fish-biomass">Plotting exploitable fish
    biomass</a>
    - <a href="#ccamlr-plots" id="toc-ccamlr-plots">CCAMLR plots</a>
    - <a href="#measo-plots" id="toc-measo-plots">MEASO plots</a>

# Changes in total consumer biomass in the Southern Ocean

In this notebook, we will use all FishMIP global models to calculate the
mean ensemble percentage change in total exploitable fish biomass in the
Southern Ocean for the decade ending in 2050. The reference period for
this calculation is 2005-2014. We define “exploitable fish biomass” as
biomass for all marine organisms with weights between 10g and 100Kg.

## Loading libraries

``` r
#Data wrangling
library(tidyverse)
library(data.table)
#Dealing with raster data
library(terra)
library(tidyterra)
#Dealing with vector data
library(sf)
#Base maps
library(rnaturalearth)
library(measoshapes)
#Color palettes
library(cmocean)
#Combining plots
library(cowplot)
```

## Setting up notebook

We will define the folders where inputs are kept, and where outputs
should be saved.

``` r
#Base folder for project
base_folder <- "/rd/gem/private/users/camillan"

#Defining location of notebook outputs
out_folder <- "../outputs"
if(!dir.exists(out_folder)){
  dir.create(out_folder)
}
```

## Loading data

We will load the total consumer biomass data to calculate percentage
change, and masks to extract data for the MEASO and CCAMLR regions only.

``` r
#MEASO
measo_mask <- read_csv("../outputs/measo_regions_1deg.csv")
```

    Rows: 15679 Columns: 4
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    dbl (4): x, y, id, area_m

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
measo_keys <- read_csv("../outputs/measo_regions_keys.csv")
```

    Rows: 18 Columns: 2
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (1): name
    dbl (1): id

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#CCAMLR
ccamlr_mask <- read_csv("../outputs/ccamlr_stats_area_1deg.csv")
```

    Rows: 7444 Columns: 4
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (1): GAR_Short_
    dbl (3): x, y, area_m

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ccamlr_keys <- read_csv("../outputs/ccamlr_stats_area_keys.csv")
```

    Rows: 20 Columns: 2
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (2): GAR_Name, GAR_Short_

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Joining MEASO and CCAMLR masks into a single data frame
measo_ccamlr <- full_join(measo_mask, ccamlr_mask, 
                          by = join_by(x, y, area_m)) |> 
  #Rename ID columns
  rename("measo"="id", "ccamlr"="GAR_Short_")

#Getting a list of files containing biomass data
global_files <- list.files(base_folder, pattern = "global_10g-100kg.rds", 
                           full.names = T, recursive = T)

#Getting a list of models
members <- str_extract(global_files, "annual_(.*)_(h|s)", group = 1) |> 
  unique()
```

## Calculating percentage change in fish biomass estimates from global FishMIP models

We will go through each FishMIP model and calculate the mean fish
biomass for the decade between 2005 and 2014 (last decade of
`historical` period), and for the period between 2041 and 2050 (for the
two emission scenarios). Finally, we will calculate the percentage
change between these two decades.

``` r
#Looping through each FishMIP model
for(m in members){
  #Load all data available for a single FishMIP model
  df_model <- str_subset(global_files, m) |> 
    map(~readRDS(.)) |> 
    map_df(~bind_rows(.)) |> 
    #Extract values for Southern Ocean only
    filter(y < -35) |> 
    #Extract data only for years to be used in maps
    filter(year >= 2005 & year <= 2014 | year >= 2041 & year <= 2050) |> 
    #Create new group column to calculate means
    mutate(group = case_when(year <= 2014 ~ "reference",
                             year >= 2041 & year <= 2050 ~ "mean50"),
           #The new group column also need to have the scenario as part of the label
           group = case_when(group != "reference" ~ str_c(group, scenario, sep = "_"),
                             T ~ group)) |> 
    #Calculate mean per ensemble member
    group_by(x, y, mem, esm, group) |> 
    summarise(mean_bio = mean(biomass, na.rm = T)) |> 
    #Reorganise table to facilitate calculations
    pivot_wider(names_from = group, values_from = mean_bio) |> 
    ungroup() |> 
    #Calculate % change in fish biomass for the two emissions scenarios
    mutate(rel_change_mean50_ssp126 = ((mean50_ssp126-reference)/reference)*100,
           rel_change_mean50_ssp585 = ((mean50_ssp585-reference)/reference)*100)
   
  #Create name to save file  
  f_out <- file.path(out_folder, str_c(m, "_perc_bio_change_data_map.csv"))
  
  #Saving results for each model
  df_model |> 
    fwrite(f_out)
}
```

### Calculating ensemble mean and standard deviation

We will now load the percentage change in biomass for all global models
and calculate an ensemble mean and standard deviation.

``` r
#Listing all relevant files to calculate biomass projections
maps_data <- list.files(out_folder, pattern = "_perc_bio_change_data_map.csv", 
                        full.names = T) |> 
  map_df(~fread(.)) |> 
  #Calculations performed by year and EEZ
  group_by(x, y) |> 
  #Apply calculations to biases only
  summarise(across(rel_change_mean50_ssp126:rel_change_mean50_ssp585, 
                   #Listing statistics to be calculated
                   list(mean = mean, sd = sd), 
                   #Setting column names
                   .names = "{.col}_{.fn}")) |> 
  ungroup() |> 
  #Apply mask
  left_join(measo_ccamlr, by = join_by(x, y))
```

    `summarise()` has grouped output by 'x'. You can override using the `.groups`
    argument.

## World base map

``` r
#South Polar Stereographic projection
sp_proj <- "+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"

#Base map of the world
world_proj <- ne_countries(returnclass = "sf") |> 
  st_transform(sp_proj)
```

## Plotting exploitable fish biomass

We will use the masks to create plots using the MEASO and CCAMLR
boundaries, but first we will define a colourmap to be used in all maps,
and the basic plotting instructions.

``` r
#Create custom-made color palette
scale_fill_custom <- function(..., alpha = 1, begin = 0, end = 1, direction = 1, 
  option = "D", values = NULL, space = "Lab", na.value = "white", 
  guide = "colourbar", aesthetics = "fill") {
  continuous_scale(aesthetics, scale_name = "custom", 
    palette = scales:::gradient_n_pal(c(cmocean("matter", start = 0.1, 
                                                end = 0.8, direction = -1)(123),
                                        cmocean("delta", start = 0.49, 
                                                end = 0.5)(20),
                                        cmocean("deep", start = 0.1, 
                                                end = 0.8)(123)), values, space), 
    na.value = na.value, guide = guide, ...)
}
```

### CCAMLR plots

``` r
#Loading CCAMLR boundaries
ccamlr <- read_sf("../../SO_shapefiles/CCAMLR_Convention_Area/statistical_areasPolygon.shp") |> 
  st_set_crs(6932)
```

    Warning in CPL_read_ogr(dsn, layer, query, as.character(options), quiet, : GDAL
    Error 1: unhandled axis direction: "North along 90 deg East"

``` r
#Apply projection to 
maps_data_proj <- maps_data |> 
  #Selecting areas within CCAMLR boundaries
  drop_na(ccamlr) |> 
  #Selecting coordinate columns and percentage change columns
  select(x, y, starts_with("rel_change")) |> 
  #Transform to multi-layer raster
  rast(type = "xyz", crs = "epsg:4326") |> 
  #Reproject to Robinson
  project(y = sp_proj) 

#Define base steps for maps
base_gg_perc <- list(scale_fill_binned(limits = c(-50, 50), n.breaks = 8,
                                  type = scale_fill_custom,
                                  oob = scales::oob_squish),
                geom_sf(inherit.aes = F, data = ccamlr, fill = NA, 
                        colour = "#5b5b5b", show.legend = F, linewidth = 0.25),
                geom_sf(inherit.aes = F, data = world_proj, show.legend = F),
                lims(y = c(-4990292, 4990292), x = c(-4591822, 4590292)),
                theme_bw(),
                theme(axis.title = element_blank(), 
                      panel.border = element_rect(colour = NA),
                      plot.title = element_text(hjust = 0.5),
                      legend.position = "none"))
```

We can now plot mean percentage change in fish biomass for the FishMIP
ensemble within the CCAMLR boundaries.

``` r
#Mean percentage change
#SSP1-2.6 2041-2050
p50_126 <- ggplot()+
  geom_spatraster(data = maps_data_proj$rel_change_mean50_ssp126_mean)+
  labs(title = "SSP1-2.6: 2041-2050")+
  base_gg_perc
```

    SpatRaster resampled to ncells = 501264

``` r
#SSP5-8.5 2041-2050
p50_585 <- ggplot()+
  geom_spatraster(data = maps_data_proj$rel_change_mean50_ssp585_mean)+
  labs(title = "SSP5-8.5: 2041-2050")+
  base_gg_perc+
  guides(fill = guide_colorbar(title = "Mean percentage change",
                               title.position = "top", title.hjust = 0.5, 
                               barwidth = 15))+
  theme(legend.position = "bottom")
```

    SpatRaster resampled to ncells = 501264

``` r
#Get legend
leg <- get_legend(p50_585)

#Remove legend
p50_585 <- p50_585+
  theme(legend.position = "none")

#Create title for plot
title <- ggdraw()+
  draw_label("Mean % change in fish biomass from 2005-2014")+
  theme(plot.margin = margin(0, 0, 0, 0, unit = "cm"))

#Plotting everything together
all_plots <- plot_grid(title, plot_grid(p50_126, p50_585, ncol = 2, nrow = 1,
                                        labels = c("a", "b"), label_x = 0.1),
                       leg, ncol = 1, nrow = 3, 
                       rel_heights = c(0.15, 1, 0.4))

#Check final map
all_plots
```

![](01_Mapping_changes_SO_files/figure-commonmark/unnamed-chunk-9-1.png)

We can save the plot as a pdf.

``` r
# Saving multi-panel plot
ggsave(file.path(out_folder, "so_perc_change_map_50s_ccamlr.pdf"), 
       device = "pdf", width = 9, height = 6)
```

We can plot the standard deviation for the percentage change across all
FishMIP models.

``` r
#Define base steps for maps
base_gg_sd <- list(scale_fill_distiller(palette = "Blues", direction = 1, 
                                     na.value = NA, limits = c(0, 40), 
                                     oob = scales::oob_squish),
               geom_sf(inherit.aes = F, data = ccamlr, fill = NA, 
                        colour = "#5b5b5b", show.legend = F, linewidth = 0.25),
                geom_sf(inherit.aes = F, data = world_proj, show.legend = F),
                lims(y = c(-4990292, 4990292), x = c(-4591822, 4590292)),
                theme_bw(),
                theme(axis.title = element_blank(), 
                      panel.border = element_rect(colour = NA),
                      plot.title = element_text(hjust = 0.5),
                      legend.position = "none"))
```

``` r
#Standard deviation
#SSP1-2.6 2041-2050
p50_126 <- ggplot()+
  geom_spatraster(data = maps_data_proj$rel_change_mean50_ssp126_sd)+
  labs(title = "SSP1-2.6: 2041-2050")+
  base_gg_sd
```

    SpatRaster resampled to ncells = 501264

``` r
#SSP5-8.5 2041-2050
p50_585 <- ggplot()+
  geom_spatraster(data = maps_data_proj$rel_change_mean50_ssp585_sd)+
  labs(title = "SSP5-8.5: 2041-2050")+
  base_gg_sd+
  guides(fill = guide_colorbar(title = "Standard deviation",
                               title.position = "top", title.hjust = 0.5, 
                               barwidth = 15))+
  theme(legend.position = "bottom")
```

    SpatRaster resampled to ncells = 501264

``` r
#Get legend
leg <- get_legend(p50_585)

#Remove legend
p50_585 <- p50_585+
  theme(legend.position = "none")

#Create title for plot
title <- ggdraw()+
  draw_label("Ensemble variability in % change fish biomass from 2005-2014")+
  theme(plot.margin = margin(0, 0, 0, 0, unit = "cm"))

#Plotting everything together
all_plots <- plot_grid(title, plot_grid(p50_126, p50_585, ncol = 2, nrow = 1,
                                        labels = c("a", "b"), label_x = 0.1),
                       leg, ncol = 1, nrow = 3, 
                       rel_heights = c(0.15, 1, 0.4))

#Check final map
all_plots
```

![](01_Mapping_changes_SO_files/figure-commonmark/unnamed-chunk-12-1.png)

We can save the plot as a pdf.

``` r
# Saving multi-panel plot
ggsave(file.path(out_folder, "so_sd_map_50s_ccamlr.pdf"), 
       device = "pdf", width = 9, height = 6)
```

### MEASO plots

``` r
#Loading CCAMLR boundaries
measo <- measo_regions05_coastline |> 
  st_transform(6932)
```

    old-style crs object detected; please recreate object with a recent sf::st_crs()

``` r
#Apply projection to 
maps_data_proj <- maps_data |> 
  #Selecting areas within CCAMLR boundaries
  drop_na(measo) |> 
  #Selecting coordinate columns and percentage change columns
  select(x, y, starts_with("rel_change")) |> 
  #Transform to multi-layer raster
  rast(type = "xyz", crs = "epsg:4326") |> 
  #Reproject to Robinson
  project(y = sp_proj) 

#Define base steps for maps
base_gg_perc <- list(scale_fill_binned(limits = c(-50, 50), n.breaks = 8,
                                  type = scale_fill_custom,
                                  oob = scales::oob_squish),
                geom_sf(inherit.aes = F, data = measo, fill = NA, 
                        colour = "#5b5b5b", show.legend = F, linewidth = 0.25),
                geom_sf(inherit.aes = F, data = world_proj, show.legend = F),
                lims(y = c(-5593136, 5593136), x = c(-5598164, 5598164)),
                theme_bw(),
                theme(axis.title = element_blank(), 
                      panel.border = element_rect(colour = NA),
                      plot.title = element_text(hjust = 0.5),
                      legend.position = "none"))
```

We can now plot mean percentage change in fish biomass for the FishMIP
ensemble within the MEASO boundaries.

``` r
#Mean percentage change
#SSP1-2.6 2041-2050
p50_126 <- ggplot()+
  geom_spatraster(data = maps_data_proj$rel_change_mean50_ssp126_mean)+
  labs(title = "SSP1-2.6: 2041-2050")+
  base_gg_perc
```

    SpatRaster resampled to ncells = 501264

``` r
#SSP5-8.5 2041-2050
p50_585 <- ggplot()+
  geom_spatraster(data = maps_data_proj$rel_change_mean50_ssp585_mean)+
  labs(title = "SSP5-8.5: 2041-2050")+
  base_gg_perc+
  guides(fill = guide_colorbar(title = "Mean percentage change",
                               title.position = "top", title.hjust = 0.5, 
                               barwidth = 15))+
  theme(legend.position = "bottom")
```

    SpatRaster resampled to ncells = 501264

``` r
#Get legend
leg <- get_legend(p50_585)

#Remove legend
p50_585 <- p50_585+
  theme(legend.position = "none")

#Create title for plot
title <- ggdraw()+
  draw_label("Mean % change in fish biomass from 2005-2014")+
  theme(plot.margin = margin(0, 0, 0, 0, unit = "cm"))

#Plotting everything together
all_plots <- plot_grid(title, plot_grid(p50_126, p50_585, ncol = 2, nrow = 1,
                                        labels = c("a", "b"), label_x = 0.1),
                       leg, ncol = 1, nrow = 3, 
                       rel_heights = c(0.15, 1, 0.4))

#Check final map
all_plots
```

![](01_Mapping_changes_SO_files/figure-commonmark/unnamed-chunk-15-1.png)

We can save the plot as a pdf.

``` r
# Saving multi-panel plot
ggsave(file.path(out_folder, "so_perc_change_map_50s_measo.pdf"), 
       device = "pdf", width = 9, height = 6)
```

We can plot the standard deviation for the percentage change across all
FishMIP models.

``` r
#Define base steps for maps
base_gg_sd <- list(scale_fill_distiller(palette = "Blues", direction = 1, 
                                     na.value = NA, limits = c(0, 40), 
                                     oob = scales::oob_squish),
               geom_sf(inherit.aes = F, data = measo, fill = NA, 
                        colour = "#5b5b5b", show.legend = F, linewidth = 0.25),
                geom_sf(inherit.aes = F, data = world_proj, show.legend = F),
                lims(y = c(-5593136, 5593136), x = c(-5598164, 5598164)),
                theme_bw(),
                theme(axis.title = element_blank(), 
                      panel.border = element_rect(colour = NA),
                      plot.title = element_text(hjust = 0.5),
                      legend.position = "none"))
```

``` r
#Standard deviation
#SSP1-2.6 2041-2050
p50_126 <- ggplot()+
  geom_spatraster(data = maps_data_proj$rel_change_mean50_ssp126_sd)+
  labs(title = "SSP1-2.6: 2041-2050")+
  base_gg_sd
```

    SpatRaster resampled to ncells = 501264

``` r
#SSP5-8.5 2041-2050
p50_585 <- ggplot()+
  geom_spatraster(data = maps_data_proj$rel_change_mean50_ssp585_sd)+
  labs(title = "SSP5-8.5: 2041-2050")+
  base_gg_sd+
  guides(fill = guide_colorbar(title = "Standard deviation",
                               title.position = "top", title.hjust = 0.5, 
                               barwidth = 15))+
  theme(legend.position = "bottom")
```

    SpatRaster resampled to ncells = 501264

``` r
#Get legend
leg <- get_legend(p50_585)

#Remove legend
p50_585 <- p50_585+
  theme(legend.position = "none")

#Create title for plot
title <- ggdraw()+
  draw_label("Ensemble variability in % change fish biomass from 2005-2014")+
  theme(plot.margin = margin(0, 0, 0, 0, unit = "cm"))

#Plotting everything together
all_plots <- plot_grid(title, plot_grid(p50_126, p50_585, ncol = 2, nrow = 1,
                                        labels = c("a", "b"), label_x = 0.1),
                       leg, ncol = 1, nrow = 3, 
                       rel_heights = c(0.15, 1, 0.4))

#Check final map
all_plots
```

![](01_Mapping_changes_SO_files/figure-commonmark/unnamed-chunk-18-1.png)

We can save the plot as a pdf.

``` r
# Saving multi-panel plot
ggsave(file.path(out_folder, "so_sd_map_50s_measo.pdf"), 
       device = "pdf", width = 9, height = 6)
```
