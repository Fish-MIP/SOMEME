Regridding outputs from DBPM and ZOOMS-IPSL
================
Denisse Fierro Arcos
3/27/24

- <a href="#regridding-outputs-from-dbpm-and-zooms-ipsl"
  id="toc-regridding-outputs-from-dbpm-and-zooms-ipsl">Regridding outputs
  from DBPM and ZOOMS-IPSL</a>
  - <a href="#loading-libraries" id="toc-loading-libraries">Loading
    libraries</a>
  - <a href="#setting-up-notebook" id="toc-setting-up-notebook">Setting up
    notebook</a>
  - <a href="#defining-useful-functions"
    id="toc-defining-useful-functions">Defining useful functions</a>
  - <a href="#applying-functions-to-regrid-data"
    id="toc-applying-functions-to-regrid-data">Applying functions to regrid
    data</a>
  - <a href="#comparing-results" id="toc-comparing-results">Comparing
    results</a>

# Regridding outputs from DBPM and ZOOMS-IPSL

In this notebook, we will regrid outputs from DBPM and ZOOMS-IPSL to
match the $1^{\circ}$ grid used by all other FishMIP models.

## Loading libraries

``` r
#Data wrangling
library(tidyverse)
library(data.table)
#Dealing with raster data
library(terra)
```

## Setting up notebook

We will define the folders where inputs are kept, and where outputs
should be saved.

``` r
#Base folder for project
base_folder <- "/rd/gem/public/fishmip/SOMEME/"

#Loading target grid for regridding
target_grid <- rast("../outputs/measo_regions_1deg.nc")

#Identifying files to be regridded
reg_files <- list.files(base_folder, full.names = T, 
                        recursive = T) |> 
  str_subset("dbpm|zoomss_ipsl")
```

## Defining useful functions

We will define three functions:  
1. `csv_to_ras` loads a csv file from a path and transforms into a
raster  
2. `ras_to_df` takes a raster and transforms it to a data frame matching
the structure of original file  
3. `csv_to_reg_df` applies the two functions above, it will also regrid
a raster, and save it if a file path is provided.

``` r
csv_to_ras <- function(file_path){
  #Read the csv file
  df <- read_csv(file_path)
  #Extract metadata
  meta <- df |> 
    select(!c(time, lat, lon, tcb)) |> 
    drop_na()
  #Keep all other data
  ras <- df |> 
    #Remove rows with NA values
    drop_na(lat, lon) |>
    select(lon, lat, time, tcb) |> 
    #Rearrange prior to creating raster
    pivot_wider(names_from = time, values_from = tcb) |> 
    #Transform to raster
    rast(type = "xyz")
  #Return raster and metadata
  out <- list(raster = ras,
              metadata = meta)
  return(out)
}

ras_to_df <- function(raster, metadata){
  #Change raster to data frame
  reg_df <- raster |>
    #Keep coordinates
    as.data.frame(xy = T) |> 
    #Reorganise data to match original data
    pivot_longer(!c(x, y), names_to = "time", values_to = "tcb") |> 
    #Add metadata
    cbind(metadata) |> 
    #Matching original data
    relocate(c(long_name, units, time)) |> 
    rename("lon" = "x", "lat" = "y") |> 
    #Transforming time column to date
    mutate(time = as_date(time))
  
  #Return data frame
  return(reg_df)
}

#From csv to regridded data frame
csv_to_reg_df <- function(file_path, target_grid, method = "bilinear",
                          path_out = NULL){
  #csv to raster
  ras <- csv_to_ras(file_path)
  
  #regrid data
  reg <- resample(ras$raster, target_grid, method = method)
  
  #transfom regridded raster to data frame
  reg_df <- ras_to_df(reg, ras$metadata)
  
  #Saving file if path was provided
  if(!is.null(path_out)){
    reg_df |> 
      write_csv(path_out)
  }
  
  #Returned regridded data as data frame
  return(reg_df)
}
```

## Applying functions to regrid data

``` r
#Looping through each file
for(f in reg_files){
  #Creating new file path to save files
  f_out <- file.path(base_folder, paste0("regridded_", basename(f)))
  #Applying regridding function
  reg <- csv_to_reg_df(f, target_grid, path_out = f_out)
  }
```

    Rows: 1217330 Columns: 6
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (2): long_name, units
    dbl  (3): lon, lat, tcb
    date (1): time

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Comparing results

We will plot the regridded data first.

``` r
reg |> 
  filter(time == max(time)) |> 
  ggplot(aes(lon, lat, fill = tcb))+
  geom_tile()
```

![](02_Regridding_data_files/figure-commonmark/unnamed-chunk-6-1.png)

Now, we will load the original file and plot it below.

``` r
read_csv(f) |> 
  filter(time == max(time, na.rm = T)) |> 
  ggplot(aes(lon, lat, fill = tcb))+
  geom_tile()
```

    Rows: 1857601 Columns: 6
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (2): long_name, units
    dbl  (3): lat, lon, tcb
    date (1): time

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

![](02_Regridding_data_files/figure-commonmark/unnamed-chunk-7-1.png)
