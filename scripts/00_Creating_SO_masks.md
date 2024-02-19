Creating masks to extract data for the Southern Ocean
================
Denisse Fierro Arcos
2/19/20

- <a href="#creating-masks-to-extract-data-for-the-southern-ocean"
  id="toc-creating-masks-to-extract-data-for-the-southern-ocean">Creating
  masks to extract data for the Southern Ocean</a>
  - <a href="#loading-libraries" id="toc-loading-libraries">Loading
    libraries</a>
  - <a href="#loading-data" id="toc-loading-data">Loading data</a>

# Creating masks to extract data for the Southern Ocean

We will create two types of masks: a raster (gridded) mask that can be
used to extract data from `netcdf` files, and a data frame mask that can
be used to extract data from a data frame.

## Loading libraries

``` r
#Spatial data
library(sf)
library(terra)
library(measoshapes)

#Manipulating and plotting data
library(tidyverse)

#Base map
library(rnaturalearth)
```

## Loading data

We will load the following data to our session:  
\* Boundaries for the [Commission for the Conservation of Marine
Antarctic Living Resources (CCAMLR) Convention
Areas](https://www.ccamlr.org/en/organisation/convention-area), which
can be downloaded from the [CCAMLR GIS
portal](https://gis.ccamlr.org/)  
\* Marine Ecosystem Assessment for the Southern Ocean boundaries from
the [`measoshapes` R
package](https://github.com/AustralianAntarcticDivision/measoshapes)  
\* Sample of the grid used by the FishMIP models

``` r
ccamlr <- read_sf("../../SO_shapefiles/CCAMLR_Convention_Area/asd-shapefile-WGS84.shp")
measo <- measo_regions05_ll_coastline
```
