library(rgdal)
library(dplyr)
library(readr)
library(leaflet)
library(raster)
select = dplyr::select

source('functions.R')

r_depth = raster('data/physiographic/EC_Depth_10km_mean_10km.img')
iplot(r_depth)

# get depth extent for running InVEST wind energy model
r1 = mask(!is.na(r_depth), r_depth)
p1 = rasterToPolygons(r1, dissolve=T)
plot(p1, col='blue')
p1_gcs = spTransform(p1, CRS('+proj=longlat +datum=WGS84 +no_defs'))
shapefile(p1_gcs, 'data/depth_extent_gcs.shp')
shapefile(p1, 'data/depth_extent_aea.shp')

# windspeed: read polygons, rasterize like depth, transform
v_windspeed90m = readOGR(
  'data/wind/Atlantic_Coast_90m_Windspeed_Offshore_Wind_High_Resolution',
  'atlantic_coast_90mwindspeed_off')
v_windspeed90m %>%
  spTransform(CRS(proj4string(r_depth))) %>%
  rasterize(r_depth, 'Speed_90') %>%
  writeRaster('data/wind/EC_Windspeed90m_10km_aea.grd')

# output to points for consuming into InVEST
v_windspeed90m %>%
  rasterize(
    #raster(extent(v_windspeed90m), crs=crs(v_windspeed90m), resolution=c(1, 1)), # test case
    raster(extent(v_windspeed90m), crs=crs(v_windspeed90m), resolution=c(0.01, 0.01)), 
    'Speed_90') %>%
  rasterToPoints(fun=function(x){!is.na(x)}) %>%
  as.data.frame() %>%
  select(lon=x, lat=y, speed_90=layer) %>%
  write_csv('data/wind/EC_Windspeed90m_10km_0.1dd_gcs.csv')

# stack and project to Mercator for leaflet
stk_aea = stack(
  raster('data/physiographic/EC_Depth_10km_mean_10km.img'),
  raster('data/physiographic/EC_Shore_Dist_10kdm_mean_10km.img'),
  raster('data/physiographic/EC_Slope_10km_mean_10km.img'),
  raster('data/wind/EC_Windspeed90m_10km_aea.grd'))
names(stk_aea) = c('depth','dist2shore','slope','windspeed90m')

# write stack
writeRaster(stk_aea, 'data/industry_wind_aea.grd')

# project to Mercator for Leaflet
writeRaster(leaflet::projectRasterForLeaflet(stk_aea), 'data/industry_wind_mer.grd')