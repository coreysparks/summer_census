library(sf)
lrs <- st_read("C:/Users/ozd504/OneDrive - University of Texas at San Antonio/projects/census_summer/data/PDBresources20220511120308/pdb2021_tract_US/pdb2021_tract_US.shp")

library(dplyr)
tx_lrs <- lrs%>%
  filter(STATEFP==48)%>%
  st_transform(crs = 2163)

st_write(tx_lrs, "./data/tx_lrs.gpkg", delete_dsn = T)
