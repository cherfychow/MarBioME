

# MarBioME Task 2 Spatial Prep
# Spatial data cleaning for visualisation of marine biodiversity monitoring efforts


# Data clean ---------------------------------------------------------------

# we need to prep a separate table of spatial data that links to the monitoring programs IDs
# some changes to the Task 2 spreadsheet were done manually so, that requires reimporting

require(readxl)
require(stringr)
require(dplyr)

source('alignment.R') # source the EuroSea monitoring programme
rm(replace, task3, eurosea, europabon_t, europabon) # just eurosea cleaned
# 
# # remove blanks
# eurosea_space <- eurosea_t %>% filter(!is.na(Latitude), !Latitude == '', !str_detect(Latitude, '^\\s?\\-\\s?$')) %>% select(DataID, Latitude, Longitude)
# nrow(eurosea_t) - nrow(eurosea_space) # removed rows

# too messy, needs manual cleaning
# write.csv(eurosea_space, 'task2_spatial_raw.csv', row.names=F)

## Some code snippets to clean polygon coordinate strings
# string <- '058;29.422', 058;09.816', 058;57.347', 058;45.804', 058;08.465''
# string <- str_replace_all(string, '\\;|\\'', ' ') # degree minute symbols remove
# str_split(string, '\\s*\\,\\s*', simplify=T) %>% clipr::write_clip(.)
# 
# lat <- str_extract_all(string, '([:digit:]+\\s?){3}(?=N)', simplify=T) %>% as.vector
# clipr::write_clip(lat)
# long <- str_extract_all(string, '([:graph:]+\\s?){3}(?=E)', simplify=T) %>% as.vector
# clipr::write_clip(long)
#paste0('ES_', rep(277:289, each=2)) %>% clipr::write_clip


# Data prep for sf --------------------------------------------------------

# Reimport
T2_coord <- read.csv('task2_spatial.csv', header=T)
degs <- T2_coord %>% filter(str_detect(Latitude, '\\s'), str_detect(Longitude, '\\s'))
T2_coord <- T2_coord %>% filter(!str_detect(Latitude, '\\s'), !str_detect(Longitude, '\\s'))
# isolate out the rows that need degree min seconds conversion

# decimal degrees conversion function
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

degs$Latitude <- angle2dec(degs$Latitude)
degs$Longitude <- angle2dec(degs$Longitude)
degs$Latitude <- as.numeric(degs$Latitude)
degs$Longitude <- as.numeric(degs$Longitude)
# might return errors if some coords were degree minutes only
View(degs)

T2_coord$Latitude <- as.numeric(T2_coord$Latitude)
T2_coord$Longitude <- as.numeric(T2_coord$Longitude)

T2_coord <- bind_rows(T2_coord, degs) %>% arrange(DataID) # remerge
View(T2_coord)
str(T2_coord) # checks
summary(T2_coord)
T2_coord[which(T2_coord$Latitude == min(T2_coord$Latitude)),] # something funky about a latitude of 4..

# check and fix the anomaly
eurosea_t %>% filter(DataID == 'ES_41' | DataID == 'ES_42')
# run these code snippets to visually check that it was just an ignored digit in 34
# string <- "Amathus station: 34°41'33.90'N Larnaca station:  34°50'1.44'N Cape Greco:  34°58'11.58'N Latsi:  35°4'7.97'N"
# string <- str_replace_all(string, "[:symbol:]|\\'", ' ') # degree minute symbols remove
# str_split(string, '\\s*\\,\\s*', simplify=T)
T2_coord[which(T2_coord$Latitude < 5), 2] <- T2_coord[which(T2_coord$Latitude < 5), 2] + 30
rm(eurosea_space, degs, string)

# two studies also incorrectly have POINT when they should be multipoint
T2_coord[str_which(T2_coord$DataID, 'ES_212|ES_214'), 4] <- 'MULTIPOINT'
T2_coord[str_which(T2_coord$DataID, 'ES_212|ES_214'),]

# UNCOMMENT TO SAVE THESE FIXES
# save these fixes
# write.csv(T2_coord, 'task2_spatial.csv', row.names=F)

require(sf)

# in order to read each study's coordinates by the right geometry, we need them in a readable WKT string
# POINT (X Y)
# MULTIPOINT((X Y), (X Y))
# POLYGON ((X Y, X Y, X Y))

T2_coord <- T2_coord %>% group_by(DataID)
geom_types <- T2_coord %>% distinct(DataID, Geom)

# the string construction varies by geometry type, so for manipulation's sake we need them in separate tables
T2_coord_pt <- T2_coord %>% filter(Geom == 'POINT')
T2_coord_mp <- T2_coord %>% filter(Geom == 'MULTIPOINT')
T2_coord_pg <- T2_coord %>% filter(Geom == 'POLYGON')

# and now pasting them together for the WKT string formats
# POINT
T2_coord_pt <- T2_coord_pt %>% summarise(wkt = paste0('POINT (', Longitude, ' ', Latitude, ')'))
head(T2_coord_pt)
n_distinct(T2_coord_pt$DataID) == nrow(T2_coord_pt) # for this table, every row should be a unique programme

# POLYGON
# comma separated coords
T2_coord_pg <- T2_coord_pg %>% group_by(DataID) %>% summarise(wkt = paste(Longitude, Latitude, collapse=', ')) %>%  # collapse coordinates with comma separators
  mutate(wkt = paste0('POLYGON ((', wkt, '))'))
head(T2_coord_pg)

# MULTIPOINT
# each set of coords in their own parentheses, comma separated
T2_coord_mp$Longitude <- paste0('(', T2_coord_mp$Longitude) # because multipoint coords are in own parentheses, I'll just add them in like this first.
T2_coord_mp$Latitude <- paste0(T2_coord_mp$Latitude, ')')
T2_coord_mp <- T2_coord_mp %>% summarise(wkt = paste(Longitude, Latitude, collapse=', ')) %>% # collapse the coordinate sets with a comma separator
  mutate(wkt = paste0('MULTIPOINT (', wkt, ')')) # wrap the collapsed strings
head(T2_coord_mp)


## Shapefile specific handling -------------------------------------------------------------------------

# *NOTE: ES_134 to ES_148 require importing Finland's shapefiles
finland <- list.files(path='shapefiles/Finland', pattern = '.shp$', full.names = T)
# object with all the Finland shape file paths
finland_shp <- as.list(rep(0, length(finland))) # empty list to populate with the loop
for (i in 1:length(finland)) {
  finland_shp[[i]] <- st_read(finland[i])
  print(st_geometry_type(finland_shp[[i]]))
}
# they're all point features
# make each monitoring programme a single multipoint geometry
# also project to WGS84
for (i in 1:length(finland)) {
  finland_shp[[i]] <- st_union(finland_shp[[i]]) %>% st_transform(., crs=st_crs(4326))
}
finland_shp[[7]] <- finland_shp[[6]] # two phytoplankton monitoring programmes, but same sampling locations
# 7 and 8 do not match a monitoring programme, so overwrite
finland_shp[[8]] <- finland_shp[[9]]
finland_shp[[9]] <- NULL

finland # open this up to manually match the DataID to the geometry objects
finID = c('ES_136',
           'ES_140',
           'ES_135',
           'ES_141',
           'ES_139',
           'ES_134',
           'ES_145',
           'ES_144')

finland_shp <- st_sf(DataID = finID, geom = c(finland_shp[[1]], finland_shp[[2]], finland_shp[[3]], finland_shp[[4]],
                                            finland_shp[[5]], finland_shp[[6]], finland_shp[[7]], finland_shp[[8]]))
finland_shp

# plot check
library(ggplot2)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_make_valid() # already in wgs84
bbox <- st_sfc(st_point(c(-20, 30)), st_point(c(60, 75)), crs=4326)
# make a crop box object

ggplot() + geom_sf(data=world) + theme_minimal()
ggplot() +
  geom_sf(data=world, color='black', fill='transparent') +
  geom_sf(data=finland_shp, color='blue', shape=21, size=0.2) +
  coord_sf(crs = st_crs(4326), xlim = st_coordinates(bbox)[1:2], ylim = st_coordinates(bbox)[3:4]) +
  theme_minimal()
rm(finID, finland, i)

# merge Finland shapefile geometries with the other multipoints
T2_coord_mp <- st_as_sf(T2_coord_mp, wkt = 'wkt', dim = 'XY', crs = st_crs(4326))
names(T2_coord_mp)[2] <- 'geometry'
st_geometry(T2_coord_mp) <- 'geometry'

# plot check
ggplot() +
  geom_sf(data=world, color='black', fill='transparent') +
  geom_sf(data=T2_coord_mp, color='blue', shape=21, size=0.2) +
  coord_sf(crs = st_crs(4326), xlim = st_coordinates(bbox)[1:2], ylim = st_coordinates(bbox)[3:4]) +
  theme_minimal()

names(finland_shp)[2] <- 'geometry'
st_geometry(finland_shp) <- 'geometry'
T2_coord_mp
finland_shp
T2_coord_mp <- rbind(T2_coord_mp, finland_shp)
T2_coord_mp # visual check merge
# plot check
ggplot() +
  geom_sf(data=world, color='black', fill='transparent') +
  geom_sf(data=T2_coord_mp, aes(color = str_extract(DataID, '(?<=\\_)[:digit:]{1,3}$') %>% as.integer), shape=21, size=0.2) +
  coord_sf(crs = st_crs(4326), xlim = st_coordinates(bbox)[1:2], ylim = st_coordinates(bbox)[3:4]) +
  theme_minimal()
rm(finland_shp)

# make sf readable points table
T2_coord_pt <- st_as_sf(T2_coord_pt, wkt = 'wkt', dim = 'XY', crs = st_crs(4326))
names(T2_coord_pt)[2] <- 'geometry'
st_geometry(T2_coord_pt) <- 'geometry'

ggplot() +
  geom_sf(data=world, color='black', fill='transparent') +
  geom_sf(data=T2_coord_pt, color = 'blue', shape=21, size=0.2) +
  coord_sf(crs = st_crs(4326), xlim = st_coordinates(bbox)[1:2], ylim = st_coordinates(bbox)[3:4]) +
  theme_minimal()

# make sf readable polygon table
T2_coord_pg <- st_as_sf(T2_coord_pg, wkt = 'wkt', dim = 'XY', crs = st_crs(4326))
names(T2_coord_pg)[2] <- 'geometry'
st_geometry(T2_coord_pg) <- 'geometry'
# polygons have to have the same beginning and end points to close
# so we need this to make them valid and plottable
T2_coord_polygon <- T2_coord_pg %>% st_cast('MULTIPOINT') %>% group_by(DataID) %>% summarise(geometry = st_convex_hull(geometry))

ggplot() +
  geom_sf(data=world, color='black', fill='transparent') +
  geom_sf(data=T2_coord_polygon, color='blue', shape=21, size=0.2) +
  coord_sf(crs = st_crs(4326), xlim = c(-20,65), ylim = c(30,75)) +
  theme_minimal()

#write.csv(T2_coord, file = 'T2_coordinates_final.csv', row.names=F) # export the cleaned coordinates


# Gridding multipoint studies----------------------------------------------------------------

# because some monitoring programmes have thousands of points, this will make mapping very chaotic to visualise multipoint programmes
# Implement a hexgrid to simplify the programme coordinates
# just for T2_coord_mp

# define the projection we want the hex grids to be created within
# Europe Equidistant Conic
proj <- '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=43 +lat_2=62 +x_0=0 +y_0=0 +ellps=intl +units=m no_defs' # projection code

T2_coord_mp <- st_transform(T2_coord_mp, crs=proj)

# calculate a convex hull that covers all studies
hulls <- st_convex_hull(st_union(T2_coord_mp))

# Plot to check
world <- st_transform(world, crs = proj)
bbox <- st_transform(bbox, crs=proj)
ggplot() +
  geom_sf(data=world, color='black', fill='transparent') +
  geom_sf(data=hulls, fill='lightblue', color='blue', alpha=0.2) +
  geom_sf(data=T2_coord_mp, aes(color = DataID), shape=21, size=0.2) +
  coord_sf(crs = proj, xlim = st_coordinates(bbox)[1:2], ylim = st_coordinates(bbox)[3:4]) +
  theme_minimal()

# make a hexagon grid to overlay the area that covers all our studies
# remember units are in m! 
hex_grid <- st_make_grid(hulls, cellsize=50000, square=F, what='polygons')

ggplot() +
  geom_sf(data=world, color='grey', fill='transparent') +
  geom_sf(data=T2_coord_mp, color='blue', alpha=0.5) +
  geom_sf(data=hex_grid, color="orange", fill='transparent') +
  coord_sf(crs=proj, xlim = c(st_bbox(bbox)[1], 5009030), ylim = st_bbox(bbox)[c(2,4)])

# generate a vector that tells me which hexagon grid corresponds with each programme
study_hex <- st_intersects(T2_coord_mp, hex_grid)

# make an empty dataframe to populate with joined hex grids per programme
T2_coord_hex <- data.frame(DataID = T2_coord_mp$DataID, geometry='')

# for each study, create a joined polygon of all the hexcells it covers
for (i in 1:nrow(T2_coord_mp)) {
  T2_coord_hex$geometry[i] <- hex_grid[as.vector(study_hex[[i]])] %>% 
    st_union(., by_feature=F)
}

T2_coord_hex <- st_as_sf(T2_coord_hex)
st_crs(T2_coord_hex) <- proj # make sure sf knows the CRS for the polygons

# plot to check resulting hex grid groups
ggplot() +
  geom_sf(data=world, color='grey', fill='transparent') +
  geom_sf(data=T2_coord_hex, fill='blue', alpha=0.5) +
  geom_sf(data=T2_coord_polygon, fill = 'blue', alpha = 0.5) +
  geom_sf(data=T2_coord_pt, color = 'blue', alpha = 0.5) +
  coord_sf(crs=proj, xlim = c(st_bbox(bbox)[1], 5009030), ylim = st_bbox(bbox)[c(2,4)])

save(T2_coord_hex, T2_coord_mp, T2_coord_pt, T2_coord_polygon, file = 'MarBioME_T2_MapApp/T2_spatial.RData')
