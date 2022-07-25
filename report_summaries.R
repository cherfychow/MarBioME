

# MarBioME Task 2
# Report preparation summary stats and figures

require(ggplot2)
require(dplyr)
require(readxl)
require(stringr)
require(tidyr)
require(sf)
require(patchwork)
require(viridisLite)

# load BioTIME palettes
source('https://gist.githubusercontent.com/cherfychow/bfd614b05afdaa4cfd887d80dde1c3e5/raw/ab814cf8080f6e381ec09bf90777c2693a73679a/BioTIMEpal')
programs <- read_excel('Task2_MonitoringProgrammes.xlsx', sheet=1, na = 'NA')[1:647,]

# make an object of pretty taxa names
taxa <- colnames(programs)[14:25]
taxa[4] <- 'Benthic invertebrates'
taxa[9] <- 'Hard corals'

# Summary bars ------------------------------------------------------------

# Country split code snippet
country_n <- programs %>% 
  pull(Country) %>% na.omit %>% 
  str_split(., '\\s?\\,\\s?|\\s?\\+\\s?', simplify=F) %>% # split comma separated countries
  unlist %>% str_remove_all(., '\\)|\\(|Regional\\s?') %>% # collapse into one vector, get rid of strings that say regional
  data.frame(Country = .) %>% as_tibble %>% # turn the country list into a tibble column
  filter(!Country == 'Regional', !str_detect(Country, 'Slovak')) %>% # can't visualise programs that only say they're regional
  group_by(Country) %>% summarise(ProgramTotal = n()) # calculate program totals per country and make a column

ggplot(data = country_n %>% arrange(desc(ProgramTotal)) %>% top_n(10) %>% mutate(Country = str_replace(string = Country, 'United Kingdom', 'UK'))) +
  geom_col(aes(x = Country, y = ProgramTotal, fill = Country), color = 'grey20') +
  scale_fill_biotime(palette = 'gradient', discrete = T) +
  guides(fill = 'none') +
  theme_classic(base_size=13) +
  labs(x=NULL, y='Number of monitoring programmes')

# tally of programs with country coverage
regional_n <- programs %>% filter(!is.na(Country)) %>% 
  mutate(countryn = str_count(Country, '\\,') + 1) %>% 
  group_by(countryn) %>% summarise(programN = n())
# which ones with 20+ countries
programs[which(str_count(programs$Country, '\\,') > 10),] %>% View

# how many programmes with NA in Country because they're regional
programs %>% filter(is.na(Country) & GeographicalCoverage == 'Regional') %>% nrow()
regional_n[8,1] <- 'Regional'

# tally of taxa covered
summary(programs[14:25])
programs[which(is.na(programs[14:25])),] %>% View
taxa_n <- colSums(programs[14:25]) %>% as.data.frame
names(taxa_n) <- 'Count'
taxa_n$Taxa <- rownames(taxa_n)
taxa_n[4,2] <- 'Benthic invertebrates'
taxa_n[9,2] <- 'Hard corals'

ggplot(data = taxa_n %>% filter(!Taxa == 'Mangrove')) +
  geom_col(aes(y= Taxa, x = Count, fill = Taxa), color = 'grey20') +
  scale_fill_biotime(palette = 'gradient', discrete = T) +
  guides(fill = 'none') +
  theme_classic(base_size = 13) +
  labs(y = 'Monitored taxa', x='Number of monitoring programmes')

# Duration of programs by taxa

programs$End2 <- programs$EndYear
programs$End2[which(programs$Ongoing == 'Yes')] <- 2022
programs$Duration <- with(programs, End2 - StartYear + 1) # year inclusive

# taxa split
taxa_dur <- programs %>% filter(get(colnames(programs[14])) == 1, !is.na(Duration)) %>%
  select(Duration) %>% mutate(Taxa = colnames(programs[14]))
for (i in 1:10) {
  taxa_dur <- programs %>% filter(get(colnames(programs[i + 14])) == 1, !is.na(Duration)) %>%
    select(Duration) %>% mutate(Taxa = colnames(programs[i + 14])) %>% 
    bind_rows(taxa_dur, .)
}

names(taxa) <- colnames(programs)[14:25]
taxa_dur$Taxa <- str_replace_all(taxa_dur$Taxa, taxa)

library(ggdist)
library(gghalves)
biotime_palettes[[4]] <- paste0(biotime_palettes[[4]], 'AA')
ggplot(data = taxa_dur, aes(y = Duration, x = Taxa, fill = Taxa), color = 'grey20') +
  stat_slab(adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.2, slab_color = 'grey20', slab_size = 0.25) + 
  geom_half_point(alpha = 0.5, shape = 21, side = 'l', size = 1,
                  ## control range of jitter
                  range_scale = .3) +
  geom_boxplot(width = .1, outlier.shape = NA) +
  scale_fill_biotime(palette = 'gradient', discrete = T, labels = taxa) +
  theme_classic(base_size = 11) + theme(legend.position = 'none') +
  scale_y_continuous(breaks = c(1, 10, 25, 50, 75, 100)) +
  labs(y = 'Duration (years)')

# Static maps -------------------------------------------------------------

# adapted from Shiny code

# read in cleaned and prepped spatial geometries
load('./MarBioME_T2_MapApp/T2_spatial.RData')
# make a vector object of the DataIDs that have coordinates
# this will be our key to determine choro vs geometry viz
coordID <- c(as.vector(T2_coord_hex$DataID),
             as.vector(T2_coord_polygon$DataID),
             as.vector(T2_coord_pt$DataID))
programs$StartYear <- as.integer(programs$StartYear)
programs$EndYear <- as.integer(programs$EndYear)

rm(T2_coord_mp) # multipoint spatial objects are getting replaced with hexagonal gridded objects
programs$Taxa <- '' # switch back to comma separated format for easier filtering
for (i in 14:25) {
  programs$Taxa[which(programs[i] == 1)] <- paste(programs$Taxa[which(programs[i] == 1)], taxa[i-12], sep=', ')
}
programs$Taxa <- str_remove_all(programs$Taxa, '^\\,\\s')

# link spatial geometries with programme metadata
T2_coord_pt$Latitude <- st_coordinates(T2_coord_pt)[,2]
T2_coord_pt$Longitude <- st_coordinates(T2_coord_pt)[,1]
T2_coord_hex <- left_join(T2_coord_hex, programs, by="DataID")
T2_coord_pt <- left_join(T2_coord_pt, programs, by="DataID")
T2_coord_polygon <- left_join(T2_coord_polygon, programs, by="DataID")

# CRS check, make sure they're all the same
proj <- '+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs' # projection code
T2_coord_hex <- st_transform(T2_coord_hex, crs = proj)
T2_coord_pt <- st_transform(T2_coord_pt, crs = proj)
T2_coord_polygon <- st_transform(T2_coord_polygon, crs = proj)

## Global map parameters
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_make_valid() %>% st_transform(., proj) # load in the world outlines

# limits bounding box
bbox <- st_sfc(st_point(c(-20, 30)), st_point(c(75, 70)), crs=4326) %>% st_transform(., proj)
# X11(type = "cairo")

## Choropleth maps --------------------------------------------------------------

# set up data
# make a tibble for studies where coordinate info isn't available for choropleth mapping
T2_choro <- programs %>% filter(!DataID %in% coordID)

load('./MarBioME_T2_MapApp/EEZ.RData')
EEZ <- st_transform(EEZ, crs = proj)
EEZ$TERRITORY1[which(EEZ$TERRITORY1 == 'Faeroe')] <- 'Faroe Islands'


### Choropleth map, time --------------------------------------------------------------

# parse multiple country for choropleth
choro <- na.omit(T2_choro$Country) %>% 
  str_split(., '\\,', simplify=F) %>% # split comma separated countries
  unlist %>% # collapse into one vector
  tibble(Country = .) %>% # turn the country list into a tibble column
  filter(!Country == 'Regional', !str_detect(Country, 'Slovak')) %>% # can't visualise programs that only say they're regional
  group_by(Country) %>% summarise(ProgramTotal = n())

choro_all <- ggplot() +
  geom_sf(data=world, color = 'grey80', fill='white', size = 0.25) +
  geom_sf(data = choro %>% 
            left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
            filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'),
          aes(fill = ProgramTotal), color = 'grey40', size = 0.25) +
  coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
  scale_fill_viridis_c(option = 'mako', direction = -1, na.value = '#EEEEEE', name = 'Total monitoring programmes') +
  theme_light() +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'bottom')

choro_all # print

# by decade (before 1970, 1970-1980, etc.)

decs <- seq(1970, 2020, by=10) # decades to plot in loop, value = end of decade
# we'll make the plots for before 1970 and after 2010 manually

choro_decs <- as.list(rep('', length(decs) + 1)) # empty list to populate
choro_dec_data <- as.list(rep('', length(decs) + 1)) # for data

# start decade plots data loop
for (i in 1:length(decs)) {
  
  choro_dec_data[[i]] <- T2_choro %>% 
    filter(StartYear < decs[i], Ongoing == 'Yes' | EndYear > decs[i]) %>% # filter to only show programmes active in this decade
    pull(Country) %>% na.omit %>% 
    str_split(., '\\,', simplify=F) %>% # split comma separated countries
    unlist %>% # collapse into one vector
    tibble(Country = .) %>% # turn the country list into a tibble column
    filter(!Country == 'Regional', !str_detect(Country, 'Slovak')) %>% # can't visualise programs that only say they're regional
    group_by(Country) %>% summarise(ProgramTotal = n())
}

# plot loop

for (i in 1:length(decs)) { 
  choro_decs[[i]] <- ggplot() +
    geom_sf(data= world, color = 'grey80', fill='white', size = 0.25) +
    geom_sf(data = choro_dec_data[[i]] %>% 
              left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
              filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'), 
            aes(fill = ProgramTotal), size = 0.1, color = 'grey50') +
    coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
    scale_fill_viridis_c(option = 'mako', direction = -1, na.value = 'transparent', 
                         name = 'Total monitoring programmes', limits = c(1,40)) +
    theme_light() + labs(title = paste0(decs[i]-10, '-', decs[i])) +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'none')
  }

# change 1970 title
choro_decs[[1]] <- choro_decs[[1]] +
  labs(title = 'Before 1970')

# only ongoing
choro_dec_data[[length(decs) + 1]] <- T2_choro %>% 
  filter(Ongoing == 'Yes') %>% # filter to only show ongoing programmes
  pull(Country) %>% na.omit %>% 
  str_split(., '\\,', simplify=F) %>% # split comma separated countries
  unlist %>% # collapse into one vector
  tibble(Country = .) %>% # turn the country list into a tibble column
  filter(!Country == 'Regional', !str_detect(Country, 'Slovak')) %>% # can't visualise programs that only say they're regional
  group_by(Country) %>% summarise(ProgramTotal = n())

choro_decs[[length(decs) + 1]] <- ggplot() +
  geom_sf(data= world, color = 'grey80', fill='white', size = 0.25) +
  geom_sf(data = choro_dec_data[[length(decs) + 1]] %>% 
            left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
            filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'), 
          aes(fill = ProgramTotal), size = 0.1, color = 'grey50') +
  coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
  scale_fill_viridis_c(option = 'mako', direction = -1, na.value = 'transparent', 
                       name = 'Total monitoring programmes', limits = c(1,40)) +
  theme_light() + labs(title = 'Ongoing programmes') +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'bottom')

# (choro_decs[[1]] + choro_decs[[2]] + choro_decs[[3]]) / (choro_decs[[4]] + choro_decs[[5]] + choro_decs[[6]]) # print decade grid
# choro_decs[[7]] # print ongoing

# print together
((choro_decs[[1]] + choro_decs[[2]] + choro_decs[[3]]) / (choro_decs[[4]] + choro_decs[[5]] + choro_decs[[6]])) | choro_decs[[7]]

# rm(choro_decs, choro_dec_data, decs, choro_all)

## Choropleth maps, taxa --------------------------------------------------------------

# 12 taxa columns, but 11 because no mangroves
# make an object of pretty taxa names
taxa <- colnames(programs)[14:25]
taxa[4] <- 'Benthic invertebrates'
taxa[9] <- 'Hard corals'

choro_taxa <- as.list(rep('', 11)) # empty list to populate with plots
choro_taxa_data <- as.list(rep('', 11)) # for data

# start taxa disaggregated data loop
for (i in 1:11) {

  choro_taxa_data[[i]] <- T2_choro %>% 
    filter(get(colnames(T2_choro)[i + 13]) == 1) %>% # filter to only show programmes active in this decade
    pull(Country) %>% na.omit %>% 
    str_split(., '\\,', simplify=F) %>% # split comma separated countries
    unlist %>% # collapse into one vector
    tibble(Country = .) %>% # turn the country list into a tibble column
    filter(!Country == 'Regional', !str_detect(Country, 'Slovak')) %>% # can't visualise programs that only say they're regional
    group_by(Country) %>% summarise(ProgramTotal = n())
  
}

# plot loop
for (i in 1:11) {
  
  choro_taxa[[i]] <- ggplot() +
    geom_sf(data= world, color = 'grey80', fill='white', size = 0.25) +
    geom_sf(data = choro_taxa_data[[i]] %>% 
              left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
              filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'), 
            aes(fill = ProgramTotal), size = 0.1, color = 'grey50') +
    coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
    scale_fill_viridis_c(option = 'mako', direction = -1, na.value = 'transparent', 
                         name = 'Total monitoring programmes', limits=c(1,25)) +
    theme_light() + labs(title = taxa[i]) +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'bottom')
  
}

# shortcut
# paste(paste0('choro_taxa[[', 1:11, ']]'), collapse=" + ")

choro_taxa[[1]] + choro_taxa[[2]] + choro_taxa[[3]] + choro_taxa[[4]] + choro_taxa[[5]] + choro_taxa[[6]] + choro_taxa[[7]] + choro_taxa[[8]] + choro_taxa[[9]] + choro_taxa[[10]] + choro_taxa[[11]] + plot_layout(ncol = 3, guides = 'collect') & theme(legend.position = 'right') # print


## Sub-national level maps -------------------------------------------------

# choro and all spatial data types
ggplot() +
  geom_sf(data=world, color = 'grey80', fill='white', size = 0.25) +
  geom_sf(data = choro %>% 
            left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
            filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'),
          aes(fill = ProgramTotal), color = 'grey40', size = 0.25) +
  geom_sf(data = T2_coord_polygon, color = 'grey20', fill = '#a3c5ff', size = 0.5, alpha = 0.8) +
  geom_sf(data = T2_coord_hex, color = 'grey20', fill = '#a3c5ff', size = 0.5, alpha = 0.8) +
  geom_sf(data = T2_coord_pt, color = 'grey20', fill = '#a3c5ff', size = 2, alpha = 0.8, shape = 21) +
  coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
  scale_fill_viridis_c(option = 'mako', direction = -1, na.value = '#EEEEEE', name = 'Total monitoring programmes') +
  theme_light() +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'bottom')


### Decade maps -------------------------------------------------------------

# reuse the decs filter used above
all_decs <- as.list(rep('', length(decs))) # empty list to fill with plot objects

for (i in 1:length(decs)) {
  
  all_decs[[i]] <- ggplot() +
    geom_sf(data=world, color = 'grey80', fill='white', size = 0.2) +
    geom_sf(data = choro_dec_data[[i]] %>% 
              left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
              filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'),
            aes(fill = ProgramTotal), color = 'grey40', size = 0.25) +
    geom_sf(data = T2_coord_polygon %>% filter(StartYear < decs[i], Ongoing == 'Yes' | EndYear > decs[i]), 
            color = 'grey20', fill = '#a3c5ff', size = 0.5, alpha = 0.8) +
    geom_sf(data = T2_coord_hex %>% filter(StartYear < decs[i], Ongoing == 'Yes' | EndYear > decs[i]), 
            color = 'grey20', fill = '#a3c5ff', size = 0.5, alpha = 0.8) +
    geom_sf(data = T2_coord_pt %>% filter(StartYear < decs[i], Ongoing == 'Yes' | EndYear > decs[i]), 
            color = 'grey20', fill = '#a3c5ff', size = 2, alpha = 0.8, shape = 21) +
    coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
    scale_fill_viridis_c(option = 'mako', direction = -1, na.value = '#EEEEEE', 
                         limits = c(1,40), name = 'Total monitoring programmes') +
    theme_light() + labs(title = paste0(decs[i]-10, '-', decs[i])) +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'none')
  
}

all_decs[[length(decs) + 1]] <- ggplot() +
  geom_sf(data=world, color = 'grey80', fill='white', size = 0.2) +
  geom_sf(data = choro_dec_data[[length(decs) + 1]] %>% 
            left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
            filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'),
          aes(fill = ProgramTotal), color = 'grey40', size = 0.25) +
  geom_sf(data = T2_coord_polygon %>% filter(Ongoing == 'Yes'), 
          color = 'grey20', fill = '#a3c5ff', size = 0.5, alpha = 0.8) +
  geom_sf(data = T2_coord_hex %>% filter(Ongoing == 'Yes'), 
          color = 'grey20', fill = '#a3c5ff', size = 0.5, alpha = 0.8) +
  geom_sf(data = T2_coord_pt %>% filter(Ongoing == 'Yes'), 
          color = 'grey20', fill = '#a3c5ff', size = 2, alpha = 0.8, shape = 21) +
  coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
  scale_fill_viridis_c(option = 'mako', direction = -1, na.value = '#EEEEEE', 
                       limits = c(1,40), name = 'Total monitoring programmes') +
  theme_light() + labs(title = 'Ongoing programmes') +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'bottom')

all_decs[[1]] <- all_decs[[1]] + labs(title = 'Before 1970')

# shortcut
# paste(paste0('all_decs[[', 1:6, ']]'), collapse=" + ")

(all_decs[[1]] + all_decs[[2]] + all_decs[[3]] + all_decs[[4]] + all_decs[[5]] + all_decs[[6]]) | all_decs[[7]]


### All spatial data, taxa --------------------------------------------------

all_taxa <- as.list(rep('', 11))

for (i in 1:11) {
  
  all_taxa[[i]] <- ggplot() +
    geom_sf(data=world, color = 'grey80', fill='white', size = 0.2) +
    geom_sf(data = choro_taxa_data[[i]] %>% 
              left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
              filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'),
            aes(fill = ProgramTotal), color = 'grey40', size = 0.25) +
    geom_sf(data = T2_coord_polygon %>% filter(get(colnames(programs)[i + 13]) == 1), 
            color = 'grey20', fill = '#a3c5ff', size = 0.5, alpha = 0.8) +
    geom_sf(data = T2_coord_hex %>% filter(get(colnames(programs)[i + 13]) == 1), 
            color = 'grey20', fill = '#a3c5ff', size = 0.5, alpha = 0.8) +
    geom_sf(data = T2_coord_pt %>% filter(get(colnames(programs)[i + 13]) == 1), 
            color = 'grey20', fill = '#a3c5ff', size = 2, alpha = 0.8, shape = 21) +
    coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
    scale_fill_viridis_c(option = 'mako', direction = -1, na.value = '#EEEEEE', 
                         limits = c(1,25), name = 'Total monitoring programmes') +
    theme_light() + labs(title = taxa[i]) +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = 'right')
  
}

# shortcut
# paste(paste0('all_taxa[[', 1:11, ']]'), collapse=" + ")

all_taxa[[1]] + all_taxa[[2]] + all_taxa[[3]] + all_taxa[[4]] + all_taxa[[5]] + all_taxa[[6]] + all_taxa[[7]] + all_taxa[[8]] + all_taxa[[9]] + all_taxa[[10]] + all_taxa[[11]] + guide_area() + plot_layout(guides = 'collect', ncol = 3)

# Version that highlights gaps by reversing colour scale

all_taxa_gap <- as.list(rep('', 11))

for (i in 1:11) {
  
  all_taxa_gap[[i]] <- ggplot() +
    geom_sf(data=world, color = 'grey80', fill='white', size = 0.2) +
    geom_sf(data = choro_taxa_data[[i]] %>% 
              left_join(x=EEZ, y=., by=c('TERRITORY1' = 'Country')) %>% st_as_sf %>% # merge tallies with EEZ spatial data
              filter(!st_geometry_type(geometry) == 'GEOMETRYCOLLECTION'),
            aes(fill = ProgramTotal), color = 'grey40', size = 0.25) +
    geom_sf(data = T2_coord_polygon %>% filter(get(colnames(programs)[i + 13]) == 1), 
            color = 'grey20', fill = '#EEEEEE', size = 0.5, alpha = 0.8) +
    geom_sf(data = T2_coord_hex %>% filter(get(colnames(programs)[i + 13]) == 1), 
            color = 'grey20', fill = '#EEEEEE', size = 0.5, alpha = 0.8) +
    geom_sf(data = T2_coord_pt %>% filter(get(colnames(programs)[i + 13]) == 1), 
            color = 'grey20', fill = '#EEEEEE', size = 2, alpha = 0.8, shape = 21) +
    coord_sf(crs=proj, xlim = st_bbox(bbox)[c(1,3)], ylim = st_bbox(bbox)[c(2,4)]) +
    scale_fill_viridis_c(option = 'mako', direction = 1, na.value = '#444444', 
                         limits = c(1,25), name = 'Total monitoring\nprogrammes') +
    theme_light() + labs(title = taxa[i]) +
    theme(axis.ticks = element_blank(), axis.text = element_blank())
  
}

# shortcut
# paste(paste0('all_taxa_gap[[', 1:11, ']]'), collapse=" + ")

all_taxa_gap[[1]] + all_taxa_gap[[2]] + all_taxa_gap[[3]] + all_taxa_gap[[4]] + all_taxa_gap[[5]] + all_taxa_gap[[6]] + all_taxa_gap[[7]] + all_taxa_gap[[8]] + all_taxa_gap[[9]] + all_taxa_gap[[10]] + all_taxa_gap[[11]] + guide_area() + plot_layout(guides = 'collect', ncol = 3)
