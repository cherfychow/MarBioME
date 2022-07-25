
# MarBioME Task 2
# Align EuropaBON Monitoring Database and EuroSea responses

require(dplyr)
require(stringr)

eurosea <- read.csv('EuroSea_final.csv')[-1,-38]
europabon <- read.csv('EuropaBON_BiodiversityData.csv', header=T)

eurosea_t <- eurosea %>% 
  select(ProgramName, SubprogramName, Organisation, RegionalCoord,
      PublicData, Country, Location, Website, Microbes:Mangrove, Time, Frequency, SOP_cite, Latitude, Longitude)
europabon_t <- europabon %>% filter(str_detect(target_realm, 'Marine|Coastal')) %>% 
  select(ProgramName=name, data_accessibility, 
         geographic_coverage:covered_nuts, data_url, target_species, 
         spatial_resolution, resolution, resolution_units, data_type, target_taxa, start_year, last_year, ongoing, temporal_resolution)

# Rename columns that match our final format without mutating
europabon_t <- europabon_t %>% rename(Location = geographic_coverage_detail, GeographicalCoverage = geographic_coverage, Country = covered_countries, URL = data_url, TargetedSpecies = target_species, SpatialRes = spatial_resolution, EBV = data_type, EOVTaxa = target_taxa, StartYear = start_year, EndYear = last_year, Ongoing = ongoing, TemporalRes = temporal_resolution)

eurosea_t <- eurosea_t %>% rename(DatabaseOrigin = PublicData, URL = Website, TemporalRes = Frequency)

europabon_t$Location <- with(europabon_t, paste(covered_nuts, Location, sep=','))
# if country is specified in covered NUTS but not Country column
europabon_t$Country[which(europabon_t$Country == '')] <- europabon_t$covered_nuts[which(europabon_t$Country == '')]
# if some columns were blank in Location or covered nuts, paste resulted in leading commas
# get rid
europabon_t$Location <- str_remove_all(europabon_t$Location, '^\\,[:blank:]?')

# also get rid of these columns
europabon_t <- europabon_t %>% select(!c(full_coverage, nuts_country, covered_nuts, data_accessibility))
europabon_t <- europabon_t %>% mutate(GrainSize = paste(resolution, resolution_units, sep=" ")) %>% select(!c(resolution, resolution_units))

# fit taxa to multiple choice EOV
europabon_t <- europabon_t %>% tibble::add_column(Microbes = 0, Phytoplankton = 0, 
                                          Zooplankton = 0, BenthicInv = 0, 
                                          Fish = 0, Turtles = 0, Birds = 0,
                                          Mammals = 0, HardC = 0, Seagrass = 0,
                                          Macroalgae = 0, Mangrove = 0)
europabon_t$Phytoplankton[str_which(europabon_t$EOVTaxa, 'Phytoplankton')] <- 1
europabon_t$Fish[str_which(europabon_t$EOVTaxa, 'Fish')] <- 1
europabon_t$Birds[str_which(europabon_t$EOVTaxa, 'Birds')] <- 1
europabon_t$Mammals[str_which(europabon_t$EOVTaxa, 'Mammals')] <- 1
europabon_t$Macroalgae[str_which(europabon_t$EOVTaxa, 'Macroalgae')] <- 1
europabon_t$StartYear[which(europabon_t$StartYear == 9999)] <- NA
europabon_t$EndYear[which(europabon_t$EndYear == 9999)] <- NA

# extract start end years from EuroSea free text
eurosea_t$StartYear <- str_extract(eurosea_t$Time, '^[:digit:]{4}') # extract the first 4 digits from Time as start year
eurosea_t$EndYear <- str_extract(eurosea_t$Time, '(?<=\\-\\s?)[:digit:]{4}$|(?<=\\,\\s?)[:digit:]{4}$') 
# if any of the time strings have a year at the end, those are our known end years
eurosea_t$Ongoing <- NA
eurosea_t$Ongoing[str_which(eurosea_t$Time, 'current|present')] <- 'Yes' # ongoing if it says current or present
eurosea_t$Ongoing[str_which(eurosea_t$Time, 'suspended')] <- 'No'
eurosea_t$Ongoing[which(!is.na(eurosea_t$EndYear))] <- 'No' # fill in Ongoing No for rows where we know end years for
eurosea_t$Time <- NULL

# change taxa NAs to 0
replace <- as.list(rep(0,12))
names(replace) <- colnames(eurosea_t)[9:20]
eurosea_t[9:20] <- tidyr::replace_na(eurosea_t[9:20], replace)

eurosea_t$ProgramName <- with(eurosea_t, paste(ProgramName, SubprogramName, sep="_"))
eurosea_t$ProgramName <- str_remove(eurosea_t$ProgramName, '\\_$|\\_NA$|\\,No|^\\,') # no trailing punctuation for programs that don't have subprogram names
eurosea_t$Organisation <- with(eurosea_t, paste(Organisation, RegionalCoord, sep=","))
eurosea_t$Organisation <- str_remove(eurosea_t$Organisation, '\\,$|\\,NA$|\\,No|^\\,|\\,\\-$') # no trailing punctuation for programs that don't have regional coordination
eurosea_t$SubprogramName <- NULL
eurosea_t$RegionalCoord <- NULL

# add blank columns that need to be manually filled in later on
eurosea_t$GeographicalCoverage <- NA
eurosea_t$TargetedSpecies <- NA
eurosea_t$EBV <- NA
eurosea_t$SpatialRes <- NA
eurosea_t$GrainSize <- NA

europabon_t$Organisation <- NA
europabon_t$DatabaseOrigin <- NA

# order as closely as possible to our target end
europabon_t <- europabon_t %>% select(ProgramName,
                                      Organisation,
                                      DatabaseOrigin,
                                      GeographicalCoverage,
                                      Country,
                                      Location,
                                      URL,
                                      TargetedSpecies,
                                      SpatialRes,
                                      GrainSize,
                                      EBV,
                                      EOVTaxa,
                                      Microbes:Mangrove,
                                      StartYear,
                                      EndYear,
                                      Ongoing,
                                      TemporalRes)

eurosea_t <- eurosea_t %>% select(ProgramName,
                                  Organisation,
                                  DatabaseOrigin,
                                  GeographicalCoverage,
                                  Country,
                                  Location,
                                  URL,
                                  TargetedSpecies,
                                  SpatialRes,
                                  GrainSize,
                                  EBV,
                                  Microbes:Mangrove,
                                  StartYear,
                                  EndYear,
                                  Ongoing,
                                  TemporalRes,
                                  SOP_cite,
                                  Latitude,
                                  Longitude)

europabon_t <- europabon_t %>% mutate(DataID = paste('EB', rownames(europabon_t), sep="_"), .before = ProgramName)
eurosea_t <- eurosea_t %>% mutate(DataID = paste('ES', rownames(eurosea_t), sep="_"), .before = ProgramName)

# write.csv(europabon_t, 'EuropaBON_Div_cleaned.csv', row.names=F)
# write.csv(eurosea_t[-29:31], 'EuroSea_Div_cleaned.csv', row.names=F)

# SOP/BP ------------------------------------------------------------------

task3 <- eurosea_t %>% select(SOP_cite, DataID) %>% distinct(SOP_cite, DataID) %>% filter(!SOP_cite == '')
# write.csv(task3, 'EuroSea_SOP.csv', row.names=F)


## To avoid overinflating monitoring programmes that were split by country, reintroduce the Subprogram Column
# require(readxl)
# require(stringr)
# require(dplyr)
# 
# task2 <- read_excel('Task2_MonitoringProgrammes.xlsx', sheet='Data')
# task2 %>% filter(str_detect(ProgramName, '\\_')) %>% View
# # look for underscores              
# newnames <- str_split(task2$ProgramName, '\\_', n=2)
# subnames <- do.call(rbind.data.frame, newnames)
# names(subnames) <- c('ProgramName2', 'SubprogramName')
# subnames[which(subnames[1] == subnames[2]),2] <- ''
# subnames <- cbind(subnames, task2$ProgramName)
# 
# clipr::write_clip(subnames[-3])