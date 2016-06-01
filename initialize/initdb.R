library(mfdb)
library(fjolst)
library(geo)
library(dplyr)
library(data.table)

# Create connection to MFDB database, as the Icelandic case study
mdb <- mfdb('Iceland')

species.key <-
    data.table(tegund =  c(1:11, 19, 21, 22,
                           23, 25, 27, 30, 31, 48 ),
               species = c('COD','HAD','POK','WHG','RED','LIN','BLI','USK',
                           'CAA','RNG','REB','GSS','HAL','GLH',
                           'PLE','WIT','DAB','HER','CAP','LUM')) %>%
    group_by(tegund)

mapping <- read.table('data/mapping.txt', header=T)

mapping <-
    mutate(merge(mapping, gear, by.x='gear',
                 by.y = 'id'),
           gear=NULL,
           description=NULL)
names(mapping)[2] <- 'gear'
mapping$gear <- as.character(mapping$gear)


# Import area definitions
reitmapping <- read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

mfdb_import_area(mdb, data.frame(
    id = 1:nrow(reitmapping),
    name = c(reitmapping$GRIDCELL),
    size = 30*60*cos(geo::r2d(reitmapping$GRIDCELL)$lat*pi/180)))
mfdb_import_division(mdb, c(
    lapply(split(reitmapping, list(reitmapping$SUBDIVISION)), function (l) l[,'GRIDCELL']),
    NULL))
mfdb_import_temperature(mdb, data.frame(
    year = 2012,
    month = 1:12,
    areacell = reitmapping$GRIDCELL[1],
    temperature = 3))

# Set-up some sampling types
mfdb_import_sampling_type(mdb, data.frame(
    id = c(1:7, 9:11),
    name = c('SEA', 'IGFS','AUT','SMN','LND','LOG','INS', 'FLND','OLND','CAA'),
    description = c('Sea sampling', 'Icelandic ground fish survey',
                    'Icelandic autumn survey','Icelandic gillnet survey',
                    'Landings','Logbooks','Icelandic nephrop survey',
                    'Foreign vessel landings','Old landings (pre 1981)',
                    'Old catch at age')))

stations <-
    data.table(subset(stodvar,
                      synaflokkur %in% c(1,8),
                      select = c(synis.id,ar,man,lat,lon,veidarfaeri))) %>%
    left_join(data.table(mapping)) %>%
    group_by(synis.id) %>%
    mutate(areacell = d2sr(lat,lon),
           veidarfaeri = NULL) %>%
    filter(areacell %in% reitmapping$GRIDCELL &
               !is.na(gear))


# Import length distribution from commercial catches
# 1 inspectors, 2 hafro, 8 on-board discard
comm.stations <- data.table(subset(stodvar, synaflokkur %in% c(1,8)))
comm.ldist <- subset(all.le, synis.id %in% comm.stations$synis.id & 
                         tegund == 19)

comm.ldist <- merge(comm.ldist, 
                    comm.stations[c('synis.id','ar','man','lat','lon')])
comm.ldist$areacell <- d2sr(comm.ldist$lat,-comm.ldist$lon)
names(comm.ldist) <- c('sample.id','species','length','count','sex',
                       'maturity','year','month','lat','lon','areacell')
comm.ldist$species <- 'GSS' 
comm.ldist <- subset(comm.ldist, areacell %in% reitmapping$GRIDCELL) 
comm.ldist$sex <- c('M','F')[comm.ldist$sex]
comm.ldist$age <- 0
comm.ldist$sampling_type <- 'SEA'
mfdb_import_survey(mdb,
                   data_source = 'example-iceland-comm.ldist',
                   comm.ldist)

#################################
# 30 35 # these are the codes for spring and autumn surveys
#################################

# import age-length frequencies
aldist <-
    data.table(all.kv) %>%
    filter(synis.id %in% stations$synis.id &
               tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(data.table(stations)) %>%
    left_join(species.key) %>%
    mutate(count=1,           
           sex = c('M','F')[pmax(1,kyn)],
           age = aldur,
           sampling_type = 'SEA',
           month = man,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL,
           aldur=NULL)

setnames(aldist,
         c('synis.id','ar','lengd', 'nr',
           'oslaegt', 'slaegt', 'lifur','kynfaeri'),
         c('sample.id','year','length','no',
           'weight','gutted','liver', 'gonad'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist.comm',
                   aldist)
rm(aldist)

