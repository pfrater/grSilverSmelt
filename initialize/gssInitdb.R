library(mfdb)
library(fjolst)
library(fjolstTranslate)
library(geo)
library(dplyr)
library(data.table)

setwd('/home/pfrater/gadget/grSilverSmelt')

# Create connection to MFDB database, as the Icelandic case study
mdb <- mfdb('Iceland')

species.key <- data.table(species.code = 19, species = 'GSS')
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
    year = rep(1960:2015, each=12),
    month = 1:12,
    areacell = reitmapping$GRIDCELL[1],
    temperature = 3))

# Set-up some sampling types
mfdb_import_sampling_type(mdb, data.frame(
    id = c(1,2,3,5,6,9),
    name = c('SEA', 'IGFS','AUT','LND','LOG', 'FLND'),
    description = c('Sea sampling', 'Icelandic ground fish survey',
                    'Icelandic autumn survey', 'Landings','Logbooks',
                    'Foreign vessel landings')))


##########################
# Import data to mfdb
##########################
source('initialize/commercialCatchSamples.R') #imports samples from commercial catch to mfdb
source('initialize/igsSpringSurvey.R') #imports spring IGS survey data to mfdb
source('initialize/igsAutumnSurvey.R') #imports autumn IGS survey data to mfdb
source('initialize/gssLandedCatch.R')  #imports data for landed catch for Greater silver smelt
    # use samples.plot to plot out counts from commercial catch samples













