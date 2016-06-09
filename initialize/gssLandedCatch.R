##########################
# landings data
##########################

mfdb_import_cs_taxonomy(mdb,'index_type',data.frame(name='landings'))

source('initialize/gssLandedCatchLogbooks.R')
source('initialize/gssLandedCatchSeaSamplingProps.R')

landings <- 
    rbind(bmt.landings, other.landings) %>%
    arrange(year, areacell)

landings <- data.table(landings)

######################################
# must input month into landings data 
# before able to import
######################################

# import landings data into mfdb
mfdb_import_survey(mdb,
                   data_source = 'commercial.landings',
                   landings)

