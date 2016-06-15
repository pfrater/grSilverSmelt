##########################
# landings data
##########################

mfdb_import_cs_taxonomy(mdb,'index_type',data.frame(name='landings'))

source('initialize/gssLandedCatchLogbooks.R')
source('initialize/gssLandedCatchSeaSamplingProps.R')

landings <- 
    rbind(bmt.landings, pgt.landings, other.landings) %>%
    filter(areacell != -1818, areacell != -1316, areacell != 3684) %>%
    arrange(year, areacell)

landings <- data.table(landings)


# import landings data into mfdb
mfdb_import_survey(mdb,
                   data_source = 'commercial.landings',
                   landings)

