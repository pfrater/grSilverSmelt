##########################
# landings data
##########################

mfdb_import_cs_taxonomy(mdb,'index_type',data.frame(name='landings'))

# read in data for gss landed catch
landedcatch <- read.csv('data/gss_landings.csv', header=T)

# adjust mapping dataset
gridcell.mapping <-
    plyr::ddply(reitmapping,~DIVISION,function(x) head(x,1)) %>%
    rename(areacell = GRIDCELL)
names(gridcell.mapping) <- tolower(names(gridcell.mapping))


##############################################################
# importing where trawls occurred and using that as an index for 
# where fish were caught, not ideal, but it works
source('computeAreas.R') # bring in calculations on proportion of trawls that occurred 
                         # on each area in each year: trawl.prop


# fix up landings data for import into mfdb
landings <- landedcatch %>%
    left_join(mapping, by=c('veidarf' = 'veidarfaeri')) %>%
    within(rm(veidarf)) %>%
    mutate(
        sampling_type='LND',
        year = ar,
        month = man
    ) %>%
    select(year, month, sampling_type, gear, catch)
landings$species <- species.key$species
landings$areacell <- 3173   # areacell chosen as the greatest number of commercial catch samples over sampling period
                            # see tapply(cy.area.all$year, cy.area.all$areacell, length)

# import landings data into mfdb
mfdb_import_survey(mdb,
                   data_source = 'commercial.landings',
                   landings)

