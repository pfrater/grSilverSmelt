##########################
# commercial catch samples
##########################
stations <-
    data.table(subset(translate.stodvar(),
                      sampling.type %in% c(1,8),
                      select = c(sample.id,year,month,lat,lon,gear.type))) %>%
    left_join(data.table(mapping)) %>%
    group_by(sample.id) %>%
    mutate(areacell = d2sr(lat,lon),
           gear.type = NULL) %>%
    filter(areacell %in% reitmapping$GRIDCELL &
               !is.na(gear))


# import length distribution from commercial catch samples
ldist <- translate.all.le() %>%
    filter(sample.id %in% stations$sample.id &
               species.code %in% species.key$species.code) %>%
    group_by(sample.id,species.code) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    left_join(translate.all.nu()) %>%
    mutate(sex = c('M','F')[pmax(1,sex)],
           age = 0,
           sampling_type = 'SEA',
           maturity_stage = pmax(1,pmin(maturity,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup()

ldist <- data.table(ldist)

mfdb_import_survey(mdb,
                   data_source = 'iceland-ldist.comm',
                   ldist)
catches <- ldist # saving this for plotting purposes later on

rm(ldist)

# import age-length frequencies for commercial catch samples
aldist <-
    translate.all.kv() %>%
    filter(sample.id %in% stations$sample.id &
               species.code %in% species.key$species.code) %>%
    group_by(sample.id,species.code) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    mutate(count=1,           
           sex = c('M','F')[pmax(1,sex)],
           sampling_type = 'SEA',
           maturity_stage = pmax(1,pmin(maturity,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup()

aldist <- data.table(aldist)

mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist.comm',
                   aldist)
rm(aldist)
