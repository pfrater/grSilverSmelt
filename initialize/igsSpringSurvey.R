##########################
# spring survey
##########################

# set up the stations from the spring survey
stations <-
    data.table(subset(translate.stodvar(), sampling.type == 30, ## autumn survey 35
                      select = c(sample.id,year,month,lat,lon,gear.type))) %>%
    left_join(data.table(mapping)) %>%
    group_by(sample.id) %>%
    mutate(month = 3,
           areacell = d2sr(lat,lon),
           sampling_type = 'IGFS') %>%
    filter(!is.na(areacell))

## Import length distribution from spring survey

ldist <- translate.all.le() %>%
    filter(sample.id %in% stations$sample.id &
               species.code %in% species.key$species.code) %>%
    group_by(sample.id,species.code) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    left_join(translate.all.nu()) %>%
    mutate(count=round(count*pmax(count.considered+count.recommended,1,na.rm=TRUE)/
                           pmax(1,count.recommended,na.rm=TRUE)),
           sex = c('M','F')[pmax(1,sex)],
           age = 0,
           maturity_stage = pmax(1,pmin(maturity,2))) %>%
    ungroup() %>%
    mutate(count.recommended = NULL,
           count.considered = NULL,
           catch=NULL,
           station.wt = NULL,
           species.code = NULL,
           gear.type = NULL)           

ldist <- data.table(ldist)

mfdb_import_survey(mdb,
                   data_source = 'iceland-ldist.igfs',
                   ldist)
rm(ldist)

## Import age - length frequencies from the spring survey
aldist <- translate.all.kv() %>%
    filter(sample.id %in% stations$sample.id &
               species.code %in% species.key$species.code) %>%
    group_by(sample.id,species.code) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    mutate(count=1,
           areacell = d2sr(lat,lon),
           sex = c('M','F')[pmax(1,sex)],
           sampling_type = 'IGFS',
           month = 3,
           maturity_stage = pmax(1,pmin(maturity,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup()

aldist <- data.table(aldist)

mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist.igfs',
                   aldist)
rm(aldist)
