##########################
# commercial catch samples
##########################
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


# import length distribution from commercial catch samples
ldist <- data.table(all.le) %>%
    filter(synis.id %in% stations$synis.id &
               tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    left_join(data.table(all.nu)) %>%
    mutate(count=fjoldi,
           sex = c('M','F')[pmax(1,kyn)],
           age = 0,
           month = man,
           sampling_type = 'SEA',
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           fjoldi = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL) 

setnames(ldist,
         c('synis.id','ar','lengd'),
         c('sample.id','year','length'))

# mfdb_import_survey(mdb,
#                   data_source = 'iceland-ldist.comm',
#                   ldist)
catches <- ldist # saving this for plotting purposes later on

rm(ldist)

# import age-length frequencies for commercial catch samples
if (FALSE){
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
}