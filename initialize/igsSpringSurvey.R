##########################
# spring survey
##########################

stations <-
    data.table(subset(stodvar, synaflokkur == 30, ## autumn survey 35
                      select = c(synis.id,ar,man,lat,lon,veidarfaeri))) %>%
    left_join(data.table(mapping)) %>%
    group_by(synis.id) %>%
    setnames(old="ar",new="year") %>%
    mutate(month = 3,
           areacell = d2sr(lat,lon),
           sampling_type = 'IGFS') %>%
    filter(!is.na(areacell))

## Import length distribution from spring survey

ldist <- data.table(all.le) %>%
    filter(synis.id %in% stations$synis.id &
               tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    left_join(data.table(all.nu)) %>%
    mutate(count=round(fjoldi*pmax(fj.talid+fj.maelt,1,na.rm=TRUE)/
                           pmax(1,fj.maelt,na.rm=TRUE)),
           sex = c('M','F')[pmax(1,kyn)],
           age = 0,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    ungroup() %>%
    mutate(man = NULL,
           fjoldi = NULL,
           kyn = NULL,
           kynthroski=NULL,
           fj.maelt = NULL,
           fj.talid = NULL,
           afli=NULL,
           vigt.synis = NULL,
           tegund = NULL,
           veidarfaeri = NULL)           

setnames(ldist,
         c('synis.id','lengd'),
         c('sample.id','length'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-ldist.igfs',
                   ldist)
rm(ldist)

## Import age - length frequencies from the spring survey
aldist <- data.table(all.kv) %>%
    filter(synis.id %in% stations$synis.id &
               tegund %in% species.key$tegund) %>%
    group_by(synis.id,tegund) %>%
    left_join(stations) %>%
    left_join(species.key) %>%
    mutate(count=1,
           areacell = d2sr(lat,lon),
           sex = c('M','F')[pmax(1,kyn)],
           age = aldur,
           sampling_type = 'IGFS',
           month = 3,
           maturity_stage = pmax(1,pmin(kynthroski,2))) %>%
    filter(!is.na(areacell)) %>%
    ungroup() %>%
    mutate(man = NULL,
           kyn = NULL,
           kynthroski=NULL,
           tegund = NULL,
           veidarfaeri = NULL,
           aldur=NULL)           

setnames(aldist,
         c('synis.id','lengd', 'nr',
           'oslaegt', 'slaegt', 'lifur','kynfaeri'),
         c('sample.id','length','no',
           'weight','gutted','liver', 'gonad'))

mfdb_import_survey(mdb,
                   data_source = 'iceland-aldist.igfs',
                   aldist)
rm(aldist)
