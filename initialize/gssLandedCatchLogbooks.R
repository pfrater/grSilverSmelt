###############################################################
# landings data with proportions computed from Logbooks entries
###############################################################

# read in data for gss landed catch
landedcatch <- read.csv('data/gss_landings.csv', header=T)
landedcatch <- rename(landedcatch,
                      year = ar,
                      month = man,
                      gear.type = veidarf)

# adjust mapping dataset
gridcell.mapping <-
    plyr::ddply(reitmapping,~DIVISION,function(x) head(x,1)) %>%
    rename(areacell = GRIDCELL)
names(gridcell.mapping) <- tolower(names(gridcell.mapping))

# source in catch proportions from Logbooks data
source('initialize/computeAreasLogbooks.R')

# merge landedcatch with catch proportions and calculate catch areas
# separating out large from small catches
# doing large catches first
big.catches <- landedcatch %>%
    filter(gear.type == 6) %>%
    group_by(year) %>%
    summarize(catch = sum(catch)) %>%
    filter(catch >= 1000) %>%
    left_join(ann.prop.catch) %>%
    mutate(prop.catch = prop * catch)
bc.86 <- 
    filter(big.catches, year==1986) %>%
    select(year, catch) %>%
    cbind(areacell=filter(big.catches, year==1987)$areacell, 
          prop=filter(big.catches, year==1987)$prop) %>%
    mutate(prop.catch = prop * catch)
bc.89 <- 
    filter(big.catches, year==1989) %>%
    select(year, catch) %>%
    cbind(areacell=filter(big.catches, year==1988)$areacell, 
          prop=filter(big.catches, year==1988)$prop) %>%
    mutate(prop.catch = prop * catch)
bc.16 <- 
    filter(big.catches, year==2016) %>%
    select(year, catch) %>%
    cbind(areacell=filter(big.catches, year==2015)$areacell, 
          prop=filter(big.catches, year==2015)$prop) %>%
    mutate(prop.catch = prop * catch)
big.catches.all <- big.catches %>%
    filter(!is.na(areacell)) %>%
    rbind(bc.86, bc.89, bc.16) %>%
    select(year, areacell, prop.catch)

# now the small catches
small.catches <- landedcatch %>%
    filter(gear.type == 6) %>%
    group_by(year) %>%
    summarize(catch = sum(catch)) %>%
    filter(catch < 1000) %>%
    mutate(areacell = sample(areacell.probs$areacell, 3, prob=areacell.probs$prop)) %>%
    rename(prop.catch = catch)

bmt.landings <- rbind(small.catches, big.catches.all) %>%
    mutate(gear.type = 6,
           species = 'GSS',
           sampling_type = 'LND') %>%
    left_join(mapping) %>%
    rename(catch = prop.catch) %>%
    select(year, areacell, sampling_type, gear, catch, species)


# vaska upp
rm(list=c('bc.16', 'bc.86', 'bc.89', 'big.catches', 'big.catches.all', 'small.catches',
          'bottom.trawls', 'ann.prop.catch', 'areacell.probs', 'gss'))







