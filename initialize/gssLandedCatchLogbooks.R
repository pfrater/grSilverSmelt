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


# merge landedcatch with monthly catch proportions to use later with importing areacells
monthly.catch <- landedcatch %>%
    filter(gear.type == 6) %>%
    group_by(year) %>%
    summarize(catch = sum(catch)) %>% 
    left_join(catch.by.month) %>%
    mutate(prop = ifelse(is.na(prop),
                         1, prop),
           prop.catch = prop * catch) %>%
    select(year, month, prop.catch)


# merge landedcatch with catch proportions and calculate catch areas
# separating out large from small catches
# doing large catches first
big.catches <- monthly.catch %>%
    group_by(year, month) %>%
    summarize(catch = sum(prop.catch)) %>%
    filter(catch >= 1000) %>%
    left_join(ann.prop.catch)
# was having a weird issue with piping into mutate(prop.catch = prop * catch)
# not calculating for 1988
big.catches$prop.catch <- big.catches$prop * big.catches$catch

bc.86 <- 
    filter(big.catches, year==1986) %>%
    select(year, month, catch) %>%
    cbind(areacell=filter(big.catches, year==1987)$areacell, 
          prop=filter(big.catches, year==1987)$prop) %>%
    mutate(prop.catch = prop * catch,
           month = 7)
bc.89 <- 
    filter(big.catches, year==1989) %>%
    select(year, month, catch) %>%
    cbind(areacell=filter(big.catches, year==1988, month==11)$areacell, 
          prop=filter(big.catches, year==1988, month==11)$prop) %>%
    mutate(prop.catch = prop * catch,
           month = 7)
bc.16 <- 
    filter(big.catches, year==2016) %>%
    select(year, catch) %>%
    cbind(areacell=filter(big.catches, year==2015, month==4)$areacell, 
          prop=filter(big.catches, year==2015, month==4)$prop) %>%
    mutate(prop.catch = prop * catch,
           month = 2)
big.catches.all <- big.catches %>%
    filter(!is.na(areacell)) %>%
    rbind(bc.86, bc.89, bc.16) %>%
    ungroup() %>%
    select(year, month, areacell, prop.catch) %>%
    arrange(year, month, areacell)

# now the small catches
small.catches <- monthly.catch %>%
    group_by(year) %>%
    summarize(prop.catch = sum(prop.catch)) %>%
    filter(prop.catch < 1000) %>%
    mutate(areacell = sample(areacell.probs$areacell, 3, prob=areacell.probs$prop),
           month = sample(month.probs$fishing.month, 3, prob=month.probs$prop))

bmt.landings <- rbind(small.catches, big.catches.all) %>%
    mutate(gear = 'BMT',
           species = 'GSS',
           sampling_type = 'LND') %>%
    rename(catch = prop.catch) %>%
    select(year, month, areacell, sampling_type, gear, catch, species)


# vaska upp
rm(list=c('monthly.catch', 'big.catches', 'bc.16', 'bc.86', 'bc.89', 
          'big.catches.all', 'small.catches',
          'bottom.trawls',  'gss', 'catch.by.month', 'ann.prop.catch', 
          'areacell.probs', 'month.probs'))







