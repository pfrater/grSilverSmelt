#############################################################
# landings data with proportions computed from sea sampling
# these are for the gear.types other than bottom.trawl
# no GSS were reported caught for other gear.types in Logbooks
#############################################################

# necessary datasets landedcatch and gridcell.mapping should be present

other.gt <- unique(filter(landedcatch, gear.type != 6)$gear.type)

##############################################################
# importing where trawls occurred and using that as an index for 
# where fish were caught, not ideal, but it works
source('initialize/computeAreasSeaSampling.R') # bring in calculations on proportion of trawls that occurred 
                         # on each area in each year: trawl.prop

# merge landedcatch and trawl.prop and assign the landedcatch to
# appropriate gridcells based on fishing effort there

# bottom.trawls makeup 96% of catch, and pelagic trawls make up 98% of the additional 
# 4%, so calculating for pgt
pgt.ann.catch <- landedcatch %>%
    filter(gear.type == 7 | gear.type == 21) %>%
    group_by(year) %>%
    summarize(catch = sum(catch)) 

pgt.monthly.catch.prop <- pgt.ann.catch %>%
    left_join(pgt.monthly.prop)

for (i in 1:nrow(pgt.monthly.catch.prop)) {
    if (is.na(pgt.monthly.catch.prop$month[i])) {
        pgt.monthly.catch.prop$month[i] <- sample(pgt.month.prob$month, 1, prob=pgt.month.prob$prop)
        pgt.monthly.catch.prop$prop[i] <- 1
    }
}

pgt.monthly.catch <- pgt.monthly.catch.prop %>%
    mutate(prop.catch = prop * catch) %>%
    select(year, month, prop.catch) %>%
    rename(catch = prop.catch)

pgt.catch.prop <- pgt.monthly.catch %>%
    left_join(pgt.catch.props)

# for monthly landings with gridcells missing
# randomly assigns a gridcell based on weighted probability 
# of reported sea samples for pelagic trawls
for (i in 1:nrow(pgt.catch.prop)) {
    if (is.na(pgt.catch.prop$gridcell[i])) {
        pgt.catch.prop$gridcell[i] <- sample(pgt.gridcell.probs$gridcell, 1, 
                                             prob=pgt.gridcell.probs$prop)
        pgt.catch.prop$prop[i] <- 1
    }
}

pgt.landings <- pgt.catch.prop %>%
    mutate(prop.catch = catch * prop) %>%
    select(year, month, gridcell, prop.catch) %>%
    rename(catch = prop.catch,
           areacell = gridcell) %>%
    mutate(gear = 'PGT',
           species = 'GSS',
           sampling_type = 'LND')


# randomly assign a gridcell to other gear.types based on weighted probability
other.catch <- landedcatch %>%
    filter(gear.type %in% other.gt)

other.catch$gridcell <- NA
for (i in 1:nrow(other.catch)) {
    temp.grid <- filter(other.gt.catch.props, gear.type == other.catch$gear.type[i]);
    if (nrow(temp.grid)==0) {
        other.catch$gridcell[i] <- sample(gridcell.probs$gridcell, 1,
                                       prob=gridcell.probs$prop)
    }
    else if (nrow(temp.grid) == 1) {
        other.catch$gridcell[i] <- temp.grid$gridcell;
    } 
    else { 
        other.catch$gridcell[i] <- sample(temp.grid$gridcell, 1, prob=temp.grid$prop);
    }
}

other.landings <- other.catch %>%
    left_join(mapping) %>%
    mutate(species = 'GSS',
           sampling_type = 'LND') %>%
    rename(areacell = gridcell) %>%
    select(year, month, areacell, sampling_type, gear, catch, species)


# vaska upp
rm(list=c('pgt.ann.catch', 'pgt.monthly.catch.prop', 'pgt.monthly.catch',
          'pgt.catch.prop', 'other.catch', 'temp.grid', 'i', 'stations', 
          'gss', 'gss.kv', 'gss.le', 'gss.nu', 'ogt.catch', 'pgt.monthly.prop',
          'pgt.month.prob', 'pgt.catch.props', 'pgt.gridcell.probs', 
          'other.gt', 'other.gt.catch.props', 'gridcell.probs', 'ogt'))






