#############################################################
# landings data with proportions computed from sea sampling
# these are for the gear.types other than bottom.trawl
# no GSS were reported caught for other gear.types in Logbooks
#############################################################

# necessary datasets landedcatch and gridcell.mapping should be present

# obtain gear types other than bottom trawl used to catch gss
other.gt <- unique(filter(landedcatch, gear.type != 6)$gear.type)

##############################################################
# importing where trawls occurred and using that as an index for 
# where fish were caught, not ideal, but it works
source('initialize/computeAreasSeaSampling.R') # bring in calculations on proportion of trawls that occurred 
                         # on each area in each year: trawl.prop

# merge landedcatch and trawl.prop and assign the landedcatch to
# appropriate gridcells based on fishing effort there

# the best I can do with this method is group 
# fishing effort to gridcell by year (month and gear.type don't align with landedcatch)
other.catch.prop <- landedcatch %>%
    filter(gear.type != 6) %>%
    group_by(year, gear.type) %>%
    summarize(catch = sum(catch)) %>%
    left_join(ogt.catch.prop)

# this is kinda messy
# it assigns a gridcell to prop.catch with NA values for gridcell
# for landedcatch values under 1000, it assigns a gridcell based on weighted probability
# for landedcatch values > 1000 it splits out catch into various gridcells
big.catches <- NULL;
for (i in 1:nrow(other.catch.prop)) {
    if (is.na(other.catch.prop$gridcell[i])) {
        grid.years <- ogt.prop.ann %>%
            filter(year == other.catch.prop$year[i]);
        if (other.catch.prop$catch[i] < 1000) {
            grid.prob <- sample(grid.years$gridcell, 1, prob=grid.years$prop);
            other.catch.prop$gridcell[i] <- grid.prob;
        }
        else {
            big.catch <- 
                select(other.catch.prop[i,], year, gear.type, catch) %>%
                left_join(grid.years) %>%
                mutate(catch = catch * prop) %>%
                select(year, gear.type, catch, gridcell)
            big.catches <- rbind(big.catches, big.catch);
        }
    }
}

other.catch.props <- 
    other.catch.prop %>%
    filter(!is.na(gridcell)) %>%
    full_join(big.catches) %>%
    filter(!is.na(gridcell)) %>%
    arrange(year, gear.type) %>%
    mutate(catch.prop=ifelse(!is.na(prop),
                             catch * prop,
                             catch))
    

# fix up landings data for import into mfdb
other.landings <- other.catch.props %>%
    left_join(mapping) %>%
    mutate(sampling_type='LND') %>%
    rename(areacell = gridcell) %>%
    select(year, areacell, sampling_type, gear, catch.prop) %>%
    rename(catch = catch.prop)
other.landings$species <- species.key$species


# vaska upp
rm(list=c('big.catch', 'big.catches', 'grid.years', 'gss', 'gss.kv', 
          'gss.le', 'gss.nu', 'ogt.catch', 'ogt.catch.prop', 
          'other.catch.props', 'grid.prob', 'i', 'other.gt',
          'ogt.prop.ann'))






