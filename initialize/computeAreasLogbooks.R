### working with Logbooks to determine where fishermen fished
 ## and compute a proportion to apply to landedcatch data


# bottom trawls are the only Logbook entries that report gr silver smelt
bottom.trawls <- translate.botnv() %>% filter(gr.silver.smelt > 0)

gss <- 
    bottom.trawls %>% 
    select(id, ship.number, lat, lon, gridcell, small.gridcell, depth,
           landing.port, year, fishing.day, fishing.month, landing.month, 
           landing.day, area, trawl.time, bottom.temp, mesh.size, number.per.kilo,
           size, type, bottom, gr.silver.smelt) %>%
    mutate(areacell = ifelse(is.na(small.gridcell),
            as.numeric(paste(gridcell, sample(1:4, 552, replace=T), sep='')),
            as.numeric(paste(gridcell, small.gridcell, sep=''))
        ))

# calculate the proportion of trawls performed in each area (gridcell or division)
ann.prop.catch <- gss %>% 
    group_by(year, areacell) %>%
    summarize(total.catch = sum(gr.silver.smelt)) %>%
    na.omit() %>% 
    group_by(year) %>%
    mutate(prop = total.catch / sum(total.catch)) %>%
    select(year, areacell, prop)

#rm(list=c('bottom.trawls', 'gss'))

areacell.probs <- 
    gss %>%
    group_by(areacell) %>%
    summarize(total.catch = sum(gr.silver.smelt)) %>%
    mutate(prop = total.catch / sum(total.catch)) %>%
    select(areacell, prop)




