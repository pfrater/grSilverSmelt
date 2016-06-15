### working with Logbooks to determine where fishermen fished
 ## and compute a proportion to apply to landedcatch data


# bottom trawls are the only Logbook entries that report gr silver smelt
bottom.trawls <- translate.botnv() %>% filter(gr.silver.smelt > 0)
bottom.trawls$areacell <- d2sr(lat=bottom.trawls$lat, lon=bottom.trawls$lon)
bottom.trawls$areacell <- 
    ifelse(is.na(bottom.trawls$areacell), 
           as.numeric(paste(bottom.trawls$gridcell[is.na(bottom.trawls$areacell)],
                            sample(x=1:4, 
                                   size=nrow(filter(bottom.trawls, is.na(areacell))), 
                                   replace=T),
                            sep='')),
           bottom.trawls$areacell)


#bottom.trawls$small.gridcell <- ifelse(is.na(bottom.trawls$small.gridcell),
#                                      as.numeric(substr(d2sr(bottom.trawls$lat,
#                                                             bottom.trawls$lon),
#                                                        start=4, stop=4)),
#                                      bottom.trawls$small.gridcell)


gss <- 
    bottom.trawls %>% 
    select(id, ship.number, lat, lon, areacell, depth,
           landing.port, year, fishing.day, fishing.month, landing.month, 
           landing.day, area, trawl.time, bottom.temp, mesh.size, number.per.kilo,
           size, type, bottom, gr.silver.smelt)

# first calculate the proportion of catches per year per month
# this is so that catches don't get replicated 12 times upon left_join
catch.by.month <- gss %>%
    group_by(year, fishing.month) %>%
    summarize(total.catch = sum(gr.silver.smelt)) %>%
    mutate(prop = total.catch / sum(total.catch)) %>%
    rename(month = fishing.month) %>%
    select(year, month, prop)


# calculate the proportion of bottom trawls per year, per month, per gridcell
ann.prop.catch <- gss %>% 
    group_by(year, fishing.month, areacell) %>%
    summarize(total.catch = sum(gr.silver.smelt)) %>%
    mutate(prop = total.catch / sum(total.catch)) %>%
    select(year, fishing.month, areacell, prop) %>%
    rename(month = fishing.month)


areacell.probs <- 
    gss %>%
    group_by(areacell) %>%
    summarize(total.catch = sum(gr.silver.smelt)) %>%
    mutate(prop = total.catch / sum(total.catch)) %>%
    select(areacell, prop)

month.probs <- 
    gss %>%
    group_by(fishing.month) %>%
    summarize(n = n()) %>%
    mutate(prop = n / sum(n))




