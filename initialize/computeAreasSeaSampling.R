### working with fjolst trawl data in stodvar dataset
### to determine where greater silver smelt were harvested from 1982 - 2016


# obtain the stations and lengths data sets from fjolst
# and subset to greater silver smelt only
stations <- translate.stodvar()
gss.le <- translate.all.le() %>% filter(species.code==19)
gss.kv <- translate.all.kv() %>% filter(species.code==19)
gss.nu <- translate.all.nu() %>% filter(species.code==19)

# using the length data set (all.le) to obtain what proportion of fish were caught where
gss <- gss.nu %>% 
        filter(fishing.gear != 30 | fishing.gear != 35) %>%
        left_join(gss.le) %>% 
        left_join(stations) %>%
        select(sample.id, year, fishing.gear, species.code, count.considered, 
               count.recommended, catch, station.wt, length, count, trawl.number,
               month, day, net.cast.time, net.pull.time, ship, station,
               net.cast.lat, net.cast.long, net.pull.lat, net.pull.long,
               depth.at.cast, depth.at.pull, gear.type, gridcell, total.catch,
               lat, lon, depth, surface.temp, air.temp, barometer,
               trawl.depth.at.cast, trawl.depth.at.pull, trawl.length,
               landing.port, sweep.length)

## plot trawls on map of Iceland to get visual sense for where fishing occurs
#source('~/R/Rscripts/icelandicGridcells.R')
#trawl.dots <- bcareas.plot + 
#   geom_point(aes(x=lon, y=lat), data=gss)
#trawl.dots
#trawl.lines <- bcareas.plot + 
#    geom_segment(aes(x=net.cast.long, xend=net.pull.long,
#                     y=net.cast.lat, yend=net.pull.lat), 
#                 data=gss %>% group_by(sample.id) %>% filter(row_number(sample.id) == 1)
#                )
#trawl.lines # uncomment this out to see, takes a few seconds to plot

# calculate the proportion of fishing performed in each area (gridcell or division)
# for gear types other than bottom trawl
ogt.catch <- gss %>% 
                filter(fishing.gear != 6) %>% 
                select(sample.id, year, fishing.gear, species.code,
                       catch, month, day, ship, lat, lon) %>%
                group_by(sample.id) %>%
                filter(row_number(sample.id) == 1) %>%
                mutate(gridcell = d2sr(lat=lat, lon=lon)) %>%
                rename(gear.type = fishing.gear) %>%
                filter(gear.type %in% other.gt) #from gssLandedCatchSeaSamplingProps.R
                
ogt.catch.prop <- ogt.catch %>%
                filter(!is.na(gridcell)) %>%
                group_by(year, gear.type, gridcell) %>%
                summarize(n = n()) %>%
                mutate(prop = n / sum(n)) %>%
                filter(year > 1980)

ogt.prop.ann <- ogt.catch %>%
                group_by(year, gridcell) %>%
                summarize(n = n()) %>%
                mutate(prop = n / sum(n)) %>%
                filter(year > 1980)







