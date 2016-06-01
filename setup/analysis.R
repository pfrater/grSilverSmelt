# analyzing the output from final gadget run from using gadget.iterative in run.R

# source('~/R/rgadget/trunk/gadgetFileIO.R') ## gadget.fit function doesn't work when this is read in
#source('~/R/rgadget/trunk/gadgetfunctions.R')
#source('~/R/rgadget/trunk/gadgetClass.R')
#source('~/R/rgadget/trunk/gadgetMethods.R')
#source('~/R/rgadget/trunk/function.R')

library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(Rgadget)
setwd('~/gadget/grSilverSmelt/gssModel')
fit <- gadget.fit(wgts="WGTS", main.file='WGTS/main.final')

## fit statistics
resTable <- fit$resTable[tail(head(names(fit$resTable),-2),-1)]
names(resTable) <- c('MATs', 'ALKc','ALKs','LDc','LDs','SI 15-25',
                     'SI 25-40','SI 40-55')
rownames(resTable) <- c('Catch','Survey', 'Maturation', 'Sind','Final')

summary.plot <-
    ggplot(subset(fit$likelihoodsummary,
                  year!='all'),
           aes(as.numeric(year), likelihood.value)) +
    geom_point() + facet_wrap(~component) +theme_bw()+
    xlab('Year') + ylab('Score')


## to calculate biomass index
tmp <- rbind.fill(fit$sidat,
                  ddply(fit$sidat,~year, summarise,
                        number.x = sum(number.x*0.00000659*lower^3.01721 ),
                        predict = sum(predict*0.00000659*lower^3.01721 ),
                        upper = sum(upper*0.00000659*lower^3.01721 ),
                        lower = sum(lower*0.00000659*lower^3.01721 ),
                        lower = 110, lower = 180,
                        length = 'Biomass'))

# plot the model survey data over the actual survey data
si.fit.survey <-
    ggplot(tmp, aes(year,number.x)) +
    geom_point() +
    geom_line(aes(year,predict)) +
    geom_linerange(data=subset(tmp,year==max(year)),
                   aes(year,ymax=number.x,ymin=predict),col='green')+
    geom_text(data=mutate(subset(tmp,year==min(year)),y=Inf),
              aes(year,y,label=length), vjust = 2,hjust = -1)+
    facet_wrap(~length,scale='free_y',ncol=2) + theme_bw() +
    ylab('Index') + xlab('Year') +
    theme (panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())


# plot the survey length-distribution data over the actual survey length-distribution data
ldist.fit.survey <-
    ggplot(subset(fit$catchdist.fleets,name == 'ldist.igfs'),
           aes(lower,predicted)) +
    geom_line(aes(lower,observed),col='gray') +
    facet_wrap(~year+step) + theme_bw() + geom_line() +
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == 'ldist.igfs' & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab('Proportion') + xlab('length') +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())

# plot the model catchdistribution data over actual catchdistribution data
ldist.fit.catch <-
    ggplot(subset(fit$catchdist.fleets,name == 'ldist.catch'),
           aes(lower,predicted)) +
    geom_line(aes(lower,observed),col='gray') +
    facet_wrap(~year+step) + theme_bw() + geom_line() +
    geom_text(data=mutate(subset(fit$catchdist.fleets,
                                 name == 'ldist.catch' & lower==min(lower)),y=Inf),
              aes(lower,y,label=year), vjust = 2,hjust = -1)+
    ylab('Proportion') + xlab('length') +
    theme (axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           panel.margin = unit(0,'cm'), plot.margin = unit(c(0,0,0,0),'cm'),
           strip.background = element_blank(), strip.text.x = element_blank())

# plot suitability against length for both survey and commercial fleets
selection.plot <-
    ggplot(fit$suitability,
           aes(l,suit,lty=fleet)) +
    geom_line() +
    theme_bw() + ylab('Suitability') + xlab('Length') +
    theme(legend.position = c(0.8,0.25), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm')) 


# plot growth curve from model
# gssimm looks good
# there is a problem here with the growth for gssmat
gr.plot <-
    ggplot(fit$stock.growth,
           aes(age,length, color=stock)) + 
    geom_line(aes(color=stock)) +
    theme_bw() + ylab('Length') + xlab('Age') +
    theme(legend.position = c(0.9,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))


# plot recruitment of stock by year
rec.plot <-
    ggplot(fit$res.by.year,aes(year,recruitment/1e6)) +
    geom_bar(stat='identity') +
    ylab("Recruitment (in millions)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))


# plotting the catch by year
catch.plot <- 
ggplot(fit$res.by.year,aes(year,catch/1000)) +
    geom_bar(stat='identity') +
    ylab("Catches (in tons)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))


# plotting the biomass by year
biomass.plot <- 
    ggplot(fit$res.by.year,aes(year,total.biomass/1000)) +
    geom_bar(stat='identity') +
    ylab("Total biomass (in tons)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))


# plotting the harvest per year
harv.plot <- 
    ggplot(fit$res.by.year,aes(year,harv.biomass/1000)) +
    geom_bar(stat='identity') +
    ylab("Harvestable biomass (in tons)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))


# plot sustainable harvest biomass per year
ssb.plot <- 
    ggplot(fit$res.by.year,aes(year,ssb/1000)) +
    geom_bar(stat='identity') +
    ylab("SSB (in tons)") + xlab('Year') +  theme_bw() +
    theme(legend.position = c(0.25,0.75), legend.title = element_blank(),
          plot.margin = unit(c(0,0,0,0),'cm'))


##################################################################################
## there are other plots that Bjarki has put together looking at yield per recruit
## and forward projections, but you should get the model working first
## start with gssmat growth. There seems to be something wrong there
##################################################################################














