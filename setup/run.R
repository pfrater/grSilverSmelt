library(Rgadget)
setwd('~/gadget/grSilverSmelt/gssModel')
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind=c('si.1525', 'si.2540','si.4055'),
                                      survey=c('ldist.igfs', 'aldist.igfs'),
                                      comm=c('ldist.comm', 'aldist.comm')),
                        wgts='WGTS')

