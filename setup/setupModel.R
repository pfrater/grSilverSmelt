library(Rgadget)

# find some decent starting values for recl and stddev
mla <- mfdb_sample_meanlength_stddev(mdb, c('age'),
                                     c(list(sampling_type="IGFS", 
                                            age=1:30), # got 30 from ICES 2013. Report on the workshop on age estimation of deep water species
                                       defaults))

init.sigma <- mla[[1]] %>% na.omit() %>% group_by(age) %>%
                summarize(ml=mean(mean), ms=mean(stddev, na.rm=T))

lw <- mfdb_sample_meanweight(mdb, c('length'),
                            c(list(sampling_type='IGFS', species='GSS',
                                   length=mfdb_interval("", seq(0,60, by=1)))))

lw.tmp <-   lw[[1]] %>% 
            mutate(length=as.numeric(as.character(length)),
                   weight=mean/1e3) %>%
            na.omit() %>% 
            nls(weight ~ a*length^b,.,start=list(a=1e-5,b=3)) %>%
            coefficients() %>%
            as.numeric()

## populate the model with starting default values
opt <- gadget.options(type='simple2stock')

## adapt opt list to greater silver smelt
weight.alpha <- lw.tmp[1]
weight.beta <- lw.tmp[2]

opt$area$numofareas <- 1
opt$time$firstyear <- 1982
opt$time$lastyear <- 2016

## set up immature stock
opt$stocks$imm <- within(opt$stock$imm, {
            name <- 'gssimm'
            minage <- 1
            maxage <- 17
            minlength <- 1
            maxlength <- 55
            dl <- 1
            growth <- c(linf='#gss.linf', 
                        k='#gss.k',
                        beta='(* 10 #gss.bbin)', 
                        binn=15, recl='#gss.recl'
                        )
            weight <- c(a=weight.alpha, b=weight.beta)
            init.abund <- sprintf('(* %s %s)', 
                                  c(0,0.08,0.1,0.1,0.09,0.08,0.06,0.045,0.03,0.02,0.01,0,0,0,0,0,0),
                                 c(0,sprintf('#gss.age%s',2:10),0,0,0,0,0,0,0))
            n <- sprintf('(* 1000 #gss.rec%s)', 1982:2016)
            doesmature <- 1
            maturityfunction <- 'continuous'
            maturestocksandratios <- 'gssmat 1'
            maturity.coefficients <- '( * 0.001 #gss.mat1) #gss.mat2 0 0'
            sigma <- c(init.sigma$ms[1], head(init.sigma$ms, 11), rep(init.sigma$ms[11],5))
            M <- rep(0.15,17)
            maturitysteps <- '0'
            doesmove <- 0
            #transitionstep <- 4
            #transitionstockandratios <- 'gssmat 1'
            doesmigrate <- 0
            doesrenew <- 1
            renewal <- list(minlength=5, maxlength=20)
})
    
# for both stocks (imm and mat) I used von Bertalanffy growth curve from Magnusson 1996 
# to set parameters for minlengths and maxlengths
# for details see 'vbParams.R'
    
    
## set up mature stock
opt$stocks$mat <- within(opt$stock$mat, {
            name <- 'gssmat'
            minage <- 4
            maxage <- 30
            minlength <- 10
            maxlength <- 58
            dl <- 1
            M <- rep(0.15, 27)
            growth <- c(linf='#gss.linf', k='#gss.k',
                        beta='(* 10 #gss.bbin)', 
                        binn=15, recl='#gss.recl'
                        )
            weight <- c(a=weight.alpha, b=weight.beta)
            init.abund <- sprintf('(* %s %s)', c(0,0.02,0.04,0.06,0.08,0.10,0.01,0.001,0,
                                                 rep(0,18)),
                                  c(0,sprintf('#gss.age%s',4:10),rep(0,19)))
            sigma <- c(init.sigma$ms[4], tail(init.sigma$ms, 26), rep(init.sigma$ms[26],3))
            doesmature <- 0
            doesmigrate <- 0
        })


# create gadget skeleton
gm <- gadget.skeleton(time=opt$time, area=opt$area,
                      stocks=opt$stocks, fleets=opt$fleets)


gm@stocks$imm@initialdata$area.factor <- '( * 100 #gss.mult)'
gm@stocks$mat@initialdata$area.factor <- '( * 100 #gss.mult)'

gm@fleets <- list(comm.fleet, igfs.fleet)
gd.list <- list(dir=gd$dir)
Rgadget:::gadget_dir_write(gd.list, gm)

curr.dir <- getwd()
setwd(gd$dir)
callGadget(s=1, log='logfile.txt', ignore.stderr=FALSE)

init.params <- read.gadget.parameters('params.out')

init.params[c('gss.linf', 'gss.k', 'gss.bbin', 'gss.mult',
              grep('age', rownames(init.params), value=T),
              'gss.mat1', 'gss.mat2'),] <-
read.table(text='switch	 value 		lower 	upper 	optimise
gss.Linf	         58	      40     70        0
gss.k	          0.14	       0.06      0.30        1
gss.bbin	         6	   1e-08    100        1
gss.mult	         100	     0.1      100        1
gss.age2	         35	    0.01     150        1
gss.age3	         25	    0.01     120        1
gss.age4	         15	   0.001     100        1
gss.age5	          7	  0.0001     100        1
gss.age6	          7	   1e-05     100        1
gss.age7	          5	   1e-08     100        1
gss.age8	          5	   1e-10     100        1
gss.age9	         25	   1e-12     100        1
gss.age10	         10	   1e-15     100        1
gss.mat1	          50	      10      200        1
gss.mat2	          50	      30      100        1',header=TRUE) 

init.params$switch <- rownames(init.params)

init.params[grepl('rec[0-9]',init.params$switch),'value'] <- 1
init.params[grepl('rec[0-9]',init.params$switch),'upper'] <- 4
init.params[grepl('rec[0-9]',init.params$switch),'lower'] <- 0.001
init.params[grepl('rec[0-9]',init.params$switch),'optimise'] <- 1

init.params['gss.recl',-1] <- c(12, 4, 20,1)

init.params[grepl('alpha',init.params$switch),'value'] <- 0.5
init.params[grepl('alpha',init.params$switch),'upper'] <- 3
init.params[grepl('alpha',init.params$switch),'lower'] <- 0.01
init.params[grepl('alpha',init.params$switch),'optimise'] <- 1

init.params[grepl('l50',init.params$switch),'value'] <- 50
init.params[grepl('l50',init.params$switch),'upper'] <- 100
init.params[grepl('l50',init.params$switch),'lower'] <- 10
init.params[grepl('l50',init.params$switch),'optimise'] <- 1

write.gadget.parameters(init.params,file='params.in')
setwd(curr.dir)
