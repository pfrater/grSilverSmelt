# this computes the length at age of greater silver smelt using von Bertalanffy growth curve
# parameters for vb are take from Magnusson 1996. Greater Silver Smelt in Icelandic Waters
# his variables are fitted to data from MRI research cruises

vb <- function(linf, k, a, t0) { #vector with linf[1], k[2], t0[3]
        length <- linf*(1 - exp(-k*(a-t0)));
        return(length);
}

### greater silver smelt - immature stock (<8 years*)
### Magnusson 1996 state 50% maturity at 8 years for males, 9 years for females
### Johnannessen and Monstad 2003 give 50% maturity at 7 years for males, 6 for females
### using Magnusson since those data are from Icelandic waters
# parameters for males from Magnusson 1996
linf.m <- 51.816
k.m <- 0.0952
t0.m <- -4.337
vb(linf.m, k.m, 1, t0.m) #minimum size for males
# parameters for females from Magnusson 1996
linf.f <- 55.768
k.f <- 0.0875
t0.f <- -4.399
vb(linf.f, k.f, 1, t0.f)
# take the mean between males and females and use that
linf.mn <- mean(linf.m, linf.f)
k.mn <- mean(k.m, k.f)
t0.mn <- mean(t0.m, t0.f)
vb(linf.mn, k.mn, 1, t0.mn)
# I set minimum size to 5 since 20 seemed awfully high (even 0 was 17.5)
# using the means to also get the length at the average age before entering mature stage (7 years)
vb(linf.mn, k.mn, 7, t0.mn)


### greater silver smelt - mature stock
vb(linf.mn, k.mn, 8, t0.mn)

library(mfdb)
mdb <- mfdb('Iceland')
mla <- mfdb_sample_meanlength_stddev(mdb, c('age'),
                                     c(list(sampling_type="IGFS", 
                                            age=1:30), # got     30 from ICES 2013. Report on the workshop on age estimation of deep water species
                                       defaults))
la <- mla[[1]]
plot(mean~age, data=la)
curve(vb(linf.mn, k.mn, x, t0.mn), add=T, col='black', lwd=1.4) # as you can see, not quite there

# define an sse function that we can minimize on
vb.sse <- function(b) {
    linf <- b[1];
    k <- b[2];
    t0 <- b[3];
    length <- linf*(1 - exp(-k*(a-t0)));
    return(length)
}

sse <- function(b) {
    lhat <- vb.sse(b);
    s <- sum((l-lhat)^2);
    return(s);
}

l <- tapply(la$mean, la$age, mean)
a <- as.numeric(names(l))
# performing a nonlinear minimization on parameters to fit this vb curve to data better
vbMin <- nlm(sse, c(51.816, 0.952, -4.337))

# visualize the output
#plot(mean~age, data=la)
linf.min <- vbMin$estimate[1]
k.min <- vbMin$estimate[2]
t0.min <- vbMin$estimate[3]
#curve(vb(linf.min, k.min, x, t0.min), add=T, col='red', lwd=1.4) # as you can see, not quite there

g <- ggplot(la, aes(x=age, y=mean)) + geom_point() +
    stat_function(fun=function (x) vb(linf.min, k.min, x, t0.min)) 
# curve to test parameter bounds    + stat_function(fun=function(x) vb(linf.min, k.min, x, t0.min))
# curve to test parameter bounds    + stat_function(fun=function(x) vb(linf.min, k.min, x, t0.min)) 


