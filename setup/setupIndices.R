## IGFS survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(15,20,25))),
    defaults))
# original len values c(5,10,15,20) - receive invalid values

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(25,30,35,40))),
    defaults))
# original len values c(20,25,30,35) - worked well

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(35,40,45,55))),
    defaults))
# original len values c(35,40,55) - worked well


gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.1525",
                                                 weight = 1,
                                                 data = igfs.SI1[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("gssimm","gssmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.2540",
                                                 weight = 1,
                                                 data = igfs.SI2[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("gssimm","gssmat")))

gadget_dir_write(gd, gadget_likelihood_component("surveyindices",
                                                 name = "si.4055",
                                                 weight = 1,
                                                 data = igfs.SI3[[1]],
                                                 fittype = 'fixedslopeloglinearfit',
                                                 slope=1,
                                                 stocknames = c("gssimm","gssmat")))