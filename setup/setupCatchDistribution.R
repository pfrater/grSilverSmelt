minage <- Rgadget:::getMinage(gm)
maxage <- Rgadget:::getMaxage(gm)
maxlength <- 58 #max(Rgadget:::getLengthgroups(gm))

## Query length data to create IGFS catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'IGFS',
    species = defaults$species,
    length = mfdb_interval("len", seq(1, maxlength, by = 1))),
    defaults))

attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "ldist.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("gssimm", "gssmat")))
rm(aggdata)

## Age IGFS
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'IGFS',
                             age = mfdb_step_interval('age',by=1,from=1,to=30),
                             species=defaults$species,
                             length = mfdb_interval("len", seq(1, maxlength, by = 1))),
                        defaults))

#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])


gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("gssimm", "gssmat")))
rm(aggdata)


## Maturity @3 from IGFS
aggdata <- mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                        list(sampling_type='IGFS',
                                age=mfdb_group(age3=3:20),
                                length = mfdb_step_interval('len', by = 1, from = 1, to = maxlength),              
                                maturity_stage = mfdb_group(gssimm = 1, gssmat = 2:5))))

gadget_dir_write(gd,
                 gadget_likelihood_component("stockdistribution",
                                             name = "matp.igfs",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("igfs"),
                                             stocknames = c("gssimm", "gssmat")))
rm(aggdata)

# Query length data to create bmt catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SEA',
    species = defaults$species,
    gear = c('BMT', 'NPT', 'SHT'),
    length = mfdb_interval("len", seq(1, maxlength, by = 1))),
    defaults))
attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage

gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
                                                 name = "ldist.comm",
                                                 weight = 1,
                                                 data = aggdata[[1]],
                                                 fleetnames = c("comm"),
                                                 stocknames = c("gssimm", "gssmat")))
rm(aggdata)

## Age fleet
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SEA',
                             age = mfdb_step_interval('age',by=1,from=1,to=30),
                             length = mfdb_interval("len", seq(1, maxlength, by = 4))),
                        defaults))
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_dir_write(gd,
                 gadget_likelihood_component("catchdistribution",
                                             name = "aldist.comm",
                                             weight = 1,
                                             data = aggdata[[1]],
                                             fleetnames = c("comm"),
                                             stocknames = c("gssimm", "gssmat")))
rm(aggdata)
