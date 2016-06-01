## setup catches by fleet for landings
landings <- mfdb_sample_count(mdb, c('age', 'length'),
                c(list(
                    sampling_type='LND',
                    species=defaults$species), defaults))

## make the bottom trawlers fleet
comm.fleet <- Rgadget:::make.gadget.fleet(name='comm', suitability='exponentiall50',
                                            fleet.data=landings[[1]],
                                            stocknames=c('gssimm', 'gssmat'))

# Rgadget:::gadget_dir_write(gd, fleet)


## set up and make igfs landings as fleet
igfs.landings <- data.frame(year=defaults$year, step=1, number=1, area=1)
igfs.fleet <- Rgadget:::make.gadget.fleet(name='igfs', suitability='exponentiall50',
                                            fleet.data=igfs.landings,
                                            stocknames=c('gssimm', 'gssmat'))

# Rgadget:::gadget_dir_write(gd, igfs.fleet)
