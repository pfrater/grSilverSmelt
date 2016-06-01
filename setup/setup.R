## general setup for gadget model on greater silver smelt (Argentina silus)
library(mfdb)

setwd('/home/pfrater/gadget')

# create a gadget directory and define some defaults to use with queries below
gd <- gadget_directory('grSilverSmelt/gssModel')
file.remove(sprintf('%s/Modelfiles/fleets', gd$dir))
setup.d <- 'grSilverSmelt/setup'
mdb <- mfdb('Iceland')


reitmapping <- read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_yearly,
    year = 1982:2015,
    species = 'GSS')

## Write out areafile and update mainfile with areafile location
gadget_dir_write(gd, gadget_areafile(
    size = mfdb_area_size(mdb, defaults)[[1]],
    temperature = mfdb_temperature(mdb, defaults)[[1]]))

## Write a penalty component to the likelihood file
gadget_dir_write(gd, 
                gadget_likelihood_component("penalty",
                name = "bounds",
                weight = "0.5",
                data = data.frame(
                switch = c("default"),
                power = c(2),
                upperW=10000,
                lowerW=10000,
                stringsAsFactors = FALSE)))

gadget_dir_write(gd, 
                 gadget_likelihood_component("understocking",
                 name = "understocking",
                 weight = "100"))


source(sprintf('%s/setupFleet.R', setup.d))
source(sprintf('%s/setupModel.R', setup.d))
source(sprintf('%s/setupCatchDistribution.R', setup.d))
source(sprintf('%s/setupIndices.R', setup.d))

#file.copy(sprintf('%s/itterfitter.sh', setup.d), gd$dir)
file.copy(sprintf('%s/run.R', setup.d), gd$dir)
file.copy(sprintf('%s/optinfofile', setup.d), gd$dir)

