*** these are various files for initializing an mfdb database for greater silver smelt. All of these files push data into your local mareframe database in one way or another ***

*initdb.R
*********
This is Bjarki's original code from which I have modified pretty much everything else in this folder. For another original copy of this, see <www.github.com/bthe/gadget-models/initdb>

*gssInitdb.R
************
This is the main initialization file similar to initdb.R, but I have sourced most of the code instead of having a lengthy single file.

*gsscommercialCatchSamples.R
****************************
Sets up the length distribution and age-length frequencies for commercial catch sea sampling of greater silver smelt.

*igsSpringSurvey.R
******************
Sets up the length distribution and age-length frequencies for spring groundfish survey greater silver smelt samples.

*igsAutumSurvey.R
******************
Sets up the length distribution and age-length frequencies for autumn groundfish survey greater silver smelt samples.

*gssLandedCatch.R
*****************
Initial setup of landed catch data.

*gssLandedCatchSeaSamplingProps.R
*********************************
Sets up the landed catch data and assigns a gridcell to where fish were caught based on the commercial catch samples from gsscommercialCatchSamples.R. Note: this method has some problems: 1) proportions of where fish were caught are based on the sea sampling data (where fishermen send in 100-200 fish from their catch), thus this is more reflective on where fishing effort occurred rather than where fish were caught 2) Many of the caught sea samples did not match up with the landedcatch data foundin the file. I had to have a pretty ugly hack-around and assigned gridcells based on what proportion of sea samples were taken from where in a given year. It's probably better to use Logbooks.

*gssLandedCatchLogbooks.R
*************************

computeAreasSeaSampling.R
*************************
Computes the proportion of sea samples that came from each statistical gridcell for each year and sampling type. For use in assigning gridcells in gssLandedCatchSeaSamplingProps.R

computeAreasLogbooks.R
**********************
Computes the proportion of landed catch that came from each statistical gridcell for each year and sampling type based on Logbooks information. For use in assigning gridcells in gssLandedCatchLogbooks.R

*commercialCatchLengthDist.R
****************************
Deprecated file for calculating length distributions. This is now embedded in the gsscommercialCatchSamples.R. I don't think you've ever even manipulated this file and can probably delete it.

