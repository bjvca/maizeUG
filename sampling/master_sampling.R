### Master file to do the sampling 
### as inputs, it takes a list of villages (in parishes in subcounties in the 5 districts) which was obtained from the OPM
### it then randomly selects parishes, with the probability of a parish to be chosen proprtional to the number of villages within the parish
### we then carried out a census in each village of the selected parishes and randomly selected 11 elegible households.  
### this was done using an ODK app (/home/bjvca/data/projects/digital green/sampling/Maize_samplingv3.xls)
### these lists were than matched according to the sampling frame, with treatment combinations randomized within each village.
### the output is the sampling_list_ID.csv, which has names and sometimes phone numbers of household head and the treatments that should be applied
### we have a similar dataset of women headed households for a smaller experiment on this particular population (femhead_list_ID.csv)
### for the main experiment, we also genetated a list that can be used as replacement (reserve_list_ID.csv)

### note: all villages can be uniquely identified by sc and village or parish and village.

### creates the sample 
source("/home/bjvca/data/projects/digital green/sampling/sampler.R")
### matches names to sample list for namayingo
source("/home/bjvca/data/projects/digital green/sampling/listing_namayingo.R")
### matches names to sample list for all other districts
source("/home/bjvca/data/projects/digital green/sampling/listing_match.R")
