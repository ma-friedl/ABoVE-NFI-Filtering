## README

Repository for ABoVE NFI training data for filtering scripts.

Note these script assume that working directory includes 4 sub-directories

1. Rcode - where R scripts live

2. inputCSVs - where input data files are stored as csv files

3. outputCSVs - where csv files with training sites flagged for inspection will be saved

4. outputPDFs - where output graphics files are saved

Note all of these paths and filenames are defined and can be changed in "0.NFI_Filtering.R"

To run the filtering code:

1. review te "driver" script ("0.NFI_Filtering.R") and edit parameters and paths accordingly; and 

2. Source the driver script, which will run the code.