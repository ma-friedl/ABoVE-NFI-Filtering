# Driver script for NFI filtering
# Define and set values for global parameters
# Source each of the script file

# set working directory
setwd("/Users/Mark/Dropbox (BOSTON UNIVERSITY)/Main/Rwork/ABoVETraining/ABoVE-NFI-Filtering")

# load libraries
library(ranger)

# path for feature data files
indatpath <- "/Users/Mark/Dropbox (BOSTON UNIVERSITY)/Main/Rwork/ABoVETraining/ABoVE-NFI-Filtering/inputCSVs"
indatfil <- 'TCB_texture_size2.csv'

# path for output data files
outdatpath <- "/Users/Mark/Dropbox (BOSTON UNIVERSITY)/Main/Rwork/ABoVETraining/ABoVE-NFI-Filtering/outputCSVs/"

# path for output PDF files
outpdfpath <- "/Users/Mark/Dropbox (BOSTON UNIVERSITY)/Main/Rwork/ABoVETraining/ABoVE-NFI-Filtering/outputPDFs"

# Set global parameter values
cls <- 'LC_Class2'
ntrees <- 500                           # number of trees in random forest
nfeat <- 50                             # number of features to use in RF after feature selection
nBadAll <- 1000                         # number of cases to extract from entire data set
p.min <- 0.05                           # min proportion of each class 
n.max.replace <- 500                    # if number of samples in glass > 500, don't worry about augmenting
#p.max.replace <- (1/nclass)*3          # max proportion is 3x of uniform (not yet implemented)
marg.thresh <- 0.5                      # export cases with margin larger than this value

# filename for plots from pre-processing using all NFI data
allEzplot <- "allnfi-Plots.pdf"

# filename for plots from filtering NFI data for each ecozone
allEcozonesPlot <- "allnfi-Plots.pdf"

# Source script files
source('Rcode/1.NFI-Preprocessing.R')
source('Rcode/2.NFI-Filter.R')
