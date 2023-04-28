#####################################################################
# NFI preprocessing script - read data files and set up data frames #
# Generate stats, plots, and filters based on all of the data       #
#####################################################################

# create and open pdf to save graphics
pdf(paste(outpdfpath,allEzplot,sep="/"))

# Read training data
nfidat.all <- na.omit(read.csv(paste(indatpath,indatfil,sep="/"),header=TRUE))

# correct LC class names so that Random Forest doesn't get confused by "-", ".", etc.
nfidat.all[nfidat.all[,cls]=='Shurb Low',cls] <- "Shrub.Low"
nfidat.all[nfidat.all[,cls]=='Shurb Tall',cls] <- "Shrub.Tall"
nfidat.all[nfidat.all[,cls]=='Wetland-M/S',cls] <- "Wetland.MS"
nfidat.all[nfidat.all[,cls]=='Forest-DB',cls] <- "Forest.DB"
nfidat.all[nfidat.all[,cls]=='Forest-EN',cls] <- "Forest.EN"
nfidat.all[nfidat.all[,cls]=='Forest-M',cls] <- "Forest.M"
nfidat.all[nfidat.all[,cls]=='Wetland-Herb',cls] <- "Wetland.Herb"
nfidat.all[nfidat.all[,cls]=='Woodland-W',cls] <- "Woodland.W"

# convert LC labels to factors
nfidat.all[,cls] <- as.factor(nfidat.all[,cls])
nfidat.all[,"Ecozones"] <- as.factor(nfidat.all[,"Ecozones"])

# set up features for analysis
lc.cols <- c("LC_Class","LC_Class2","Ecozones")

tm.feat <- c("BLUE_SYNT","GREEN_SYNT","RED_SYNT","NIR_SYNT","SWIR1_SYNT","SWIR2_SYNT",
             "TCB","TCB_max","TCB_p20","TCB_p5","TCB_p80",
             "TCG","TCG_max","TCG_p20","TCG_p5","TCG_p80",
             "TCW","TCW_max","TCW_p20","TCW_p5","TCW_p80")

text.feat <- c("TCB_asm","TCB_contrast","TCB_corr","TCB_dent","TCB_diss","TCB_dvar","TCB_ent","TCB_idm", "TCB_imcorr1",
               "TCB_imcorr2","TCB_inertia","TCB_maxcorr","TCB_prom","TCB_savg","TCB_sent","TCB_shade","TCB_svar","TCB_var")

additional.feat <- c("DEM_SLOPE","ELEVATION","GREEN_AMPLITUDE","GREEN_AMPLITUDE2",
                "GREEN_AMPLITUDE3","GREEN_COS","GREEN_COS2","GREEN_COS3","GREEN_INTP","GREEN_PHASE","GREEN_PHASE2","GREEN_PHASE3",
                "GREEN_RMSE","GREEN_SIN","GREEN_SIN2","GREEN_SIN3","GREEN_SLP","NIGHT_LIGHTS","NIR_AMPLITUDE","NIR_AMPLITUDE2",
                "NIR_AMPLITUDE3","NIR_COS","NIR_COS2","NIR_COS3","NIR_INTP","NIR_PHASE","NIR_PHASE2","NIR_PHASE3",
                "NIR_RMSE","NIR_SIN","NIR_SIN2","NIR_SIN3","NIR_SLP","POPULATION","RAINFALL","RED_AMPLITUDE",
                "RED_AMPLITUDE2","RED_AMPLITUDE3","RED_COS","RED_COS2","RED_COS3","RED_INTP","RED_PHASE","RED_PHASE2",
                "RED_PHASE3","RED_RMSE","RED_SIN","RED_SIN2","RED_SIN3","RED_SLP","SWIR1_AMPLITUDE","SWIR1_AMPLITUDE2",
                "SWIR1_AMPLITUDE3","SWIR1_COS","SWIR1_COS2","SWIR1_COS3","SWIR1_INTP","SWIR1_PHASE","SWIR1_PHASE2","SWIR1_PHASE3",
                "SWIR1_RMSE","SWIR1_SIN","SWIR1_SIN2","SWIR1_SIN3","SWIR1_SLP","SWIR2_AMPLITUDE","SWIR2_AMPLITUDE2","SWIR2_AMPLITUDE3",
                "SWIR2_COS","SWIR2_COS2","SWIR2_COS3","SWIR2_INTP","SWIR2_PHASE","SWIR2_PHASE2","SWIR2_PHASE3","SWIR2_RMSE",
                "SWIR2_SIN","SWIR2_SIN2","SWIR2_SIN3","SWIR2_SLP","TEMPERATURE","TEMP_AMPLITUDE","TEMP_AMPLITUDE2",
                "TEMP_AMPLITUDE3","TEMP_COS","TEMP_COS2","TEMP_COS3","TEMP_INTP","TEMP_PHASE","TEMP_PHASE2","TEMP_PHASE3",
                "TEMP_RMSE","TEMP_SIN","TEMP_SIN2","TEMP_SIN3","TEMP_SLP","TEMP_SYNT","maxRAINFALL","maxTEMPERATURE",
                "maxTemp","minRAINFALL","minTEMPERATURE","minTemp","precip")

other.feat <- colnames(nfidat.all)[!(colnames(nfidat.all) %in% c(tm.feat,text.feat,additional.feat))]

# extract tasseled cap features
nfidat <- nfidat.all[,c(lc.cols,tm.feat)]

# look at distribution of sites across classes and ecozones
par(mar=c(5,8,3,3))
barplot(table(as.factor(nfidat[,"LC_Class"])),
        horiz=TRUE,
        las=1,
        col='red',
        main="LC_Class")

barplot(table(as.factor(nfidat[,"LC_Class2"])),
        horiz=TRUE,
        las=1,
        col='red',
        main="LC_Class2")

barplot(table(as.factor(nfidat[,"Ecozones"])),
        horiz=TRUE,
        las=1,
        col='red',
        main="Eco_Zones")

# Plot synthetic reflectance bands - July 1 vs class
boxplot(nfidat[,"BLUE_SYNT"]~nfidat[,cls],
        xlab="Class",
        ylab="BLUE_SYNT",
        ylim=c(0,0.4),
        outline=F)

boxplot(nfidat[,"GREEN_SYNT"]~nfidat[,cls],
        xlab="Class",
        ylab="GREEN_SYNT",
        ylim=c(0,0.4),
        outline=F)

boxplot(nfidat[,"RED_SYNT"]~nfidat[,cls],
        xlab="Class",
        ylab="RED_SYNT",
        ylim=c(0,0.4),
        outline=F)

boxplot(nfidat[,"NIR_SYNT"]~nfidat[,cls],
        xlab="Class",
        ylab="NIR_SYNT",
        ylim=c(0,0.6),
        outline=F)

boxplot(nfidat[,"SWIR1_SYNT"]~nfidat[,cls],
        xlab="Class",
        ylab="SWIR1_SYNT",
        ylim=c(0,0.4),
        outline=F)

boxplot(nfidat[,"SWIR2_SYNT"]~nfidat[,cls],
        xlab="Class",
        ylab="SWIR2_SYNT",
        ylim=c(0,0.4),
        outline=F)

# Plot TC Indices - July 1 - by class
boxplot(nfidat[,"TCG"]~nfidat[,cls],
        xlab="Class",
        ylab="TCG",
        outline=F)

boxplot(nfidat[,"TCW"]~nfidat[,cls],
        xlab="Class",
        ylab="TCW",
        outline=F)

boxplot(nfidat[,"TCB"]~nfidat[,cls],
        xlab="Class",
        ylab="TCB",
        outline=F)

# TC phenological Indices
# TCG
boxplot(nfidat[,"TCG_p5"]~nfidat[,cls],
        xlab="Class",
        ylab="TCGp5",
        outline=F)

boxplot(nfidat[,"TCG_p20"]~nfidat[,cls],
        xlab="Class",
        ylab="TCG_p20",
        outline=F)

boxplot(nfidat[,"TCG_p80"]~nfidat[,cls],
        xlab="Class",
        ylab="TCGp_p80",
        outline=F)

boxplot(nfidat[,"TCG_max"]~nfidat[,cls],
        xlab="Class",
        ylab="TCG_max",
        outline=F)

# TCB
boxplot(nfidat[,"TCB_p5"]~nfidat[,cls],
        xlab="Class",
        ylab="TCBp5",
        outline=F)

boxplot(nfidat[,"TCB_p20"]~nfidat[,cls],
        xlab="Class",
        ylab="TCB_p20",
        outline=F)

boxplot(nfidat[,"TCB_p80"]~nfidat[,cls],
        xlab="Class",
        ylab="TCBp_p80",
        outline=F)

boxplot(nfidat[,"TCB_max"]~nfidat[,cls],
        xlab="Class",
        ylab="TCB_max",
        outline=F)

# TCW
boxplot(nfidat[,"TCW_p5"]~nfidat[,cls],
        xlab="Class",
        ylab="TCWp5",
        outline=F)

boxplot(nfidat[,"TCW_p20"]~nfidat[,cls],
        xlab="Class",
        ylab="TCW_p20",
        outline=F)

boxplot(nfidat[,"TCW_p80"]~nfidat[,cls],
        xlab="Class",
        ylab="TCWp_p80",
        outline=F)

boxplot(nfidat[,"TCW_max"]~nfidat[,cls],
        xlab="Class",
        ylab="TCW_max",
        outline=F)

# Random Forest
rf.feat <- c("BLUE_SYNT","GREEN_SYNT","RED_SYNT",
             "NIR_SYNT","SWIR1_SYNT","SWIR2_SYNT",
             "TCB","TCB_max","TCB_p20","TCB_p5","TCB_p80",
             "TCG","TCG_max","TCG_p20","TCG_p5","TCG_p80",
             "TCW","TCW_max","TCW_p20","TCW_p5","TCW_p80")

rf.df <-data.frame(nfidat[,cls],nfidat[,rf.feat])
colnames(rf.df)[1] <- "LC_Class"

# estimate model
ranger.perm <- ranger(LC_Class~.,
                      data=rf.df,
                      num.trees=ntrees,
                      importance='permutation')

# plot variable importance
barplot(sort(ranger.perm$variable.importance),
        horiz=TRUE,
        las=1,
        col='red',
        cex.names=0.65,
        main = paste("Variable Importance - All Ecozones; RF Accuracy =", round(1-ranger.perm$prediction.error,3)))

# Estimate base model w/full set of features
ranger.base <- ranger(LC_Class~.,
                      data=rf.df,
                      num.trees=ntrees,
                      importance='none',
                      probability=TRUE)

# create data frame w/basic results from global random forest
rf.preds <- levels(rf.df[,"LC_Class"])[apply(ranger.base$predictions,1,which.max)]
rf.results <- data.frame(rf.df[,"LC_Class"],rf.preds,ranger.base$predictions)
colnames(rf.results)[1:2] <- c("LC_Class","RF.Pred")

# # estimate margin between labeled class and most probable class
# class.sorted <- t(apply(ranger.base$predictions,1,sort))
# nclass <- length(unique(rf.results[,"LC_Class"]))
# rf.margin <- class.sorted[,nclass]-class.sorted[,nclass-1]

# estimate margin between labeled class and most probable class - has to be a better way to do this
rf.margin <- matrix(NA,nrow=nrow(rf.results),ncol=1)
for (i in 1:nrow(rf.results)) {
  rf.margin[i,] <- max(ranger.base$predictions[i,]) - ranger.base$predictions[i,rf.df[i,"LC_Class"]] 
}

# create a data frame with results
rf.results <- data.frame(rf.results,rf.margin)

# identify misclassificed cases
misclass <- rf.results[,"LC_Class"] != rf.results[,"RF.Pred"]

# look at margins vs misclass
boxplot(rf.margin~misclass)

# look at margins as a function of classes
boxplot(rf.margin~rf.results[,"LC_Class"],
        outline=FALSE)

# filtering - test approach based on LDA/Random forest; inspect sites flagged based on results
rf.results.misclass <- rf.results[misclass,]
marg.ord <- order(rf.results.misclass[,"rf.margin"],decreasing=TRUE)
rf.results.misclass <- rf.results.misclass[marg.ord,]

# select rows where margin exceeds threshold
exceeds.margin <- rf.results.misclass[,'rf.margin'] > marg.thresh

# grab and export worst the "nBadAll cases
bad.pix <- rownames(rf.results.misclass[exceeds.margin,])
export.df <- data.frame(nfidat.all[bad.pix,"lat"], 
                        nfidat.all[bad.pix,"lon"],
                        nfidat.all[bad.pix,"trainYear"],
                        rf.results.misclass[bad.pix,])
colnames(export.df)[1:3] <- c("Lat","Long","Year")
write.csv(export.df,paste(outdatpath,"All Ecozones Bad Cases.csv",sep="/"))

dev.off()
