################  NFI filtering  ###########################
# 1. stratify by ecoregion
# 2. check class distribution 
# 3. Add under-sampled from other ecoregions 
# 3. Feature selection
# 4. Random forest
# 5. Filter/remove sites with low p assigned to class label
############################################################

# PDF for graphics
pdf(paste(outpdfpath,"NFI-Filtering.pdf",sep="/"))

# Get rid of snow & ice - too small
nfidat.all <- subset(nfidat.all,nfidat.all[,cls] != "SnowIce")
nfidat.all[,cls] <- droplevels(nfidat.all[,cls])

# select subset of features to work with
nfidat <- nfidat.all[,c(cls,tm.feat,additional.feat)]

# create list of ecozones
ecozones <- sort(unique(nfidat.all[,'Ecozones']))

# summarize freq in each ecozone
nclass <- length(unique(nfidat.all[,cls]))
classNames <- as.character(sort(unique(nfidat.all[,cls])))
n.class.ez <- data.frame(matrix(NA,length(ecozones),nclass))
rownames(n.class.ez) <- ecozones
colnames(n.class.ez) <- classNames

for (ez in ecozones) {          
  ezdat <- subset(nfidat.all,nfidat.all[,"Ecozones"] == ez)
  n.class.ez[ez,] <- table(ezdat[,cls])
}

# stratify and filter by ecozones
for (ez in ecozones) {          
  # grab data for ecozone
  ezdat <- subset(nfidat,nfidat.all[,"Ecozones"] == ez)
  
  # plot distribution of classes in ecozone in training data
  barplot(table(ezdat[,cls]),
          horiz=TRUE,
          las=1,
          col='red',
          cex.names=0.75,
          main=paste("Original Distribution:",ez))
  
  # number of training samples in ecozone
  nSamps <- nrow(ezdat)
  
  print(paste("Starting n", ez, nSamps))
  
  # flag undersampled classes
  p.samp <- round((n.class.ez[ez,]/nSamps),2)
  underSampClasses <- classNames[(p.samp < p.min) & n.class.ez[ez,] < n.max.replace]
  
  # estimate n samples to replace, but set max to 500
  min.samp <- round(nSamps*p.min)
  if (min.samp > n.max.replace) {
    min.samp <- n.max.replace
  }
  
  # ok go get more samples from larger data set for classes where proportion < 0.05 
  for (cls.sub in underSampClasses) {
    # for each "undersampled" class
    print(paste("adding to", cls.sub))
    
    # get current # samps
    n.current <- nrow(subset(ezdat,ezdat[,cls] == cls.sub))
    
    # random sample all data in class outside of ecozone
    all.cls <- subset(nfidat,nfidat[,cls] == cls.sub & nfidat.all[,"Ecozones"] != ez)
    smpl <- sample(1:nrow(all.cls),size=min.samp-n.current)
    
    # and add to data for ecozone
    ezdat <- rbind(ezdat,all.cls[smpl,])
    # NOTE - THIS MEANS SMALL CLASSES WILL BE BELOW P.MIN 
    # (BECAUSE SIZE OF DATA SET FOR ECOZONE INCREASES)
  }
  
  # ADD code to sub-sample over smapled classes here! 
  
  # quick look at result
  barplot(table(ezdat[,cls]),
          col="red",
          cex.names=0.75,
          horiz=TRUE,
          las=1,
          main=paste("Up-Sampled Distrion",ez))
  
  # start with feature selection
  ranger.perm <- ranger(LC_Class2~.,
                        data=ezdat,
                        num.trees=ntrees,
                        importance='permutation')
  
  # plot top 50 features importance
  totfeat <- ncol(ezdat)-1
  barplot(sort(ranger.perm$variable.importance)[(totfeat-nfeat):totfeat],
          horiz=TRUE,
          las=1,
          col='red',
          cex.names=0.35,
          main = paste("Variable Importance:",ez))
  
  # re-estimate model with top 50 features - prob need to experiment with n features
  ezdat <- ezdat[,c(cls,names(sort(ranger.perm$variable.importance)[(totfeat-nfeat):totfeat]))]
  ranger.base <- ranger(LC_Class2~.,
                        data=ezdat,
                        num.trees=ntrees,
                        importance='none',
                        probability=TRUE)
  
  # create data frame with probabilities
  rf.preds <- colnames(ranger.base$predictions)[apply(ranger.base$predictions,1,which.max)]
  
  # now set up data frame with results
  rf.results <- data.frame(ezdat[,cls],rf.preds,round(ranger.base$predictions,3))
  colnames(rf.results)[1:2] <- c(cls,"RF.Pred")
  rownames(rf.results) <- rownames(ezdat)
  
  # get margin with labeled class
  rf.margin <- matrix(NA,nrow(ranger.base$predictions),ncol=1)
  
  # loop through compute delta btwn max likelihood class and labeled class
  for (i in 1:nrow(ranger.base$predictions)) { 
    rf.margin[i] <- max(ranger.base$predictions[i,]) - ranger.base$predictions[i,as.character(rf.results[i,cls])]
  }
  
  # add margin to data frame with RF results
  rf.results <- data.frame(rf.results,rf.margin)
  
  # remove cases from outside of ecozone
  rf.results <- subset(rf.results,nfidat.all[rownames(rf.results),"Ecozones"] == ez)
  
  # Identify mis-classified cases (according to LC_Class label)
  misclass.pix <- subset(rf.results,rf.results[,cls] != rf.results[,"RF.Pred"])
  boxplot(misclass.pix[,"rf.margin"]~misclass.pix[,cls],
          horizontal=TRUE,las=1,
          xlab="Margin",
          ylab="",
          main=paste("Margin for Misclassified Pixels:",ez))
  
  # identify pixels that are misclassified with large margin
  bad.pix <- subset(misclass.pix,misclass.pix[,"rf.margin"] > marg.thresh)
  
  # now sort based on magnitude of margin
  bad.pix <- bad.pix[order(bad.pix[,"rf.margin"],decreasing = TRUE),]
  
  # create final csv
  ez.bad.pix <- data.frame(nfidat.all[rownames(bad.pix),"poly_id"],
                           nfidat.all[rownames(bad.pix),"trainYear"],
                           nfidat.all[rownames(bad.pix),c("lat","lon")],
                           bad.pix)
  
  # assign names to columns
  colnames(ez.bad.pix)[1] <- "PolygonID"
  colnames(ez.bad.pix)[2] <- "Year"
  
  # plot number of cases labeled s bad in each class
  barplot(table(rf.results[,cls]),
          horiz=TRUE,
          las=1,
          main=paste("Bad Pixels in Each Class in:", ez))
  barplot(table(ez.bad.pix[,cls]),
          horiz=TRUE,
          las=1,
          add=TRUE,
          col="red")
  
  # Create table with classification errors for each class
  bad.pix.tbl <- table(ez.bad.pix$LC_Class2,ez.bad.pix$RF.Pred)
  
  # now plot mis-classifications in each class for "bad" pix
  par(mfrow=c(2,1))
  for (cls.i in rownames(bad.pix.tbl)) {
    print(cls.i)
    barplot(bad.pix.tbl[cls.i,],
            main=paste("Bad Pix in",ez,",",cls.i,"Classified As:"),
            cex.names = 0.5,
            col="blue")
  }
  par(mfrow=c(1,1))
  
  # and write to file
  write.csv(ez.bad.pix,paste(outdatpath,ez," Bad Cases.csv",sep=""),row.names = TRUE)
  
}

dev.off()
