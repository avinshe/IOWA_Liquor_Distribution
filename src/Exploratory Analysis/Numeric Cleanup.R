#--------Import data and convert all nulls and blanks to NA-------
setwd('C:/Users/User/Desktop/HW/Spring2020/DPA/Project')
IOWA_dataset <- read.csv("IOWA_LIQUOR_CATOR_CLEANED_MERGED_DATA.csv")
IOWA_dataset <- IOWA_dataset
IOWA_dataset[IOWA_dataset==""] <- NA
sapply(IOWA_dataset, anyNA)
Initial_rowcount <- nrow(IOWA_dataset)
summary(IOWA_dataset$State.Bottle.Cost)

#-----------NumericValidations-----------
names(IOWA_dataset)
SaleCheck <- which((as.numeric(IOWA_dataset$Bottles.Sold*IOWA_dataset$State.Bottle.Retail) == as.numeric(IOWA_dataset$Sale..Dollars.)))
notSaleCheck <- which((as.numeric(IOWA_dataset$Bottles.Sold*IOWA_dataset$State.Bottle.Retail) != as.numeric(IOWA_dataset$Sale..Dollars.)))
length(notSaleCheck)/nrow(IOWA_dataset)
nrow(IOWA_dataset) - (length(SaleCheck)+length(notSaleCheck))

ProfitMargin <- (IOWA_dataset$State.Bottle.Retail-IOWA_dataset$State.Bottle.Cost)/IOWA_dataset$State.Bottle.Retail
summary(ProfitMargin)
outliers <- boxplot.stats(ProfitMargin)$out

VolSold <- which((IOWA_dataset$Bottles.Sold*IOWA_dataset$Bottle.Volume..ml.)/1000 == IOWA_dataset$Volume.Sold..Litres.)
length(VolSold)
notVolSold <- which(!(IOWA_dataset$Bottles.Sold*IOWA_dataset$Bottle.Volume..ml.)/1000 == IOWA_dataset$Volume.Sold..Litres.)
length(notVolSold)/nrow(IOWA_dataset)
nrow(IOWA_dataset) - (length(VolSold)+length(notVolSold))

0.2/100*nrow(IOWA_dataset)

sapply(IOWA_dataset,class)
numVars <- c('Bottle.Volume..ml.','State.Bottle.Cost','State.Bottle.Retail','Bottles.Sold','Sale..Dollars.','Volume.Sold..Litres.')

IOWA_dataset <- na.omit(IOWA_dataset)

library(corrplot)
correlations = cor(IOWA_dataset[,numVars])
corrplot(correlations)

sapply(IOWA_dataset,anyNA)

#------------BottleSizeAnalysis-----------

BottleSize <- aggregate(IOWA_dataset$Bottles.Sold,by=list(IOWA_dataset$Bottle.Volume..ml.),length)
View(BottleSize)
colnames(BottleSize) <- c("BottleVolume","NumberOfInvoices")
write.csv(BottleSize,"Bottlesize.csv")
sapply(BottleSize,class)
for (i in 1:nrow(BottleSize)){
  if (BottleSize$NumberOfInvoices[i] < 700000){
    BottleSize$BottleVolume[i] <- "Other"
  }
}

#------------OUTLIERS----------------------
SubsetOfBottles <- IOWA_dataset[IOWA_dataset$Bottle.Volume..ml. %in% c(750,1750,1000,375,500),]
nrow(SubsetOfBottles)/nrow(IOWA_dataset)*100

SB750 <- IOWA_dataset[IOWA_dataset$Bottle.Volume..ml. == 750,]
sapply(SB750,class)
length(unique(SB750$Item.Number))
outlier_values_750 <- boxplot.stats(as.numeric(SB750$State.Bottle.Cost)~as.numberic(SB750$Item.Number))$out

mod <- lm(IOWA_dataset$State.Bottle.Cost ~ IOWA_dataset$State.Bottle.Cost+IOWA_dataset$Bottle.Volume..ml.)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")


boxplot(subsetofcost$State.Bottle.Cost~subsetofcost$Bottle.Volume..ml.,data=subsetofcost, main="Cost vs BottleSize", col = "green",
        xlab="Bottle Volume", ylab="Cost per Bottle")

#------Make a list of year-mon and it's ranking-----------------
IOWA_dataset$Date <- as.Date(IOWA_dataset$Date, "%m/%d/%Y")
IOWA_dataset$YearMon<- paste(format(as.Date(IOWA_dataset$Date, "%Y"),"%Y"),format(as.Date(IOWA_dataset$Date,"%m"),"%m"))
head(IOWA_dataset)

totYearMon <- unique(IOWA_dataset$YearMon)
YearMonRank<-as.data.frame(totYearMon)
YearMonRank$rank <- rep(1:97)
colnames(YearMonRank) <- c("YearMon","rank")

#-------Extract Zero and null numeric columns------------------
ZeroAndNulls <- IOWA_dataset[(IOWA_dataset$State.Bottle.Cost == 0 | IOWA_dataset$State.Bottle.Retail == 0 | is.na(IOWA_dataset$State.Bottle.Cost) | is.na(IOWA_dataset$State.Bottle.Retail)),]
ZeroAndNulls <- merge(ZeroAndNulls, YearMonRank, by=c("YearMon"))
MedcostPerItem <- aggregate(IOWA_dataset$State.Bottle.Cost, by=list(IOWA_dataset$YearMon,IOWA_dataset$Item.Number,IOWA_dataset$Bottle.Volume..ml.), FUN=median, na.rm=T)
colnames(MedcostPerItem) <- c("YearMon","Item.Number","Bottle.Volume..ml.","State.Bottle.Cost")
MedcostPerItem <- merge(MedcostPerItem, YearMonRank, by=c("YearMon"))
MedRetPerItem <- aggregate(IOWA_dataset$State.Bottle.Retail, by=list(IOWA_dataset$YearMon,IOWA_dataset$Item.Number,IOWA_dataset$Bottle.Volume..ml.), FUN=median, na.rm=T)
colnames(MedRetPerItem) <- c("YearMon","Item.Number","Bottle.Volume..ml.","State.Bottle.Retail")
MedRetPerItem <- merge(MedRetPerItem, YearMonRank, by=c("YearMon"))

#-------Impute Cost, Retail and Sale--------------------------
names(ZeroAndNulls)

for(i in 1:nrow(ZeroAndNulls)){
  print(i)
  item <- ZeroAndNulls[i,16]
  botvol <- ZeroAndNulls[i,19]
  rank <- ZeroAndNulls[i,27]
  #----------CostImputation---------------
  toImpute <- MedcostPerItem[MedcostPerItem$Item.Number == item & MedcostPerItem$Bottle.Volume..ml. == botvol & MedcostPerItem$rank == as.numeric(rank)-1,]
  toImpute1 <- MedcostPerItem[MedcostPerItem$Item.Number == item & MedcostPerItem$Bottle.Volume..ml. == botvol & MedcostPerItem$rank == as.numeric(rank)+1,]
  if (nrow(toImpute)!=0 & nrow(toImpute1)!=0){
    if (toImpute$State.Bottle.Cost==toImpute1$State.Bottle.Cost){
      ZeroAndNulls[i,20]<-toImpute$State.Bottle.Cost  
    }
  }
  toImpute <- toImpute[0,]
  toImpute1 <- toImpute1[0,]
  #-------RetailImputation---------------
  toImpute <- MedRetPerItem[MedRetPerItem$Item.Number == item & MedRetPerItem$Bottle.Volume..ml. == botvol & MedRetPerItem$rank == as.numeric(rank)-1,]
  toImpute1 <- MedRetPerItem[MedRetPerItem$Item.Number == item & MedRetPerItem$Bottle.Volume..ml. == botvol & MedRetPerItem$rank == as.numeric(rank)+1,]
  if (nrow(toImpute)!=0 & nrow(toImpute1)!=0){
    if (toImpute$State.Bottle.Retail==toImpute1$State.Bottle.Retail){
      ZeroAndNulls[i,21]<-toImpute$State.Bottle.Retail  
    }
  }
  toImpute <- toImpute[0,]
  toImpute1 <- toImpute1[0,]
}
  #----------SaleImputation-------------
ZeroAndNulls$Sale..Dollars. <- ZeroAndNulls$State.Bottle.Retail*ZeroAndNulls$Bottles.Sold

  #----------CheckforZerosAndNulls------
RowsBeforeNullDrop <- nrow(ZeroAndNulls)
ZeroAndNulls <- na.omit(ZeroAndNulls)
RowsAfterNullDrop <- nrow(ZeroAndNulls)

ZeroAndNulls <- ZeroAndNulls[!(ZeroAndNulls$State.Bottle.Cost == 0),]
RowsAfterZeroDrop <- nrow(ZeroAndNulls)

RowsBeforeNullDrop - RowsAfterNullDrop
RowsAfterNullDrop - RowsAfterZeroDrop
#----------------UpdateIOWADataset-----------
IOWA_dataset <- IOWA_dataset[!(IOWA_dataset$State.Bottle.Cost == 0 | IOWA_dataset$State.Bottle.Retail == 0 | is.na(IOWA_dataset$State.Bottle.Cost) | is.na(IOWA_dataset$State.Bottle.Retail)),]
IOWA_dataset <- IOWA_dataset[1:25]
ZeroAndNulls <- ZeroAndNulls[2:26]
names(IOWA_dataset)
names(ZeroAndNulls)



IOWA_dataset <- rbind(IOWA_dataset,ZeroAndNulls)
View(ZeroAndNulls)

backup <- IOWA_dataset

IOWA_dataset <- na.omit(IOWA_dataset)

sapply(ZeroAndNulls, anyNA)
sapply(IOWA_dataset, anyNA)
Final_rowcount <- nrow(IOWA_dataset)

Initial_rowcount - Final_rowcount
#1420

write.csv(IOWA_dataset,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/Cleaned.csv")
#----------------------------------------

alctype <- unique(IOWA_dataset$AchoholTypes)
View(alctype)

write.csv(ZeroAndNulls,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/trail.csv")

trail <- read.csv("trail.csv")

nrow(IOWA_dataset)

summary(IOWA_dataset$State.Bottle.Retail)
