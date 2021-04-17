setwd('C:/Users/User/Desktop/HW/Spring2020/DPA/Project')
my_data <- read.csv("Iowa_Liquor_Sales.csv",skip=2000000, nrows=1000000, header = TRUE)
my_data <- read.csv("Iowa_Liquor_Sales.csv")
my_data <- read.csv("new_liquor_clean.csv")
master <- read.csv("Master.csv")

head(my_data)

my_data[my_data==""] <- NA

sapply(my_data, class)

my_data$Date <- as.Date(my_data$Date, "%m/%d/%Y")
my_data$Year<- format(as.Date(my_data$Date, "%Y"),"%Y")
my_data$mon<- format(as.Date(my_data$Date, "%m"),"%m")

my_data$YearMon<- paste(format(as.Date(my_data$Date, "%Y"),"%Y"),format(as.Date(my_data$Date, "%m"),"%m"))

head(my_data)

my_data$YearMon <- format(as.Date(my_data$YearMon, "%m/%Y"),"%m/%Y")



totYearMon <- unique(my_data$YearMon)
str_squish(totYearMon)

totYearMon<-sort(totYearMon)
rank <- rep(1:97)

YearMonRank<-as.data.frame(totYearMon,rank)
YearMonRank$rank <- rep(1:97)

View(YearMonRank)
colnames(YearMonRank) <- c("YearMon","rank")
#---CLEANING NUMERIC DATA-----------------------------------------

sapply(my_data, anyNA)

summary(my_data$State.Bottle.Cost)
summary(my_data$State.Bottle.Retail)
summary(my_data$Sale..Dollars.)

Zcost <- my_data[(my_data$State.Bottle.Cost == 0 | my_data$State.Bottle.Retail == 0 | is.na(my_data$State.Bottle.Cost) | is.na(my_data$State.Bottle.Retail)),]
Zcost <- merge(Zcost, YearMonRank, by=c("YearMon"))


MedcostPerItem <- aggregate(my_data$State.Bottle.Cost, by=list(my_data$YearMon,my_data$Item.Number,my_data$Bottle.Volume..ml.), FUN=median, na.rm=T)
View(MedcostPerItem)
colnames(MedcostPerItem) <- c("YearMon","Item.Number","Bottle.Volume..ml.","State.Bottle.Cost")
MedcostPerItem <- merge(MedcostPerItem, YearMonRank, by=c("YearMon"))

write.csv(MedcostPerItem,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/datewiseCostMed.csv")

Zcost$Year<- format(as.Date(Zcost$Date, "%Y"),"%Y")

ZcostCorrected <- merge(Zcost, MedcostPerItem, by=c("Year","Item.Number","Bottle.Volume..ml."))

MedRetPerItem <- aggregate(my_data$State.Bottle.Retail, by=list(my_data$YearMon,my_data$Item.Number,my_data$Bottle.Volume..ml.), FUN=median, na.rm=T)
colnames(MedRetPerItem) <- c("YearMon","Item.Number","Bottle.Volume..ml.","State.Bottle.Retail")
MedRetPerItem <- merge(MedRetPerItem, YearMonRank, by=c("YearMon"))


ZcostCorrected <- merge(ZcostCorrected, MedRetPerItem, by=c("Year","Item.Number","Bottle.Volume..ml."))
ZcostCorrected$Sale..Dollars. <- ZcostCorrected$State.Bottle.Retail.y*ZcostCorrected$Bottles.Sold
ZcostCorrected$State.Bottle.Cost.x <- ZcostCorrected$State.Bottle.Cost.y
ZcostCorrected$State.Bottle.Retail.x <- ZcostCorrected$State.Bottle.Retail.y

my_data <- my_data[!my_data$State.Bottle.Cost == 0,]

keep <- names(ZcostCorrected)
keep <- keep[1:length(my_data)]
keep[21] <- "State.Bottle.Retail"
keep[20] <- "State.Bottle.Cost"
colnames(ZcostCorrected) <- keep

ZcostCorrected <- ZcostCorrected[keep]

my_data <- rbind(my_data,ZcostCorrected)

write.csv(my_data,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/Cleaned.csv")

#-------------------County-wise bottles-----------------------------

Countywisebottle <- aggregate(my_data$Bottle.Volume..ml., by=list(my_data$County,my_data$Bottle.Volume..ml.), FUN=length)

write.csv(Countywisebottle,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/CountyWiseBottle.csv")

#------------------------------------------------

for(i in 1:nrow(Zcost)){
  print(i)
  item <- Zcost[i,17]
  botvol <- Zcost[i,20]
  rank <- Zcost[i,28]
  #----------Cost---------------
  toImpute <- MedcostPerItem[MedcostPerItem$Item.Number == item & MedcostPerItem$Bottle.Volume..ml. == botvol & MedcostPerItem$rank == as.numeric(rank)-1,]
  toImpute1 <- MedcostPerItem[MedcostPerItem$Item.Number == item & MedcostPerItem$Bottle.Volume..ml. == botvol & MedcostPerItem$rank == as.numeric(rank)+1,]
  if (nrow(toImpute)!=0 & nrow(toImpute1)!=0){
    if (toImpute$State.Bottle.Cost==toImpute1$State.Bottle.Cost){
      Zcost[i,21]<-toImpute$State.Bottle.Cost  
    }
  }
  #-------Retail---------------
  toImpute <- MedRetPerItem[MedRetPerItem$Item.Number == item & MedRetPerItem$Bottle.Volume..ml. == botvol & MedRetPerItem$rank == as.numeric(rank)-1,]
  toImpute1 <- MedRetPerItem[MedRetPerItem$Item.Number == item & MedRetPerItem$Bottle.Volume..ml. == botvol & MedRetPerItem$rank == as.numeric(rank)+1,]
  if (nrow(toImpute)!=0 & nrow(toImpute1)!=0){
    if (toImpute$State.Bottle.Retail==toImpute1$State.Bottle.Retail){
      Zcost[i,22]<-toImpute$State.Bottle.Retail  
    }
  }
}

Zcost$Sale..Dollars. <- Zcost$State.Bottle.Retail*Zcost$Bottles.Sold

nrow(toImpute)

Zcost[1,21]

View(rank)
nrow(toImpute1)

Zcost[1,20]

as.numeric(Year)+1

View(item)

Zcost[3094,16]

#------------------------------------------------

callength(unique(master$Store.Number)) - 2317

names(master)

head(as.Date(my_data$Date))

head(my_data$Date)

dl <- levels(my_data$Date)
max(as.Date(dl, "%m/%d/%Y"))
min(as.Date(dl, "%m/%d/%Y"))



as.Date(as.character(dl))
summary(my_data$Date)

master_data <- my_data[3:10]

master_data <- master_data[!duplicated(master_data), ]

master_data <- unique(master_data)

write.csv(my_data,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/Cleaned.csv")

head(master_data)

names(my_data)

add <- data.frame(my_data$Address,is.na(my_data$Store.Location))
add <- subset(add, add$is.na.my_data.Store.Location. == TRUE)

length(unique(master_data$Store.Number))

length(unique(add$my_data.Address))

master_data <- na.omit(master_data)

max(my_data$Date)
