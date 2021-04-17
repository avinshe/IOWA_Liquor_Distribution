setwd('C:/Users/User/Desktop/HW/Spring2020/DPA/Project/Explore')

#https://www.reddit.com/r/datasets/comments/djucb6/nfl_game_stats_scraped_from_espn/
NFL_dataset <- read.csv("NFL matches.csv")

#https://www.kaggle.com/gsnehaa21/federal-holidays-usa-19662020
USHolidays <- read.csv("usholidays.csv")

#https://towardsdatascience.com/a-tutorial-on-fairness-in-machine-learning-3ff8ba1040cb

#-------------AfterAnalysis-------------

ThreeYearSales <- read.csv("C:/Users/User/Desktop/HW/Spring2020/DPA/Project/Explore/ThreeYearSales.csv")

#------USHolidays analysis--------------
USHolidays$Date <- as.Date(USHolidays$Date,"%m/%d/%Y")
USHolidays$weekday <- weekdays(USHolidays$Date)
USHolidays <- USHolidays[,c(2,3,4)]
USHolidays$Htype <- rep("Federal Holiday",30)
View(USHolidays)

rep("Match Day",2)

#-------NFL analysis------------------
names(NFL_dataset)

NFL_dataset <- NFL_dataset[,c(1,2,3)]
NFL_dataset <- unique(NFL_dataset)

NFL_dataset$date <- as.Date(NFL_dataset$date,"%m/%d/%Y")
NFL_dataset <- NFL_dataset[,c(1,2,4)]

NFL_dataset <- NFL_dataset[NFL_dataset$date > as.Date("2017-01-01"),]
NFL_dataset <- NFL_dataset[NFL_dataset$date < as.Date("2020-01-01"),]

NFL_dates <- unique(NFL_dataset$date)
NFL_dates <- as.data.frame(NFL_dates)
NFL_dates$holiday <- rep("MatchDay",136)
NFL_dates$weekday <- weekdays(NFL_dates$NFL_dates)
NFL_dates$Htype <- rep("Match Day",136)

View(NFL_dates)
View(NFL_dataset)

names(USHolidays)
colnames(NFL_dates) <- names(USHolidays)

#------EventsAnalysis---------------

Events_dataset <- rbind(USHolidays,NFL_dates)
sapply(Events_dataset,class)
Events_dataset$Holiday <- as.character(Events_dataset$Holiday)


for (i in 1:nrow(Events_dataset)){
  if(grepl('2017-02-05',Events_dataset$Date[i])){
    Events_dataset[i,2] <- "SuperBowl Day"}
  if(grepl('2018-02-04',Events_dataset$Date[i])){
    Events_dataset[i,2] <- "SuperBowl Day"}
  if(grepl('2019-02-03',Events_dataset$Date[i])){
    Events_dataset[i,2] <- "SuperBowl Day"}
  }

View(Events_dataset)

Events_dataset$Quarter <- quarters(Events_dataset$Date)
Events_dataset$ones <- c(rep(1))
QbyEvents <- aggregate(Events_dataset$ones, by=list(Events_dataset$Quarter),length)
View(QbyEvents)
QbyEvents$EventDesc <- c("Post Season Playoffs","SuperBowls","Federal Holidays & NFL Season","Holiday Season & NFL Season")
names(QbyEvents) <- c("Quarter","NumberofEvents")

write.csv(QbyEvents,"QbyEvents.csv")

#--------QuarterWiseSales-------------

View(ThreeYearSales)
QuarterSales <- aggregate(ThreeYearSales$Sales,by=list(ThreeYearSales$Quarter),sum)
View(QuarterSales)
write.csv(QuarterSales,"QuarterSales.csv")


#---------BeforeMerge---------

View(Events_dataset)
MatchDays <- Events_dataset[Events_dataset$Htype == 'Match Day',]
View(MatchDays)
MatchDays$Month <- months(MatchDays$Date)

NumofEvents <- aggregate(MatchDays$Date,by=list(MatchDays$Month,MatchDays$weekday),length)
NumofEvents1 <- aggregate(MatchDays$Date,by=list(MatchDays$Month),length)
NumofEvents$wkwn <- c(rep(NA,16))


for (i in 1:nrow(NumofEvents)){
  if(grepl('Monday',NumofEvents$Group.2[i])){
    NumofEvents[i,4] <- "Weekday"}
  if(grepl('Thursday',NumofEvents$Group.2[i])){
    NumofEvents[i,4] <- "Weekday"}
  if(grepl('Saturday',NumofEvents$Group.2[i])){
    NumofEvents[i,4] <- "Weekend"}
  if(grepl('Sunday',NumofEvents$Group.2[i])){
    NumofEvents[i,4] <- "Weekend"}
}

wkwnagg <- aggregate(NumofEvents$x,by=list(NumofEvents$Group.1,NumofEvents$wkwn),sum)
View(wkwnagg)

write.csv(wkwnagg,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/Explore/wkwnagg.csv")

nSepweekdaypct <- sum(NumofEvents[NumofEvents$wkwn=='Weekday'&NumofEvents$Group.1=='September',][3])/sum(NumofEvents[NumofEvents$Group.1=='September',][3])
nOctweekdaypct <- sum(NumofEvents[NumofEvents$wkwn=='Weekday'&NumofEvents$Group.1=='October',][3])/sum(NumofEvents[NumofEvents$Group.1=='October',][3])
nNovweekdaypct <- sum(NumofEvents[NumofEvents$wkwn=='Weekday'&NumofEvents$Group.1=='November',][3])/sum(NumofEvents[NumofEvents$Group.1=='November',][3])
nDecweekdaypct <- sum(NumofEvents[NumofEvents$wkwn=='Weekday'&NumofEvents$Group.1=='December',][3])/sum(NumofEvents[NumofEvents$Group.1=='December',][3])
nJanweekdaypct <- 0
nFebweekdaypct <- 0

nSepweekendpct <- sum(NumofEvents[NumofEvents$wkwn=='Weekend'&NumofEvents$Group.1=='September',][3])/sum(NumofEvents[NumofEvents$Group.1=='September',][3])
nSepweekdaypct <- sum(NumofEvents[NumofEvents$wkwn=='Weekday'&NumofEvents$Group.1=='September',][3])/sum(NumofEvents[NumofEvents$Group.1=='September',][3])
nOctweekdaypct <- sum(NumofEvents[NumofEvents$wkwn=='Weekday'&NumofEvents$Group.1=='October',][3])/sum(NumofEvents[NumofEvents$Group.1=='October',][3])
nNovweekdaypct <- sum(NumofEvents[NumofEvents$wkwn=='Weekday'&NumofEvents$Group.1=='November',][3])/sum(NumofEvents[NumofEvents$Group.1=='November',][3])
nDecweekdaypct <- sum(NumofEvents[NumofEvents$wkwn=='Weekday'&NumofEvents$Group.1=='December',][3])/sum(NumofEvents[NumofEvents$Group.1=='December',][3])

View(nSepweekdaypct)

View(NumofEvents)

plot <- ggplot(data=NumofEvents, aes(c(x=NumofEvents$Group.1,NumofEvents$Group.2), y=NumofEvents$x)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

plot

write.csv(NumofEvents,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/Explore/numofevents.csv")

distmonths <- data.frame(unique(MatchDays$Month))

colnames(NumofEvents) <- c("month","NumofEvents")
colnames(meanSales) <- c("month","Salesmean")
colnames(distmonths) <- c("month")

M1<-merge(distmonths,NumofEvents,by=c("month"))

View(M1)

#---------Merge------------

merged <- merge(ThreeYearSales, Events_dataset, by=c("Date"))
sum(merged$Sales)/sum(ThreeYearSales$Sales)*100



View(merged)

sampleDays <- ThreeYearSales[sample(nrow(ThreeYearSales),87),]
View(sampleDays)

sum(sampleDays$Sales)/sum(ThreeYearSales$Sales)*100

merged$month <- months(merged$Date)

MatchDayDataset <- merged[merged$Htype == 'Match Day',]
View(MatchDayDataset)

NumofEvents <- aggregate(MatchDayDataset$Sales,by=list(MatchDayDataset$month),length)
meanSales <- aggregate(MatchDayDataset$Sales,by=list(MatchDayDataset$month),mean)

distmonths <- data.frame(unique(MatchDayDataset$month))

colnames(NumofEvents) <- c("month","NumofEvents")
colnames(meanSales) <- c("month","Salesmean")
colnames(distmonths) <- c("month")

M1<-merge(distmonths,NumofEvents,by=c("month"))
M2<-merge(distmonths,meanSales,by=c("month"))
M3<-merge(M1,M2,by=c("month"))

show(M3)
plot(M3$NumofEvents,M3$Salesmean)
c_pear <- cor(M3$NumofEvents, M3$Salesmean, method = 'pearson')
c_pear

#----------SalesOnEvents----------------------

names(my_data)
ThreeYearSales <- my_data[,c(3,23)]
sapply(ThreeYearSales,class)

ThreeYearSales$Date <- as.Date(ThreeYearSales$Date)
ThreeYearSales <- ThreeYearSales[ThreeYearSales$Date > as.Date("2017-01-01"),]
ThreeYearSales <- ThreeYearSales[ThreeYearSales$Date < as.Date("2020-01-01"),]
ThreeYearSales <- aggregate(ThreeYearSales$Sale..Dollars.,by=list(ThreeYearSales$Date),sum)
colnames(ThreeYearSales) <- c("Date","Sales")
View(ThreeYearSales)
head(ThreeYearSales)

#---------JanuarySales----------

ThreeYearSales$Date <- as.Date(ThreeYearSales$Date)
ThreeYearSales$month <- months(ThreeYearSales$Date)
ThreeYearSales$Year <- as.numeric(format(ThreeYearSales$Date,"%Y"))
ThreeYearSales$MoNum <- as.numeric(format(ThreeYearSales$Date,"%m"))
View(ThreeYearSales)

ThreeYearSales <- ThreeYearSales[ThreeYearSales$Weekday %in% c('Monday','Tuesday','Wednesday','Thursday'),]

mindate <- aggregate(ThreeYearSales$Date,by=list(ThreeYearSales$Year,ThreeYearSales$MoNum),min)
MonthSplit <- aggregate(ThreeYearSales$Sales,by=list(ThreeYearSales$Year,ThreeYearSales$MoNum,ThreeYearSales$month),sum)
MonthSplit <- MonthSplit[order(MonthSplit$Group.1,MonthSplit$Group.2),]
write.csv(merged,"MonthSplitNoFri.csv")
merged <- merge(mindate,MonthSplit,by=c('Group.1','Group.2'))
merged <- merged[order(merged$Group.1,merged$Group.2),]
View(MonthSplit)
View(mindate)
View(merged)


#---------SuperbowlWeekSales----------------

Sbwk <- ThreeYearSales$Sales
nxtwk <- ThreeYearSales$Sales

sum(Sbwk) - sum(nxtwk)

View(ThreeYearSales)

write.csv(ThreeYearSales,"C:/Users/User/Desktop/HW/Spring2020/DPA/Project/Explore/ThreeYearSales.csv")


#-----------DECSales--------------

View(ThreeYearSales)
DecSales<- ThreeYearSales[as.numeric(format(ThreeYearSales$Date,"%m"))==12,]
OthSales<- ThreeYearSales[!as.numeric(format(ThreeYearSales$Date,"%m"))==12,]
View(DecSales)
View(OthSales)

sum(DecSales$Sales)/sum(OthSales$Sales)*100

(sum(DecSales$Sales)-sum(OthSales$Sales))/sum(OthSales$Sales)

#------------QuatSales-------------

View(ThreeYearSales)
ThreeYearSales$Date <- as.Date(ThreeYearSales$Date)

Q1 <- ThreeYearSales$Sales[ThreeYearSales$Quarter =='Q1'] 
Q2 <- sample(ThreeYearSales$Sales[ThreeYearSales$Quarter =='Q2'],190) 
Q3 <- sample(ThreeYearSales$Sales[ThreeYearSales$Quarter =='Q3'],190) 
Q4 <- sample(ThreeYearSales$Sales[ThreeYearSales$Quarter =='Q4'],190) 

QSales <- c(Q1,Q2,Q3,Q4)

Quarts <- c(
  rep('Q1', 190)
  , rep('Q2', 190)
  , rep('Q3', 190)
  , rep('Q4', 190)
)

anovadf <- data.frame(Quarts,QSales)

fit <- aov(anovadf$QSales ~ anovadf$Quarts)
summary(fit)

N = nrow(anovadf)
df1 = 4 - 1
df2 = N - 4
df2
criticalValue = qf(p = 1 - .05
                   , df1 = df1
                   , df2 = df2
)

criticalValue

library(ggplot2)

plot <- ggplot(data=anovadf, aes(x=anovadf$Quarts, y=anovadf$QSales)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

plot


ifelse(F >= criticalValue, "Reject H0", "Fail to reject H0")

#------------WeekdaySales-------------

View(ThreeYearSales)
ThreeYearSales$Date <- as.Date(ThreeYearSales$Date)
ThreeYearSales$Weekday <- weekdays(ThreeYearSales$Date)

mon <- ThreeYearSales$Sales[ThreeYearSales$Weekday =='Monday'] 
tue <- sample(ThreeYearSales$Sales[ThreeYearSales$Weekday =='Tuesday'],149) 
wed <- sample(ThreeYearSales$Sales[ThreeYearSales$Weekday =='Wednesday'],149) 
thu <- sample(ThreeYearSales$Sales[ThreeYearSales$Weekday =='Thursday'],149) 
fri <- sample(ThreeYearSales$Sales[ThreeYearSales$Weekday =='Friday'],149)


WSales <- c(mon,tue,wed,thu,fri)

Wdays <- c(
  rep('mon', 149)
  , rep('tue', 149)
  , rep('wed', 149)
  , rep('thu', 149)
  , rep('fri', 149)
)

anovadf <- data.frame(Wdays,WSales)

fit <- aov(anovadf$WSales ~ anovadf$Wdays)
summary(fit)

N = nrow(anovadf)
df1 = 4 - 1
df2 = N - 4
df2
criticalValue = qf(p = 1 - .05
                   , df1 = df1
                   , df2 = df2
)

library(ggplot2)

plot <- ggplot(data=anovadf, aes(x=anovadf$Wdays, y=anovadf$WSales)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

plot

criticalValue

ifelse(F >= criticalValue, "Reject H0", "Fail to reject H0")

#-----------4thQuarterSales--------------


ThreeYearSales$Quarter <- quarters(ThreeYearSales$Date)

View(ThreeYearSales)
ThreeYearSales$Date <- as.Date(ThreeYearSales$Date)
FourQ <- ThreeYearSales[as.numeric(format(ThreeYearSales$Date,"%m"))>9,]
FirstQ <- ThreeYearSales[as.numeric(format(ThreeYearSales$Date,"%m"))<4,]

View(FourQ)

sum(FourQ$Sales)/sum(ThreeYearSales$Sales)*100
sum(FirstQ$Sales)/sum(ThreeYearSales$Sales)*100

(sum(FourQ$Sales)-sum(FirstQ$Sales))/sum(FirstQ$Sales)*100

#-----------WeekdaySales---------------

ThreeYearSales$Date <- as.Date(ThreeYearSales$Date)
View(ThreeYearSales)
WeekdaySales <- aggregate(ThreeYearSales$Sales,by=list(ThreeYearSales$Weekday),sum)

write.csv(WeekdaySales,"WeekdaySales.csv")

#-----FUNFACTS------------
#--------MLK's Birthday-------------
#January 15, 1929
Y <- 2019
dates <- c()
for (i in 1:500){
  SDate <- as.Date(paste(Y+i,"-01-01",sep=""))
  EDate <- as.Date(paste(Y+i,"-01-31",sep=""))
  dates <- c(dates,which(weekdays(seq(from = SDate, to = EDate, by = 'days'))=="Monday")[3])
}
which(dates == 15)
View(dates)

#--------George Washington's birthday-------------
#February 22, 1732

unique(which(weekdays(seq(from = as.Date("2000-01-01"), 
                          to = as.Date("2099-12-31"), 
                          by = 'days'))
             =="Monday")
       == 22) #NONE/FALSE

#------------------------------------------------


summary(my_data$State.Bottle.Cost)
