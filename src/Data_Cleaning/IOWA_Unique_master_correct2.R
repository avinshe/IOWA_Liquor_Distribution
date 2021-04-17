library(readr)
df_master_Aman <- read_csv("df_master_Aman.csv")
df_unique_master3 <- read_csv("df_unique_master3.csv")
df_transaction_master_Aman <- read_csv("df_transaction_master_Aman.csv")
dublicate_store_cleaned_Aman <- read_csv("dublicate_store_cleaned_Aman.csv")

Iowa_Liquor_Sales_Cleaned_v3 <- read_csv("Iowa_Liquor_Sales_Cleaned_v3.csv")
liquor4=Iowa_Liquor_Sales_Cleaned_v3


df_unique_master3=as.data.frame(df_unique_master3)
View(df_unique_master3)

# drop X1
drops=c('X1')
df_unique_master3=df_unique_master3[ , !(names(df_unique_master3) %in% drops)]

drops=c('X1')
dublicate_store_cleaned_Aman=dublicate_store_cleaned_Aman[ , !(names(dublicate_store_cleaned_Aman) %in% drops)]
View(dublicate_store_cleaned_Aman)

drops=c('X1')
df_transaction_master_Aman=df_transaction_master_Aman[ , !(names(df_transaction_master_Aman) %in% drops)]
View(df_transaction_master_Aman)

drops=c('X1')
liquor4=liquor4[ , !(names(liquor4) %in% drops)]
View(liquor4)

df_unique_master4=df_unique_master3
View(df_unique_master4)
dublicate_store_cleaned2=dublicate_store_cleaned_Aman
df_transaction_master2=df_transaction_master_Aman

# dropping columns from liquor
names(liquor4)
liquor5=liquor4[,c("Store Number","Zip Code","Address","City","Store Location") ]
names(liquor5)[names(liquor5) == "Zip Code"] <- "Zip_Code"
names(liquor5)[names(liquor5) == "Store Number"] <- "Store_Number"
names(liquor5)[names(liquor5) == "Store Location"] <- "Store_Location"

names(liquor5)
View(liquor5)
names(df_unique_master4)

length(unique(liquor5$Store_Number))
#2318

length(unique(df_transaction_master2$Store_Number))
#2318

unique_stores=unique(df_unique_master4$Store_Number)

liquor5_unique=liquor5[liquor5$Store_Number %in% unique_stores,]
liquor5_unique=unique(liquor5_unique)
View(liquor5_unique)
length(unique(liquor5$Store_Number))

# split store location to lon and lat columns
lat_unique=rep(NA, nrow(liquor5_unique))
lon_unique=rep(NA, nrow(liquor5_unique))

for (i in 1:nrow(liquor5_unique)) {
  zz=unlist(strsplit(liquor5_unique$Store_Location[i], " "))
  lon_unique[i]=unlist(strsplit(zz[2], "\\("))[2]
  lat_unique[i]=unlist(strsplit(zz[3], ")"))
}

# merging lat lon to df_unique_master
length(lon_unique)
length(liquor5_unique$Store_Number)

liquor5_unique2=cbind(liquor5_unique,lon_unique,lat_unique)
View(liquor5_unique2)

summary(liquor5_unique2)

# we see 1 NA in zip code so need to fix that with google api
no_zip=liquor5_unique2[(is.na(liquor5_unique2$Zip_Code)) > 0,]

library(revgeo)
no_zip_unique=as.character(unlist(revgeo(longitude = as.double(as.character(no_zip$lon_unique)), latitude = as.double(as.character(no_zip$lat_unique)),provider = 'google',API = '###########',  output='frame')[6]))
no_zip_unique=as.numeric(no_zip_unique)

liquor5_unique2$Zip_Code[753]=no_zip_unique

str(liquor5_unique2)

sum(is.na(liquor5_unique2))
sum(is.na(liquor5_unique2))

no_zip=liquor5_unique2[(is.na(liquor5_unique2)) > 0,]
summary(liquor5_unique2)

View(liquor5_unique2)

# merge liquor5_unique2 and dublicate_store_cleaned2
length(unique(dublicate_store_cleaned2$Store_Number))
#112

length(unique(liquor5_unique2$Store_Number))
#2206

length(unique(df_transaction_master2$Store_Number))
#2318

# 112+2206=2318 so we see that the number of store is preserved and we do not miss any

names(liquor5_unique2)
names(dublicate_store_cleaned2)

# renaming the columns in both the above dataframes
names(liquor5_unique2)[names(liquor5_unique2) == "lon_unique"] <- "lon"
names(liquor5_unique2)[names(liquor5_unique2) == "lat_unique"] <- "lat"
names(liquor5_unique2)

# dropping store_location from liquor5_unique2
liquor5_unique3=liquor5_unique2[,c("Store_Number","Zip_Code","Address","City","lon","lat") ]
names(liquor5_unique3)


# exporting liquor5_unique3 to local as csv
write.csv(liquor5_unique3,"/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/liquor5_unique3_Aman.csv", row.names = TRUE)

# checking the datatype of liquor5_unique3 and dublicate_store_cleaned2 before merging
str(liquor5_unique3)
str(dublicate_store_cleaned2)

# changing datatype of lat and lon of liquor5_unique3 to number as it is in dublicate_store_cleaned2
liquor5_unique3$lon=as.double(as.character(liquor5_unique3$lon))
liquor5_unique3$lat=as.double(as.character(liquor5_unique3$lat))

# removing ',' and '.' from address column in liquor5_unique3
df_nocomma= gsub(",","",liquor5_unique3$Address)
df_nocomma_noDot= gsub("\\.","",df_nocomma)
liquor5_unique3$Address=df_nocomma_noDot

View(liquor5_unique3)
length(liquor5_unique3$Store_Number)
summary(liquor5_unique3)

liquor5_unique3[liquor5_unique3$`Store_Number`==5142,]$lon[1]=-91.63368
liquor5_unique3[liquor5_unique3$`Store_Number`==5142,]$lat[1]=41.97667

# we see that one of the lon is -104, its in colorado even zip code is for colorado so can be an outlier. So we have to leave it as it is

liquor5_unique3=unique(liquor5_unique3)

str(liquor5_unique3)
str(dublicate_store_cleaned2)

df_master3=rbind(liquor5_unique3,dublicate_store_cleaned2)
nrow(liquor5_unique3)
#2207

nrow(dublicate_store_cleaned2)
#112

nrow(df_master3)
#2319

# converting lon lat to Store Location
Store_Location=rep(NA,nrow(df_master3))
for (i in 1:nrow(df_master3)) {
  Store_Location[i]=sprintf("POINT (%f %f)", df_master3$lon[i], df_master3$lat[i])
}

# combine store location to df_master3
df_master3=cbind(df_master3,Store_Location)

summary(df_master3)
View(df_master3)
sum(is.na(df_master3))

# exporting df_master3 to local as csv
write.csv(df_master3,"/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/df_master6_Aman.csv", row.names = TRUE)

# merging df_master3 with Iowa_Liquor_Sales_Cleaned_v3
Iowa_Liquor_Sales_Cleaned_v3_2=Iowa_Liquor_Sales_Cleaned_v3

names(Iowa_Liquor_Sales_Cleaned_v3_2)
names(df_master3)

# dropping Address, City, Zip_Code, Store_location
drops=c("Address","City","Zip Code","Store Location")
Iowa_Liquor_Sales_Cleaned_v3_2=Iowa_Liquor_Sales_Cleaned_v3_2[ , !(names(Iowa_Liquor_Sales_Cleaned_v3_2) %in% drops)]

# changing Store_Number of df_master3 to Store Number
names(df_master3)[names(df_master3) == "Store_Number"] <- "Store Number"
names(df_master3)[names(df_master3) == "Store_Location"] <- "Store Location"
names(Iowa_Liquor_Sales_Cleaned_v3_2)
names(df_master3)
length(unique(Iowa_Liquor_Sales_Cleaned_v3_2$`Store Number`))
#2318
length(Iowa_Liquor_Sales_Cleaned_v3_2$`Store Number`)
#17846801
nrow(Iowa_Liquor_Sales_Cleaned_v3_2)
#17846801


View(Iowa_Liquor_Sales_Cleaned_v3_2)

# merge (inner join) df_master with liquor2 on the basis of store number
df_transaction_master2=merge(x=Iowa_Liquor_Sales_Cleaned_v3_2,y=df_master3, by.x = "Store Number", by.y = "Store Number", all= FALSE)

View(df_transaction_master2)
# investigate if the records are correct
length(unique(df_transaction_master2$`Store Number`))
#2318
length(df_transaction_master2$`Store Number`)
#18230950
nrow(df_transaction_master2)
#18230950
summary(df_transaction_master2)

# droping lon and lat and changing the order of columns
df_transaction_master3=df_transaction_master2
names(df_transaction_master3)
df_transaction_master3=df_transaction_master3[,c("Invoice/Item Number","Store Number","Date", "Store Name",
                                                 "Zip_Code","Address","City","Store Location","County Number","County","Category",
                                                 "Category Name","Vendor Number","Vendor Name","Item Number","Item Description",
                                                 "Pack","Bottle Volume (ml)","State Bottle Cost","State Bottle Retail",
                                                 "Bottles Sold","Sale (Dollars)","Volume Sold (Liters)","Volume Sold (Gallons)")]

View(df_transaction_master3)


# exporting df_master3 to local as csv
write.csv(df_transaction_master2,"/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/IOWA_transaction_latlon.csv", row.names = TRUE)


# exporting df_master3 to local as csv
write.csv(df_transaction_master3,"/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/IOWA_transaction.csv", row.names = TRUE)
















