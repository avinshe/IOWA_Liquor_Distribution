# preparing files for tableau visualization

df_centroid_county22=df_centroid_county2
df_centroid_county55=df_centroid_county5

df_delivery=merge(x=df_centroid_county55,y=df_centroid_county22, by.x = 'county', by.y = 'county', all=FALSE)

df7=df6

# converting df7(which is a list of dataframe) to a single dataframe
library(dplyr)
df8=bind_rows(df7, .id = "column_label")
View(df8)

# droping columns from df8
df8=df8[,c('county','Store_Number','store_city','store_longitude','store_latitude','transaction_count')]

# adding column=type
type=rep('store',nrow(df8))

df8=cbind(type,df8)

# renaming the columns of df8
names(df8)[names(df8) == "store_city"] <- "city"
names(df8)[names(df8) == "store_longitude"] <- "longitude"
names(df8)[names(df8) == "store_latitude"] <- "latitude"



# renaming the columns of df_delivery
names(df_delivery)[names(df_delivery) == "deliveryPad_city"] <- "city"
names(df_delivery)[names(df_delivery) == "lon"] <- "longitude"
names(df_delivery)[names(df_delivery) == "lat"] <- "latitude"

# adding to df_delivery column=type, Store_Number, transaction_count
type=rep('Delivery Pad',nrow(df_delivery))
Store_Number=rep(NA,nrow(df_delivery))
transaction_count=rep(NA,nrow(df_delivery))

df_delivery=cbind(type,df_delivery,Store_Number,transaction_count)


names(df8)
names(df_delivery)

df9=df8[,c('type','county','city','latitude','longitude','Store_Number','transaction_count')]

# rbind df8 and df_delivery

df_store_delivery=rbind(df9,df_delivery)
View(df_store_delivery)

nrow(df_store_delivery)
#2416
nrow(df9)
#2317
nrow(df_delivery)
#99

# exporting df_store_delivery

write.csv(df_store_delivery,"/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/Delivery_Pad/df_store_delivery.csv", row.names = TRUE)









