df=df_county_data
typeof(df)


# droping the columns like address,city,county,zipcode
for (i in 1:length(df)) {
  df[[i]]=df[[i]][,c("Store_Number","longitude","latitude","transaction_count")]
}

# changing transaction_count to population
for (i in 1:length(df)) {
  names(df[[i]])[names(df[[i]]) == "transaction_count"] <- "population"
}


K = 1 #optimal K from elbow method
df_centroid=c()
df_kmeans_result=c()
for (i in 1:length(df)) {
  cluster_assignment=c()
  cluster_assignment = weighted_kmeans(df[[i]], K)
  df_city_cluster = as.data.frame(cbind(longitude = df[[i]]$longitude, latitude = df[[i]]$latitude, cluster = cluster_assignment[i]))
  
  #calculate centroids (weighted average)
  centroid_long = vector()
  centroid_lat = vector()
  
  for (k in c(1:K)) {
    cluster_k = which(cluster_assignment == k) #city index of cluster k
    centroid_long[k] = weighted.mean(df[[i]]$longitude[cluster_k], df[[i]]$population[cluster_k])
    centroid_lat[k] = weighted.mean(df[[i]]$latitude[cluster_k], df[[i]]$population[cluster_k])
  }
  
  #create data frame for centroid with dummy cluster number 
  df_centroid[[i]] = as.data.frame(cbind(longitude = centroid_long, latitude = centroid_lat, cluster = rep(K+1, length(centroid_lat))))
  
  #append df_city_cluster and df_centroid for ggplot
  df_kmeans_result[[i]] = rbind.data.frame(df_city_cluster, df_centroid[[i]])
  
}

df_kmeans_result[[1]]
df_centroid[[1]]$longitude

df_centroid3=df_centroid

lon=vector()
lat=vector()
county=vector()
length(df_centroid3)
for (i in 1:length(df_centroid3)) {
  lon[i]=df_centroid3[[i]]$longitude
  lat[i]=df_centroid3[[i]]$latitude
  county[i]=df_county_name[i]
}

df_centroid_county2=cbind(county,lat,lon)


# exporting df_centroid_county2 to local as csv
write.csv(df_centroid_county2,"/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/df_centroid_county2.csv", row.names = TRUE)

#--------------------------------------------------------------------------
# merging the centrod lat, lon and county name of every county with with store details

df2=df
# changing population to transaction_count
for (i in 1:length(df2)) {
  names(df2[[i]])[names(df2[[i]]) == "population"] <- "transaction_count"
}

df2[[99]]
df_centroid_county3=as.data.frame(cbind(county,lat,lon))
for (i in 1:length(df2)) {
  df2[[i]][,'county']=df_centroid_county3$county[i]
  df2[[i]][,'deliveryPad_latitude']=df_centroid_county3$lat[i]
  df2[[i]][,'deliveryPad_longitude']=df_centroid_county3$lon[i]
}

for (i in 1:length(df2)) {
  attach(df2[[i]])
  df2[[i]] <- df2[[i]][order(-transaction_count),]
}

df3=df2

# renaming and rearranging the columns
for (i in 1:length(df3)) {
  df3[[i]]=df3[[i]][,c('county','Store_Number','longitude','latitude','transaction_count','deliveryPad_longitude','deliveryPad_latitude')]
  names(df3[[i]])[names(df3[[i]]) == "longitude"] <- "store_longitude"
  names(df3[[i]])[names(df3[[i]]) == "latitude"] <- "store_latitude"
  
}

df4=df3
df4[[2]]

# adding city
df4[[99]]
df_city_store=master_datastore[,c('Store_Number','City')]

length(df_city_store$City)
nrow(df_city_store)

for (i in 1:length(df4)) {
  df4[[i]]=merge(x=df4[[i]],y=df_city_store, by.x = "Store_Number", by.y = "Store_Number", all= FALSE)
  names(df4[[i]])[names(df4[[i]]) == "City"] <- "store_city"
  df4[[i]]=df4[[i]][,c('county','Store_Number','store_city','store_longitude','store_latitude','transaction_count','deliveryPad_longitude','deliveryPad_latitude')]
  
}


# predict the city of the delivery pads

# do reverse gio code to predict city, county, zipcode for respective store number

df_centroid_county4=df_centroid_county3

# install.packages("revgeo")
library(revgeo)

deliveryPad_city=rep(NA,nrow(df_centroid_county4))

attach(df_centroid_county4)

for (i in 1:nrow(df_centroid_county4)) {
  deliveryPad_city[i]=revgeo(longitude = lon[i], latitude = lat[i],provider = 'google',API = '#######################',  output='frame')[3]
}

deliveryPad_city2=deliveryPad_city
deliveryPad_city2=unlist(deliveryPad_city)
deliveryPad_city2=as.data.frame(deliveryPad_city2)

# adding delivery city to deliveryPad_city
df_centroid_county5=df_centroid_county4
df_centroid_county5=as.data.frame(cbind(df_centroid_county5,deliveryPad_city2))

# dropping lat and lon from df_centroid_county5 and renaming deliveryPad_city2 to deliveryPad_city
df_centroid_county5=df_centroid_county5[,c('county','deliveryPad_city2')]
names(df_centroid_county5)[names(df_centroid_county5) == "deliveryPad_city2"] <- "deliveryPad_city"

# adding delivery pad city to df5
df5=df4
for (i in 1:length(df5)) {
  df5[[i]]=merge(x=df5[[i]],y=df_centroid_county5, by.x = "county", by.y = "county", all= FALSE)
}

df6=df5
# sorting in descending order based on transaction_count
for (i in 1:length(df6)) {
  attach(df6[[i]])
  df6[[i]] <- df6[[i]][order(-transaction_count),]
}



#-------------------------------------------------------------------------------------------------------------
# exporting df6 to local as csv
for (i in 1:length(df2)) {
  write.csv(df6[[i]],sprintf("/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/Delivery_Pad/deliveryPad_%s.csv", as.character(df6[[i]]$county[1])), row.names = TRUE)
  
}
write.csv(df6[[9]],sprintf("/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/Delivery_Pad/df299_%s.csv", as.character(df6[[9]]$county[1])), row.names = TRUE)


#-------------------------------------------------------------------------------------------------------------
# exporting df_centroid_county4 to local as csv
df_centroid_county6=df_centroid_county4
df_centroid_county6=as.data.frame(cbind(df_centroid_county4,deliveryPad_city2))
names(df_centroid_county6)[names(df_centroid_county6) == "deliveryPad_city2"] <- "deliveryPad_city"

write.csv(df_centroid_county6,"/Users/amanprasad/Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/Delivery_Pad/deliveryPad_IOWA_without_storeData.csv", row.names = TRUE)






