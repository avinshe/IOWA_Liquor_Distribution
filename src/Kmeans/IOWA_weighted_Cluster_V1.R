library(readr)

# importing the master file and tracsaction data
Master_Data_Stores_final_version_S <- read_csv("Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/Master_Data_Stores_final_version_S.csv")
master_datastore=Master_Data_Stores_final_version_S

Iowa_Liquor_Sales <- read_csv("Documents/Courses_IIT_Fall_2019/DPA/Project-DPA/Iowa_Liquor_Sales.csv", 
                              col_types = cols(Date = col_date(format = "%m/%d/%Y")))
transaction=Iowa_Liquor_Sales

# checking the count of transaction and master with respect to store_number

length(unique(transaction$`Store Number`))
# 2330
length(unique(master_datastore$`Store Number`)) 
# 2317

uniq_transaction_store=unique(transaction$`Store Number`)

uniq_master_store=unique(master_datastore$`Store Number`)

# selecting only those stores from transaction whcih are present in master
transaction2=transaction[transaction$`Store Number` %in% uniq_master_store,]
length(unique(transaction2$`Store Number`))
# 2317

# renaming the columns in master
names(master_datastore)
names(master_datastore)[names(master_datastore) == "Store Number"] <- "Store_Number"
names(master_datastore)[names(master_datastore) == "Zip Code"] <- "Zip_Code"

# droping the unnecessary columns from master
master_datastore=master_datastore[,c("Store_Number","Address","Zip_Code","City","County","lat","lon")]
View(master_datastore)

# find the number of transactions of every store in transaction2
num_transaction_store=as.data.frame(table(transaction2$`Store Number`))
View(num_transaction_store)
nrow(num_transaction_store)
#2317

# adding num_transaction_store to master_datastore on the basis of store number

# renamaning the columns in num_transaction_store
names(num_transaction_store)
names(num_transaction_store)[names(num_transaction_store) == "Var1"] <- "Store_Number"
names(num_transaction_store)[names(num_transaction_store) == "Freq"] <- "transaction_count"

master_datastore2=merge(x=master_datastore,y=num_transaction_store, by.x = 'Store_Number', by.y = 'Store_Number', all=FALSE)
nrow(master_datastore2)
#2317
nrow(master_datastore)
#2317

# renaming the columns in master_datastore2
names(master_datastore2)
names(master_datastore2)[names(master_datastore2) == "lat"] <- "latitude"
names(master_datastore2)[names(master_datastore2) == "lon"] <- "longitude"



View(master_datastore2)

# create separate dataframe for every county
uniq_county=as.character(unique(master_datastore2$County))
length(uniq_county)
#99

df_county_name=c()
df_county_data=c()
for (i in 1:length(uniq_county)) {
  df_county_data[[i]]=(as.data.frame(master_datastore2[master_datastore2$County == uniq_county[i],]))
  df_county_name[[i]]=uniq_county[i]
}

df_county_data[[1]]
df_county_data[[2]]
df_county_name[[1]]

length(df_county_data)
# 99




