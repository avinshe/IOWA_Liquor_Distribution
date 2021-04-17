file <- '/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/Iowa_Liquor_Sales.csv'

master_file <- '/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/Master_Data_Stores_final_version.csv'


df <- read.csv(file, sep = ",", header = TRUE)


df_master <- read.csv(master_file, sep = ",", header = TRUE)
str(df)

class(df)
class(df_master)
df_master <- df_master[,-1]

df1 <- merge(x = df, y = df_master, by = 'Store.Number', all.y = TRUE)
colnames(df1)

summary(df1)

df1 <- df1[,-c(5:10)]

colnames(df1)

names(df1)[names(df1)=="Invoice.Item.Number"] <- "Invoice Item Number"
names(df1)[names(df1)=="Store.Name"] <- "Store Name"
names(df1)[names(df1)=="Category.Name"] <- "Category Name"
names(df1)[names(df1)=="Vendor.Number"] <- "Vendor Number"
names(df1)[names(df1)=="Vendor.Name"] <- "Vendor Name"
names(df1)[names(df1)=="Item.Number"] <- "Item Number"
names(df1)[names(df1)=="Item.Description"] <- "Item Description"
names(df1)[names(df1)=="Bottle.Volume..ml."] <- "Bottle Volume (ml)"
names(df1)[names(df1)=="State.Bottle.Cost"] <- "State Bottle Cost"
names(df1)[names(df1)=="State.Bottle.Retail"] <- "State Bottle Retail"
names(df1)[names(df1)=="Bottles.Sold"] <- "Bottles Sold"
names(df1)[names(df1)=="Sale..Dollars."] <- "Sale (Dollars)"
names(df1)[names(df1)=="Volume.Sold..Liters."] <- "Volume Sold (Liters)"
names(df1)[names(df1)=="Volume.Sold..Gallons."] <- "Volume Sold (Gallons)"

names(df1)[names(df1)=="Address.y"] <- "Address"
names(df1)[names(df1)=="Zip.Code.y"] <- "Zip Code"
names(df1)[names(df1)=="City.y"] <- "City"
names(df1)[names(df1)=="County.y"] <- "County"
names(df1)[names(df1)=="Store.Number"] <- "Store Number"
colnames(df1)

df1 <- df1[, c(2:24, 1)]
#Saving merged file to csv
merged_file <- "/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/IOWA LIQUOR SALES/IOWA_LIQUOR_CLEANED_MERGED_DATA.csv"

#df1$Row_id <- rownames(df1)

#df1 <- df1[, c(25, 1:24)]

write.csv(x = df1, file = merged_file, quote = TRUE, row.names = FALSE)


test = read.csv(merged_file, sep = ",", header = TRUE)


nrow(df1)
length(df1$`Invoice Item Number`)
length(unique(df1$`Invoice Item Number`))
