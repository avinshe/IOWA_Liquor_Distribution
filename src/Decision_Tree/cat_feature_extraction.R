library(tools)
library(dplyr)
library(stringr)

file <- "/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/IOWA LIQUOR SALES/IOWA_LIQUOR_CLEANED_MERGED_DATA.csv"


df <- read.csv(file = file, sep = ",", header = TRUE)

summary(df)

caps <- function(x){
  string <- strsplit(as.character(x), " ")[[1]]
  paste0(toupper(substring(string, 1, 1)), tolower(substring(string, 2)), sep = " ", collapse = " ")
}


class(df)
str(df)

unique(df$Category.Name)

sum(is.na(df$Category.Name) | df$Category.Name == "") #24579

sum(is.na(df$Category) | df$Category.Name == "") #24579

sum(str_detect(df$Item.Description, 'Special')) #26334

head(df[df$Category.Name == "",])


df$Category.Name <- as.factor(sapply(df$Category.Name, caps))

df$Item.Description <- as.factor(sapply(df$Item.Description, caps))

df$Category.Name <- trimws(df$Category.Name)
df$Item.Description <- trimws(df$Item.Description)
table(df$Category.Name)
table(df$Item.Description)


head(df[,c('Category.Name', 'Item.Description')])

head(df[(df[,'Item.Description'] == "Stolichnaya  Premium  Vodka  80  Proof  (lv)"),c('Category.Name', 'Item.Description')])
unique(df[(df[,'Item.Description'] == "Ha  1792  Full  Proof"),c('Category.Name', 'Item.Description')])

df_map <- unique(df[df$Category.Name != "", c('Category.Name','Item.Description')])


sum(str_detect(df_map$Category.Name, 'Special'))

df_map[str_detect(df_map$Category.Name, 'Special'),]


#df[df[,'Category.Name'] == '','Category.Name'] <- df_map[df_map[,'Item.Description'] == df[,'Item.Description'],'Category.Name']
#ifelse(str_detect(df_map$Category.Name, 'Special'), 'Special')
df_map$AchoholTypes <- ifelse(str_detect(df_map$Category.Name, 'Special'), 'Special', ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Whisky|Whiskies*'), 'Whisky',
                              ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Vodka*'), 'Vodka',
                                     ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Brandies*'), 'Brandy',
                                            ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Tequila*'), 'Tequila',
                                                   ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Liqueurs*'), 'Liqueur',
                                                          ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Rum*'), 'Rum',
                                                                 ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Beer*'), 'Beer',
                                                                        ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Cocktails*'), 'Cocktails',
                                                                               ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Scotch*'), 'Whisky',
                                                                                      ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Gins*'), 'Gin',
                                                                                             ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Sec*'), 'Liqueur',
                                                                                                    ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Amaretto*'), 'Liqueur',
                                                                                                           ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Bourbon*'), 'Whisky',
                                                                                                                  ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Mezcal*'), 'Mezcal',
                                                                                                                         ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Neutral*'), 'Neutral',
                                                                                                                                ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Vap*'), 'Vap',
                                                                                                                                       ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Rock  &  Rye*'), 'Whisky',
                                                                                                                                              ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Schnapps*'), 'Schnapps',
                                                                                                                                                     ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Creme  De  Cacao*'), 'Liqueur',
                                                                                                                                                            ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Creme  De  Menthe*'), 'Liqueur',
                                                                                                                                                                   ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Creme  De  Almond*'), 'Liqueur',
                                                                                                                                                                          ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Anisette*'), 'Liqueur',
                                                                                                                                                                                 ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*American  Alcohol*'), 'American  Alcohol',
                                                                                                                                                                                        ifelse(str_detect(as.character(df_map[,'Category.Name']), '.*Iowa  Distilleries*'), 'Iowa  Distilleries',NA)))))))))))))))))))))))))



df_map$AchoholTypes <- ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Old  Forester  Bourbon*'), 'Whisky', 
                              ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Casa  Noble  Crystal*'), 'Tequila', 
                                     ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Hiram  Walker  Orange  Curacao*'), 'Schnapps', 
                                            ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Jose  Cuervo  Gold  W/1l  Classic  Margarita  Mix*'), 'Tequila', df_map$AchoholTypes))))
                              

df_map$AchoholTypes <- ifelse(is.na(df_map$AchoholTypes), ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Vodka'), 'Vodka',
                                                                 ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Tequila'), 'Tequila',
                                                                        ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Liqueur'), 'Liqueur',
                                                                               ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Gin'), 'Gin',
                                                                                      ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Bacardi  Coco'), 'Rum',
                                                                                             ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Creme  De  Almond'), 'Liqueur',
                                                                                                    ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Rain  Mango'), 'Vodka',
                                                                                                           ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Margarita'), 'Cocktails',
                                                                                                                  ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Salvador\'s  Top  Shelf'), 'Cocktails',
                                                                                                                         ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Johnnie  Walker'), 'Whisky',
                                                                                                                                ifelse(str_detect(as.character(df_map[,'Item.Description']), 'Mr.  Boston  Riva$'), 'Vodka',df_map$AchoholTypes))))))))))),
                                                                 df_map$AchoholTypes)




df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Beer'),'Beer',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Rootbeer'),'Beer',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Rum'),'Rum',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Lady  Bligh'),'Rum',df_map$AchoholTypes)

#df_map$AchoholTypes <-  ifelse(str_detect(df_map$AchoholTypes,'Liqueur'),'Schnapps',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Gin'),'Gin',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Hennessy  Vsop  Privilege'),'Brandy',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Hiram  Walker  Coffee  Brandy'),'Brandy',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Hiram  Walker  Kirschwasser'),'Brandy',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Five  O\'clock'),'Rum',df_map$AchoholTypes)

df_map$AchoholTypes <-  ifelse(str_detect(df_map$Item.Description,'Mezcal'),'Mezcal',df_map$AchoholTypes)

df_liquor <- unique(df_map[,c('Item.Description','AchoholTypes')])




df_whisky <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Whisky',], df_liquor[df_liquor$AchoholTypes != 'Whisky',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Whisky','AchoholTypes'], .$AchoholTypes.y, 'Whisky')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_whisky$Item.Description, 'AchoholTypes'] <- 'Whisky'

df_schnapps <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Schnapps',], df_liquor[df_liquor$AchoholTypes != 'Schnapps',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Schnapps','AchoholTypes'], .$AchoholTypes.y, 'Schnapps')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_schnapps$Item.Description, 'AchoholTypes'] <- 'Schnapps'


df_vodka <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Vodka',], df_liquor[df_liquor$AchoholTypes != 'Vodka',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Vodka','AchoholTypes'], .$AchoholTypes.y, 'Vodka')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_vodka$Item.Description, 'AchoholTypes'] <- 'Vodka'


df_liqueur <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Liqueur',], df_liquor[df_liquor$AchoholTypes != 'Liqueur',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Liqueur','AchoholTypes'], .$AchoholTypes.y, 'Liqueur')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_liqueur$Item.Description, 'AchoholTypes'] <- 'Liqueur'

df_rum <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Rum',], df_liquor[df_liquor$AchoholTypes != 'Rum',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Rum','AchoholTypes'], .$AchoholTypes.y, 'Rum')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_rum$Item.Description, 'AchoholTypes'] <- 'Rum'

df_tequila <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Tequila',], df_liquor[df_liquor$AchoholTypes != 'Tequila',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Tequila','AchoholTypes'], .$AchoholTypes.y, 'Tequila')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_tequila$Item.Description, 'AchoholTypes'] <- 'Tequila'

df_cocktails <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Cocktails',], df_liquor[df_liquor$AchoholTypes != 'Cocktails',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Cocktails','AchoholTypes'], .$AchoholTypes.y, 'Cocktails')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_cocktails$Item.Description, 'AchoholTypes'] <- 'Cocktails'

df_brandy <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Brandy',], df_liquor[df_liquor$AchoholTypes != 'Brandy',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Brandy','AchoholTypes'], .$AchoholTypes.y, 'Brandy')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_brandy$Item.Description, 'AchoholTypes'] <- 'Brandy'

df_gin <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Gin',], df_liquor[df_liquor$AchoholTypes != 'Gin',], by="Item.Description")  %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Gin','AchoholTypes'], .$AchoholTypes.y, 'Gin')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_gin$Item.Description, 'AchoholTypes'] <- 'Gin'

df_american_alchohol <- inner_join(df_liquor[df_liquor$AchoholTypes == 'American  Alcohol',], df_liquor[df_liquor$AchoholTypes != 'American  Alcohol',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'American  Alcohol','AchoholTypes'], .$AchoholTypes.y, 'American  Alcohol')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_american_alchohol$Item.Description, 'AchoholTypes'] <- 'American  Alcohol'

df_vap <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Vap',], df_liquor[df_liquor$AchoholTypes != 'Vap',], by="Item.Description")  %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Vap','AchoholTypes'], .$AchoholTypes.y, 'Vap')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_vap$Item.Description, 'AchoholTypes'] <- 'Vap'

df_neutral <- inner_join(df_liquor[df_liquor$AchoholTypes == 'Neutral',], df_liquor[df_liquor$AchoholTypes != 'Neutral',], by="Item.Description") %>%
  mutate(alchohol = ifelse(.$AchoholTypes.y %in% df_liquor[df_liquor$AchoholTypes == 'Neutral','AchoholTypes'], .$AchoholTypes.y, 'Neutral')) %>%
  select(Item.Description, alchohol)

df_liquor[df_liquor$Item.Description %in% df_neutral$Item.Description, 'AchoholTypes'] <- 'Neutral'


df_liquor <- unique(df_liquor)

#sum(str_detect(as.character(df$Category.Name), 'Special'))

df <- merge(df, df_liquor, by = 'Item.Description', all.x = TRUE)
df <- df[,c(2, 3, 24, 4:6, 25, 7, 8, 9, 1, 10:23)]

df[is.na(df$AchoholType),]

df[df$Item.Description == "\"beefeater  \"\"24\"\"\"","AchoholTypes"] <- "Gin"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Beer"),"AchoholTypes"] <- "Beer" 

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Aalborg  Jubilaeums  Aquavit"),"AchoholTypes"] <- "Schnapps"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Aberlour  A''bunadh"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Admiral  Nelson  Black  Patch  Mini"),"AchoholTypes"] <- "Rum"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Ardbeg  Auriverdes  Ha"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Rum"),"AchoholTypes"] <- "Rum"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Amrut  Fusion  Single  Malt  Whisy  Ha"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Whisky"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Ansac  Vs    Cognac"),"AchoholTypes"] <- "Brandy"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Bailey"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Whiskey"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "martini"),"AchoholTypes"] <- "Cocktails"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Barrel  Buffalo  Bill"),"AchoholTypes"] <- "Cocktails"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Bird  Dog  Apple  W/glass"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Bird  Dog  Blackberry"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Bird  Dog  Peach"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Black  Velvet  Toasted  Caramel"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Bombay  Sapphire  W/glass"),"AchoholTypes"] <- "Gin"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Gin"),"AchoholTypes"] <- "Gin"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Vodka"),"AchoholTypes"] <- "Vodka"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Carolan's"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Chivas  Regal"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Ciroc  Ten  Ha"),"AchoholTypes"] <- "Vodka"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Cedar  Ridge  Barrel  Proof  Bourbon"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Cody  Road  Uncommon  Double  Barrel"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Crown  Royal"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Diplomatico  Reserva  Exclusiva"),"AchoholTypes"] <- "Rum"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Disaronno"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Forty  Creek"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Glen  Moray"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Glenfiddich"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Glenlivet"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Blueberry  Valentino"),"AchoholTypes"] <- "Cocktails"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Burnetts"),"AchoholTypes"] <- "Vodka"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Calvados"),"AchoholTypes"] <- "Brandy"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Glenmorangie"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Highland  Park  Ice"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Johnnie  Walker"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Laphroaig"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Orphan  Barrel"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Rye"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Seven  Devils"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Hennessy"),"AchoholTypes"] <- "Brandy"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Tequila"),"AchoholTypes"] <- "Tequila"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Jack  Daniel's"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Jack  Daniels"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Jim  Beam"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Jose  Cuervo"),"AchoholTypes"] <- "Tequila"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Glendronach"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Grand  Marnier"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Jefferson's  Reserve"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Knob  Creek"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Maker's  Mark"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Moonshine"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Midnight  Moon"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Old  Forester"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Mezcal"),"AchoholTypes"] <- "Mezcal"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Ron  Abuelo  Anejo"),"AchoholTypes"] <- "Rum"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Cocoyac  Excellence"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Chateau  Du  Busca"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "[0-9]yr"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "El  Mayor  Extra  Anejo"),"AchoholTypes"] <- "Tequila"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Encanto  Grand  &  Noble"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Anejo"),"AchoholTypes"] <- "Tequila"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Aquavit"),"AchoholTypes"] <- "Schnapps"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Gamle  Ode  Holiday"),"AchoholTypes"] <- "Schnapps"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Absinthe"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Kahlua  Mocha"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Michters"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Armagnac"),"AchoholTypes"] <- "Brandy"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Schnapps"),"AchoholTypes"] <- "Schnapps"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Patron  Citronge  Mango"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Patron  Silver"),"AchoholTypes"] <- "Tequila"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Pearl  Caramel"),"AchoholTypes"] <- "Vodka"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Prairie  Wolf  Dark"),"AchoholTypes"] <- "Vodka"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Peach"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Southern  Comfort"),"AchoholTypes"] <- "Liqueur"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Spanish  Single  Malt"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Martini"),"AchoholTypes"] <- "Cocktails"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Stolichnaya"),"AchoholTypes"] <- "Vodka"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Malt"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Terry  Centenario"),"AchoholTypes"] <- "Brandy"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Twenty  Grand  Gold"),"AchoholTypes"] <- "Vodka"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Ty  Ku  Coconut"),"AchoholTypes"] <- "Cocktails"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Van  Gogh  Coconut"),"AchoholTypes"] <- "Vodka"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "White  Dog"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Wild  Turkey  American  Honey"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Woodford  Reserve"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType) & str_detect(df$Item.Description, "Wild  Turkey  Kentucky  Spirit  Buy  The  Barrel"),"AchoholTypes"] <- "Whisky"

df[is.na(df$AchoholType),]

final_file <- "/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/IOWA LIQUOR SALES/IOWA_LIQUOR_CATOR_CLEANED_MERGED_DATA.csv"

write.csv(x = df, file = final_file, quote = TRUE, row.names = FALSE)



#df[df$Item.Description == "Bombay  Sapphire  W/glass",]
