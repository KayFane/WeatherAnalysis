#load raw data
#
#Outputs:
#stores.info -- Information on every store
#stores.sale -- Sales info on every store
#stores.weather -- Weather info on every store
#Outputs Sample:
# > head(stores.info)
# STORE_NBR    FCLTY_CITY   FCLTY_PROVINCE FCLTY_POSTAL_CD LONGITUDE LATITUDE
# 1       610      RICHMOND BRITISH COLUMBIA         V6X 2C2  -123.098   49.132
# 2       483      SQUAMISH BRITISH COLUMBIA         V8B 0H5  -123.134   49.736
# 3       438 WILLIAMS LAKE BRITISH COLUMBIA         V2G 3A6  -122.130   52.118
# 4       466        DUNCAN BRITISH COLUMBIA         V9L 0C1  -123.718   48.801
# 5       426       LANGLEY BRITISH COLUMBIA         V2Y 1A1  -122.667   49.118
# 6       433    CHILLIWACK BRITISH COLUMBIA         V2R 4E7  -121.958   49.139
# SERVICE_BAY_CNT
# 1              10
# 2               8
# 3              10
# 4              10
# 5              16
# 6              10
# > head(stores.sale)
# STORE_NUM  PROD_NBR    CATEGORY_NM FINELINE_CD POS_TRANSACTION_ID TRANSACTION_DATE
# 1       609 121020001 AUTO BATTERIES       12102          710129144       2012-07-25
# 2       368 121020001 AUTO BATTERIES       12102          634057339       2012-03-21
# 3       479 121020001 AUTO BATTERIES       12102          754327323       2012-10-15
# 4       486 121020001 AUTO BATTERIES       12102          678154159       2012-06-04
# 5       487 121020001 AUTO BATTERIES       12102          716080443       2012-08-04
# 6       363 121020001 AUTO BATTERIES       12102          609311863       2012-01-18
# TRANSACTION_TM ITEM_TRANSACTION_QTY ITEM_RETURN_QUANTITY ACTIVE_CONSUMER_PRICE_AMT
# 1       19.40.50                    1                    0                      9.99
# 2       09.24.18                    1                    0                      9.99
# 3       18.18.45                    1                    0                      9.99
# 4       15.53.35                    1                    0                      9.99
# 5       15.07.18                    1                    0                      9.99
# 6       09.34.52                    1                    0                      9.99
# > head(stores.weather)
# weather_id   city_num province_abbr_nm        weather_date  weather_tm
# 1     187038    KELOWNA               BC 2013-04-03 00:00:00 02:00:00:00
# 2     187084    KELOWNA               BC 2013-11-18 00:00:00 02:00:00:00
# 3     146020 ABBOTSFORD               BC 2013-04-25 00:00:00 02:00:00:00
# 4     146066 ABBOTSFORD               BC 2013-12-04 00:00:00 02:00:00:00
# 5     161954  COQUITLAM               BC 2012-01-31 00:00:00 02:00:00:00
# 6     162000  COQUITLAM               BC 2013-08-12 00:00:00 02:00:00:00
# weather_datatype_cd max_temp_amt min_temp_amt precipitation_mm_amt snow_cm_amt
# 1                 obs       17.800        1.600                0.000       0.000
# 2                 obs        6.900        0.200                4.100       0.000
# 3                 obs       18.200        3.500                0.000       0.000
# 4                 obs        1.800       -3.000                0.000       0.000
# 5                 obs        6.600        4.600               13.100       0.000
# 6                 obs       25.200       14.600                0.000       0.000
# weather_en_desc         weather_fr_desc weather_icon_nm weather_source_desc
# 1 No Precipitation   pas de precipitations       no_precip             primary
# 2           Cloudy                 Nuageux               o             primary
# 3           Cloudy                 Nuageux               o             primary
# 4     Mainly sunny Généralement ensoleillé               h             primary
# 5             Rain                   Pluie               r           secondary
# 6           Cloudy                 Nuageux               o             primary
# > 

library(plyr)

province <- "BC"
#province <- "AB_BC"


path.stores.info <- sprintf("./data/%s/weather project - store.tab", province) 
path.stores.sale <- sprintf("./data/%s/weather project - sales.tab", province)
path.weather <- sprintf("./data/%s/weather project - daily weather.tab", province)

stores.info <- read.table(path.stores.info, sep="\t", header=TRUE)
stores.sale <- read.table(path.stores.sale, sep="\t", header=TRUE)
weather <- read.table(path.weather, sep="\t", header=TRUE)

weather$city_num <- as.character(weather$city_num)


#aggregate stores.sale by category
stores.category.sale <- ddply(stores.sale,c("STORE_NUM","CATEGORY_NM","TRANSACTION_DATE"),summarise,
                              TRANSACTION_QTY = sum(ITEM_TRANSACTION_QTY),
                              RETURN_QUANTITY = sum(ITEM_RETURN_QUANTITY),
                              CONSUMER_PRICE = sum(ACTIVE_CONSUMER_PRICE_AMT))

StoreSalesWeather <- function(store.id) {
  #store.id = 350
  store.info <- stores.info[stores.info$STORE_NBR == store.id, ]
  store.city <- as.character(store.info$FCLTY_CITY)
  
  if (!any(weather$city_num == store.city))
    return(NA)
  store.weather <- weather[weather$city_num == store.city, ]
  store.weather <- subset(store.weather, select = c("weather_id","weather_date", "max_temp_amt","min_temp_amt","precipitation_mm_amt", "snow_cm_amt","weather_en_desc"))
  store.weather$weather_date <- substr(store.weather$weather_date, 0, 10)
  
  store.category.sale <- stores.category.sale[stores.category.sale$STORE_NUM == store.id, ]
  
  store.category.saleAndWeather <- merge(x=store.category.sale, y=store.weather, by.
                                         ="TRANSACTION_DATE", by.y="weather_date")
  
  store.category.saleAndWeather$max_temp_amt = as.numeric(as.character(store.category.saleAndWeather$max_temp_amt))
  store.category.saleAndWeather$min_temp_amt = as.numeric(as.character(store.category.saleAndWeather$min_temp_amt))
  store.category.saleAndWeather$precipitation_mm_amt = as.numeric(as.character(store.category.saleAndWeather$precipitation_mm_amt))
  store.category.saleAndWeather$snow_cm_amt = as.numeric(as.character(store.category.saleAndWeather$snow_cm_amt))
  
  store.category.saleAndWeather$DAY_OF_WEEK <- weekdays(as.Date(store.category.saleAndWeather$TRANSACTION_DATE))
  
  return(store.category.saleAndWeather)
}

StoresSalesWithWeather <- function() {
  stores.id = unique(stores.info$STORE_NBR)
  stores.org <- c("")  
  
  for (store.id in stores.id) {
    store.org <- StoreSalesWeather(store.id)
    if (is.data.frame(store.org)) {
      stores.org <- rbind(stores.org, store.org)
    }
  }
  
  return(stores.org)
}

#path.org <- "./results/SalesWithWeather.txt"
#write.table(stores.org, path.org, sep="\t", row.names=FALSE,append = FALSE)
sales.with.weather <- StoresSalesWithWeather()
# for (category in unique(sales.with.weather$CATEGORY_NM))
# {
#   sales.category <- data.frame()
#   temp <- data.frame()
#   if (nrow(sales.category) == 0)
#     sales.category <- sales.with.weather[sales.with.weather$CATEGORY_NM == category, ]
#   else 
#     temp <- sales.with.weather[sales.with.weather$CATEGORY_NM == category, ]  
#         
#   
# }

sales.batteries <- sales.with.weather[sales.with.weather$CATEGORY_NM == "AUTO BATTERIES", ]
sales.batteries <- sales.batteries[, c(-3, -5)]
colnames(sales.batteries)[3] <- "BATTERIES_QTY"
colnames(sales.batteries)[4] <- "BATTERIES_PRICE"

sales.heatingcooling <- sales.with.weather[sales.with.weather$CATEGORY_NM == "AUTO HEATING & COOLING PARTS", ]
sales.heatingcooling <- sales.heatingcooling[, c(1,2,4,6)]
colnames(sales.heatingcooling)[3] <- "HEATING_COOLING_QTY"
colnames(sales.heatingcooling)[4] <- "HEATING_COOLING_PRICE"
sales.category <- merge(sales.batteries, sales.heatingcooling, by = c("TRANSACTION_DATE", "STORE_NUM"))

sales.brakes <- sales.with.weather[sales.with.weather$CATEGORY_NM == "BRAKES", ]
sales.brakes<- sales.brakes[, c(1,2,4,6)]
colnames(sales.brakes)[3] <- "BRAKES_QTY"
colnames(sales.brakes)[4] <- "BRAKES_PRICE"
sales.category <- merge(sales.category, sales.brakes, by = c("TRANSACTION_DATE", "STORE_NUM"))

sales.steering <- sales.with.weather[sales.with.weather$CATEGORY_NM == "STEERING & SUSPENSION", ]
sales.steering <- sales.steering[, c(1,2,4,6)]
colnames(sales.steering)[3] <- "STEERING_QTY"
colnames(sales.steering)[4] <- "STEERING_PRICE"


sales.category <- merge(sales.category, sales.steering, by = c("TRANSACTION_DATE", "STORE_NUM"))

