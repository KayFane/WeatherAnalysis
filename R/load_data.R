# > head(categories.sales)
# TRANSACTION_DATE STORE_NUM BATTERIES_QTY BATTERIES_PRICE weather_id
# 1       2012-01-02       351             8          792.92     206770
# 2       2012-01-02       353            21         2044.80     185460
# 3       2012-01-02       360            11         1024.90     212375
# 4       2012-01-02       363            10          969.91     176021
# 5       2012-01-02       365             7          646.93     284364
# 6       2012-01-02       437             5          499.95     255877
# max_temp_amt min_temp_amt precipitation_mm_amt snow_cm_amt
# 1          4.5         -5.4                  0.0           0
# 2          1.5         -8.4                  0.0           0
# 3          3.6          0.2                  3.0           0
# 4          2.8        -10.5                  0.0           0
# 5         11.1          5.7                  2.2           0
# 6         -2.8        -25.1                  0.3           0
# weather_en_desc DAY_OF_WEEK HEATING_COOLING_QTY
# 1                    Cloudy      Monday                   1
# 2                      Snow      Monday                  32
# 3             Rain-snow mix      Monday                   8
# 4 Sunny with cloudy periods      Monday                   7
# 5                      Rain      Monday                   5
# 6                      Rain      Monday                   2
# HEATING_COOLING_PRICE BRAKES_QTY BRAKES_PRICE STEERING_QTY
# 1                 36.12          3       286.46            1
# 2                439.15          7       269.95            4
# 3                312.36          6       248.56            2
# 4                264.60          3       214.97            4
# 5                 36.40          3       114.98            4
# 6                 29.82          5       146.96            2
# STEERING_PRICE
# 1          46.09
# 2          89.35
# 3          64.65
# 4         118.14
# 5         140.21
# 6          48.78
categories.sales <- read.table("/Users/KayE/Documents/R/CT/results/categories_sales.txt",header=TRUE)
categories.sales$TRANSACTION_DATE <- as.Date(categories.sales$TRANSACTION_DATE)


#sample <- categories.sales[categories.sales$TRANSACTION_DATE < as.Date('2012-02-29',"%Y-%m-%d"),]