
sales <- ReadTable("categories_sales_BC.txt")
sales$TRANSACTION_DATE <- as.Date(sales$TRANSACTION_DATE)




#sample <- sales[sales$TRANSACTION_DATE < as.Date('2012-02-29',"%Y-%m-%d"),]