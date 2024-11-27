# Read File 
orders <- read.csv('orders.csv', na.strings = c('', 'N/A'), stringsAsFactors = F)
products <- read.csv('products.csv', na.strings = c('', 'N/A'), stringsAsFactors = F) 

# Hilangin N/A pada dataset 
orders <- na.omit(orders)
products <- na.omit(products)

# Visualization 
# a 
boxplot(products$product_price~products$department, data = products,
        xlab = "Department",
        ylab = "Product Price",
        main = "Product Price For All Product Department",
        col = rainbow(length(unique(products$department)))
)


# b = Pie Chart
# Table -> Untuk menghitung Jumlah data yang sama
dept_count <- table(products$department)

# Bikin .csv yang baru (Pake data frame)
df_dept <- data.frame(
  department = names(dept_count),
  count = as.numeric(dept_count)
)

# Bikin order untuk milih data file dari atribut
df_dept <- df_dept[order(df_dept$count, decreasing = TRUE), ]


top_5 <- df_dept[1:5,]

# nrow -> Jumlah row nya berapa?
others <- sum(df_dept$count [6: nrow(df_dept)])

df_others <- data.frame(
  department = "Others",
  count = others
)

# rbind -> Gabungin 2 data frame 
top_final <- rbind(top_5, df_others)

# Round -> Hitung Bilangan desimal
top_final$percentage <- round((top_final$count/sum(top_final$count)) * 100, 2)

# Nambah Persen "%" pas print
labels <- paste(top_final$department, top_final$percentage, "%",
                sep = "")


# pie plot
pie(top_final$count, 
    labels = labels,
    main = "Top 5 Deparment (Based on Product Count",
    col = rainbow(nrow(top_final)))

# c
frozen_products <- products[products$department == "frozen", ]

aisle_count <- table(frozen_products$aisle)

# Bikin .csv yang baru (Pake data frame)
df_aisle <- data.frame(
  aisle = names(aisle_count),
  count = as.numeric(aisle_count)
)

# Tidak usah tulis decreasing -> Otomatis FALSE
df_aisle <- df_aisle[order(df_aisle$count), ]

top_3 <- df_aisle[1:3,]

# bar plot
# names.arg -> Untuk kasih nama di bawah Bar
barplot(top_3$count,
        names.arg = top_3$aisle,
        main = "Lowest 3 Aisle in frozen Department (Based on Product Count",
        col = rainbow(nrow(top_3)))


# d


# Apriori 
# a. Data Preprocessing
# Remove yang bukan alcohol
alcohol_product <- products[products$department == "alcohol", ]

# Specialty wines champagnes
alcohol_product <- alcohol_product[alcohol_product$aisle !=
                                     "specialty wines champagnes", ]
# remove duplicate
alcohol_product <- unique(alcohol_product)

# b. Data Transformation
transaction_data <- merge(orders, alcohol_product, by = "product_id")
transactions_data <- split(transaction_data$product_name, transaction_data$order_id)
  
install.packages("arules")
library("arules") 




