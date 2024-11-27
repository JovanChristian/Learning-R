# Read csv 
products <- read.csv('products.csv', na.strings = c('', 'N/A'), stringsAsFactors = F)
orders <- read.csv('orders.csv', fileEncoding = 'UTF-8-BOM', na.strings = c('', 'N/A'), stringsAsFactors = F)

# Hilang N/A data
orders <- na.omit(orders)
products <- na.omit(products)

#Remove All Product is not alchohol 
alcohol <- products[products$department == 'alcohol',]
alcohol <- alcohol[alcohol$aisle != 'specialty wines champagnes', ]
alcohol <- unique(alcohol)

# Prepare Data 
Transaction <- merge(orders, alcohol, by = 'product_id')

# Split Data 
Transaction <- split(Transaction$product_name, Transaction$order_id)

# Apriori 
installed.packages('arules')
library('arules')

# as 
Transaction <- as(Transaction, 'transactions') 

TransactionData <- apriori(Transaction, parameter = list(support = 0.04, target = 'Frequent'))
inspect(TransactionData)
inspect(ruleInduction(TransactionData, confidence = 0.5))
