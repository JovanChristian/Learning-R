# Read File 
products <- read.csv('products.csv', na.strings = c('', 'N/A'), stringsAsFactors = F)
orders <- read.csv('orders.csv', fileEncoding = 'UTF-8-BOM', na.strings = c('', 'N/A'), stringsAsFactors = F)

# Hilangin N/A Data 
products <- na.omit(products)
orders <- na.omit(orders)

# Preprocessing 
alcohol <- products[products$department == 'alcohol',]
alcohol <- alcohol[alcohol$aisle != 'specialty wines champagnes',]
alcohol <- unique(alcohol) 

# Prepare Data 
Transaction <- merge(alcohol, orders, by = 'product_id')
Transaction <- split(Transaction$product_name, Transaction$order_id)

install.packages('arules')
library('arules')

# as
TransactionData <- as(Transaction, 'transactions')

TransactionData <- apriori(TransactionData, parameter = list(support = 0.04, target = 'Freq'))
inspect(TransactionData)
inspect(ruleInduction(TransactionData, confidence = 0.5))
