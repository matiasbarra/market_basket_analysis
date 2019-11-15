# Project: Marker Basket Analysis 
# Date: 11 Nov 2019
# Author: Matias Barra

# Script purpose: perform a market basket analysis to help Blackwell's board 
# of directors to better understand the clientele that Electronidex is currently 
# serving and if Electronidex would be an optimal acquisition

####--------------- Set Enviroment ---------------------------------------------####

# load required libraries
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
pacman::p_load("readr","ggplot2","arules","arulesViz","plotly", "tidyverse", "knitr", "shinythemes", "plyr","RColorBrewer")


####--------- Import data sets and initial exploration -------------------------#### 


setwd("/Users/matiasbarra/Documents/Data_Analytics_Course/Data_Analytics_2/4_Discover_Associations_Between_Products")
Data <- read.csv("./data/ElectronidexTransactions2017.csv", header = F)# read transactions as df
sum(Data !="") # check total number of products sold

setwd("/Users/matiasbarra/Documents/Data_Analytics_Course/Data_Analytics_2/4_Discover_Associations_Between_Products")
categories <- read.csv("./data/prodCategories.csv", header = T)# read categories as df

# read "transactions.csv" as basket to perform association rules analysis
Data <- read.transactions("./data/ElectronidexTransactions2017.csv", 
                           format = "basket", 
                           sep = ",",
                           rm.duplicates = F)

summary(Data)
# 9835 rows (elements/itemsets/transactions) and
# 125 columns (items) and a density of 0.03506172 

inspect(Data)# You can view the transactions. Is there a way to see a certain # of transactions? 
length (Data) # Number of transactions.
size (Data) # Number of items per transaction
LIST(Data) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(Data)# To see the item labels

#Creating a new Data Frame with a list of 125 products
ProductLabels <- as.data.frame(itemLabels(Data))
ProductLabels

write.csv(ProductLabels, file="ProductLabels.csv", row.names = F)

colnames(categories)[1] <- "Product"
colnames(categories)[2] <- "Category"


####--------- data transformation ----------------------------------------------------#### 


view(categories)
view(Data)

# we split main transactional data into categories
Data@itemInfo$category <- categories[,2]
Data_divided <- aggregate(Data, by = 'category')
itemLabels(Data_divided)

# we split main transactional data into b2b and b2c segments
b2b_segment <- Data[which(size(Data) > 3), ]
inspect(b2b_segment)

b2c_segment <- Data[which(size(Data) <= 3), ]
inspect(b2c_segment)

consume_alone <- Data[which(size(Data) == 1), ]
inspect(consume_alone)



####--------- Explorative Analysis ----------------------------------------------------#### 


#plotting the top 10 products 
itemFrequencyPlot(Data, topN = 10, type = "relative", 
                  col = colorRampPalette(brewer.pal(9, "Set1"))(10), 
                  main = "Products Relative Item Frequency Plot")

#plotting the top 10 categories 
itemFrequencyPlot(Data_divided, topN = 10, type = "relative", 
                  col = colorRampPalette(brewer.pal(9, "Set1"))(10), 
                  main = "Categories Relative Item Frequency Plot")

#plotting the top 10 products in B2B segment 
itemFrequencyPlot(b2b_segment, topN = 10, type = "relative",
                  col = colorRampPalette(brewer.pal(5, "PRGn"))(10), 
                  main = "B2B Relative Item Frequency Plot")

#plotting the top 10 products in B2C segment 
itemFrequencyPlot(b2c_segment, topN = 10, type = "relative",
                  col = colorRampPalette(brewer.pal(5, "RdBu"))(10), 
                  main = "B2C Relative Item Frequency Plot")

#plotting the top 10 products bought alone 
itemFrequencyPlot(consume_alone, topN = 10, type = "relative",
                  col = colorRampPalette(brewer.pal(5, "PRGn"))(10), 
                  main = "Products Consume Alone")

item.freq.Ab <- itemFrequency(Data, type = 'absolute') 
item.freq.Ab

item.freq.Re <- itemFrequency(Data, type = 'relative') 
item.freq.Re


####--------------------- Apriori Algorithm ------------------------------------------####  


# Rules by Products
rules <- apriori(data = Data,
                 parameter = list(support = 0.002, confidence =0.8))

redundant <- is.redundant(rules, measure = "confidence")
rules <- rules[!redundant]
summary(rules)


rules1 <- apriori(data = Data,
                  parameter = list(support = 0.01, confidence =0.5))

redundant1 <- is.redundant(rules1, measure = "confidence")
rules1 <- rules1[!redundant1]
summary(rules1)


iMacRule <- apriori(data = Data,
                    parameter = list(support = 0.001, confidence = 0.15),
                    appearance = list(default = "rhs", lhs = "iMac"))

hpLaptop.rule <- subset(iMacRule, items %in% 'HP Laptop')
inspect(hpLaptop.rule)


# Rules by Category 

rule_category <- apriori(data = Data_divided,
                         parameter = list(support = 0.002, confidence =0.8))

redundant_category <- is.redundant(rule_category, measure = "confidence")
rule_category <- rule_category[!redundant_category]
summary(rule_category)


rule_cat_display <- apriori(data = Data_divided,
                         parameter = list(support = 0.002, confidence =0.8),
                         appearance = list(default = "rhs", lhs = "display"))

rule_cat_display <- is.redundant(rule_category, measure = "confidence")
rule_cat_display <- rule_cat_display[!redundant_category]
summary(rule_cat_display)


rule_cat_PC <- apriori(data = Data_divided,
                            parameter = list(support = 0.005, confidence =0.5),
                            appearance = list(default = "rhs", lhs = "PC"))

rule_cat_PC <- is.redundant(rule_category, measure = "confidence")
rule_cat_PC <- rule_cat_PC[!redundant_category]
summary(rule_cat_PC)

rule_cat_laptop <- apriori(data = Data_divided,
                       parameter = list(support = 0.005, confidence =0.5),
                       appearance = list(default = "rhs", lhs = "laptop"))

rule_cat_laptop <- is.redundant(rule_category, measure = "confidence")
rule_cat_laptop <- rule_cat_laptop[!redundant_category]
summary(rule_cat_laptop)


####--------------------- Visualizing the results ------------------------------------------####


inspect(sort(rules, by = 'lift')[1:10])
inspect(sort(rules1, by = 'lift')[1:10])
inspect(sort(iMacRule, by = 'lift')[1:10])
inspect(sort(rule_category, by = 'lift')[1:10])

plot(iMacRule[1:10], engine = "interactive")
plot(iMacRule[1:10], method = "graph", )

plotly_arules(rule_category[1:10], method = "matrix")
plotly_arules(rules)
plotly_arules(rules, measure = c("support", "lift"), shading = "confidence")
plotly_arules(rules, method = "two-key plot")

#exploring rules using Shiny Library
ruleExplorer(rules)
ruleExplorer(rules1)
ruleExplorer(iMacRule)
ruleExplorer(rule_category)

# exploring rules using plotly 
plotly_arules(rule_category, jitter = 10, 
              marker = list(opacity = .7, size = 10, symbol = 1), 
              colors = c("blue", "green")) 


plot(rules [1:5], method = "graph", measure = "support", 
     shading = "lift", engine = "igraph", data = NULL, control = NULL)



