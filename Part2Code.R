#We will be using "1000-out1.csv"
#setwd("")

install.packages("arules") 
library(arules) 
install.packages("arulesViz")
library(arulesViz)


# Read 1000-out1.csv as transaction (For rule mining)
data_trans <- read.transactions("1000-out1.csv",sep=",", cols = 1, rm.duplicates = FALSE) 

#Dataframe with goods' names and IDs
id <- c(0:49)
name <- factor(c("Chocolate Cake","Lemon Cake","Casino Cake","Opera Cake","Strawberry Cake","Truffle Cake","Chocolate Eclair","Coffee Eclair","Vanilla Eclair","Napoleon Cake","Almond Tart","Apple Pie","Apple Tart","Apricot Tart","Berry Tart","Blackberry Tart","Blueberry Tart","Chocolate Tart","Cherry Tart","Lemon Tart","Pecan Tart","Ganache Cookie","Gongolals Cookie","Raspberry Cookie","Lemon Cookie","Chocolate Meringue","Vanilla Meringue","Marzipan Cookie","Tuile Cookie","Walnut Cookie","Almond Croissant","Apple Croissant","Apricot Croissant","Cheese Croissant","Chocolate Croissant","Apricot Danish","Apple Dansh","Almond Twist","Almond Bear Claw","Blueberry Danish","Lemonade","Raspberry Lemonade","Orange Juice","Green Tea","Bottled Water","Hot Coffee","Chocolate Coffee","Vanilla Frappucino","Cherry Soda","Single Espresso"))
goods_name <- data.frame(id, name)


#Discretization done by replacing all IDs with names for easier reading
#Match and replace good's IDs with their name using goods_name dataframe
data_trans@itemInfo$labels <- goods_name$name[match(data_trans@itemInfo$labels,goods_name$id)] 

#Association Rule Mining

#Algorithm : apriori
#Start timer
ptm <- proc.time()

#Set minimum support to 0.005,
rules.all <- apriori(data_trans ,list(supp = 0.005, conf = 0.8, minlen=2)) 
#Sort the rules by confidence
rules_sort<-sort(rules.all, by="confidence", decreasing=TRUE) 
summary(rules.all) 

#Remove redundant rules
subset.matrix <- is.subset(rules.all, rules.all)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.all[!redundant]
rules_pruned<-sort(rules.pruned, by="confidence", decreasing = TRUE)
summary(rules_pruned)
inspect(rules_pruned)
#Let's only look at the top 10 rules sorted by confidence
inspect(head(rules_ec_pruned, 10))

proc.time() - ptm
#End Timer

#Visualization of rules
plot(rules_pruned)
plot(rules_pruned, method="grouped")
plot(rules_pruned, method="graph", shading="confidence")
plot(rules_pruned, measure = c("support", "lift"), shading="confidence")

###########Calculate how many times each item appears###########
#Read 1000-out1.csv as table 
data1 <- read.table("1000-out1.csv", header = FALSE, sep = ",", row.names = 1, col.names = paste0("Item",seq(0,8)), fill = TRUE)

goods_name <- data.frame(id=c(0:49),name=c("Chocolate Cake","Lemon Cake","Casino Cake","Opera Cake","Strawberry Cake","Truffle Cake","Chocolate Eclair","Coffee Eclair","Vanilla Eclair","Napoleon Cake","Almond Tart","Apple Pie","Apple Tart","Apricot Tart","Berry Tart","Blackberry Tart","Blueberry Tart","Chocolate Tart","Cherry Tart","Lemon Tart","Pecan Tart","Ganache Cookie","Gongolals Cookie","Raspberry Cookie","Lemon Cookie","Chocolate Meringue","Vanilla Meringue","Marzipan Cookie","Tuile Cookie","Walnut Cookie","Almond Croissant","Apple Croissant","Apricot Croissant","Cheese Croissant","Chocolate Croissant","Apricot Danish","Apple Dansh","Almond Twist","Almond Bear Claw","Blueberry Danish","Lemonade","Raspberry Lemonade","Orange Juice","Green Tea","Bottled Water","Hot Coffee","Chocolate Coffee","Vanilla Frappucino","Cherry Soda","Single Espresso"))
data1$Item1 <- with(goods_name, name[match(data1$Item1, id)])
data1$Item2 <- with(goods_name, name[match(data1$Item2, id)])
data1$Item3 <- with(goods_name, name[match(data1$Item3, id)])
data1$Item4 <- with(goods_name, name[match(data1$Item4, id)])
data1$Item5 <- with(goods_name, name[match(data1$Item5, id)])
data1$Item6 <- with(goods_name, name[match(data1$Item6, id)])
data1$Item7 <- with(goods_name, name[match(data1$Item7, id)])
data1$Item8 <- with(goods_name, name[match(data1$Item8, id)])
head(data1)

##Visulaize the data through barplot:
data <- data.frame(sapply(data1,as.factor))
vec <- sapply(data, as.vector)
prodCount <- table(vec)
prodCount <- sort(prodCount, decreasing = TRUE)
barplot(prodCount, main="Occurence of items in transaction", ylab="Num of Occurences"  , las=2, cex.names=0.6, beside=FALSE, ylim = c(0,120), border="blue") 

####

#Identify rules based on previous plot, by taking the most popular item to identify which other items are bought alongside
##Most bought: Gongolals Cookie for 1000-out.csv
rules_pop1<-apriori(data_trans, parameter=list(supp=0.003,conf = 0.5), 
                   appearance = list(default="lhs",rhs="Gongolals Cookie"))
inspect(rules_pop1)
rules_pop2<-apriori(data_trans, parameter=list(supp=0.003,conf = 0.5), 
                   appearance = list(default="rhs",lhs="Gongolals Cookie"))
inspect(rules_pop2)


########################################################################################

#Mining using Eclat algorithm

#Start timer
ptm <- proc.time()

rules_eclat <- eclat(data_trans, parameter = list(supp = 0.005, minlen=2))
inspect(rules_eclat)
rule_ec.all <- ruleInduction(rules_eclat, data_trans, confidence=0.8)
rule_ec.all<-sort(rule_ec.all, by="support", decreasing = TRUE)
inspect(rule_ec.all)

#Remove redundant rules
subset.matrix <- is.subset(rule_ec.all, rules.all)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules_ec.pruned <- rule_ec.all[!redundant]
rules_ec_pruned<-sort(rules_ec.pruned, by="confidence", decreasing = TRUE)

#Let's only look at the top 10 rules sorted by confidence
inspect(head(rules_ec_pruned, 10))
summary(rules_ec_pruned)

proc.time() - ptm
#End Timer

#Visualization
plot(rules_ec_pruned)
plot(rules_ec_pruned, method="grouped")
plot(rules_ec_pruned, method="graph", shading="confidence")
plot(rules_ec_pruned, measure = c("support", "lift"), shading="confidence")
