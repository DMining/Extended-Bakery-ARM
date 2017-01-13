# Association Rule Mining Assignment
##Extended Bakery Dataset

###Objectives: What is the domain and what are the potential benefits to be derived from association rule mining?
A bakery is a retail business which produces and sells products such as bread, cookies, cakes, and even beverages. One of the goals is to earn maximum profit. By understanding the patterns of how customers purchase in the bakery, they can know which product they should put more focus on. For the product with highest number of purchase among the items is the most popular product while product with the lowest number of purchase is the least popular product of the bakery. The bakery could do adjustments from the cost for the ingredients, more money could be given to buy the ingredients to produce more popular products while some budget for the least popular products could be cut off. This way, the bakery could increase their profit by producing more popular products to allow more purchase from customers, while decrease the amount of less popular products in order to prevent wastage and keeping their products fresh. Besides, by understanding the patterns of purchase by customers, the bakery could know what other products would likely be purchased along with the most popular products. Thus, they could do some arrangement for products in order to increase the total sales of the bakery. It is a physiological way to attract customers, but it is a strategy for them to increase sales from analyzing the patterns.

###Data Set Description: What is in the data, and what preprocessing was done to make it amenable for association rule mining. Where choices were made (e.g., parameter settings for discretization, or decisions to ignore an attribute), describe your reasoning behind the choices.
The dataset we are using is “1000-out1.csv”. It contains information about one year worth of sales information for bakery shops. The dataset chosen have 1000 instances. Each purchase is represented by a receipt number which is the first column in the data. Each item is represented by an item ID from 0 to 49 which totalled to 50 types of items. 
In preprocessing, item ID is replaced by item name to reduce confusion.

```r
# Read 1000-out1.csv as transaction (For rule mining)
data_trans <- read.transactions("1000-out1.csv",sep=",", cols = 1, rm.duplicates = FALSE) 

#Dataframe with goods' names and IDs
id <- c(0:49)
name <- factor(c("Chocolate Cake","Lemon Cake","Casino Cake","Opera Cake","Strawberry Cake","Truffle Cake","Chocolate Eclair","Coffee Eclair","Vanilla Eclair","Napoleon Cake","Almond Tart","Apple Pie","Apple Tart","Apricot Tart","Berry Tart","Blackberry Tart","Blueberry Tart","Chocolate Tart","Cherry Tart","Lemon Tart","Pecan Tart","Ganache Cookie","Gongolals Cookie","Raspberry Cookie","Lemon Cookie","Chocolate Meringue","Vanilla Meringue","Marzipan Cookie","Tuile Cookie","Walnut Cookie","Almond Croissant","Apple Croissant","Apricot Croissant","Cheese Croissant","Chocolate Croissant","Apricot Danish","Apple Dansh","Almond Twist","Almond Bear Claw","Blueberry Danish","Lemonade","Raspberry Lemonade","Orange Juice","Green Tea","Bottled Water","Hot Coffee","Chocolate Coffee","Vanilla Frappucino","Cherry Soda","Single Espresso"))
goods_name <- data.frame(id, name)

#Discretization done by replacing all IDs with names for easier reading
#Match and replace good's IDs with their name using goods_name dataframe
data_trans@itemInfo$labels <- goods_name$name[match(data_trans@itemInfo$labels,goods_name$id)] 
```
***
###Rule Mining Process and Resulting Rules
We will use *apriori algorithm* and *eclat algorithm* for rule mining, and show the top 10 rules sorted by confidence. ```proc.time()``` is used to determine the time used for the algorithm.

First use __apriori algorithm__ for rule mining: 

```r
ptm <- proc.time()

#Set minimum support to 0.005, confidence to 0.8.
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
inspect(rules_pruned)
#Let's only look at the top 10 rules sorted by confidence
inspect(head(rules_ec_pruned, 10))

proc.time() - ptm
#End Timer
```
Output:
```r
> inspect(head(rules_ec_pruned, 10))
     lhs                                               rhs                  support confidence
[1]  {Apricot Croissant,Hot Coffee}                 => {Blueberry Tart}     0.032   1.0000000 
[2]  {Apple Croissant,Apple Dansh,Cherry Soda}      => {Apple Tart}         0.031   1.0000000 
[3]  {Apple Tart,Apple Dansh,Cherry Soda}           => {Apple Croissant}    0.031   1.0000000 
[4]  {Apple Tart,Apple Croissant,Cherry Soda}       => {Apple Dansh}        0.031   1.0000000 
[5]  {Raspberry Cookie,Raspberry Lemonade}          => {Lemon Cookie}       0.029   1.0000000 
[6]  {Lemon Cookie,Lemonade,Raspberry Lemonade}     => {Raspberry Cookie}   0.028   1.0000000 
[7]  {Raspberry Cookie,Lemonade,Raspberry Lemonade} => {Lemon Cookie}       0.028   1.0000000 
[8]  {Raspberry Cookie,Lemon Cookie,Lemonade}       => {Raspberry Lemonade} 0.028   1.0000000 
[9]  {Apple Tart,Apple Dansh}                       => {Apple Croissant}    0.040   0.9756098 
[10] {Casino Cake,Chocolate Coffee}                 => {Chocolate Cake}     0.038   0.9743590 
     lift     itemset
[1]  12.34568 228    
[2]  12.65823 331    
[3]  10.98901 331    
[4]  11.90476 331    
[5]  15.15152 262    
[6]  12.19512 159    
[7]  15.15152 159    
[8]  13.88889 159    
[9]  10.72099 334    
[10] 11.59951 219    
> 
> proc.time() - ptm
   user  system elapsed 
   0.15    0.01    0.25 
```
Visualize the rules from apriori algorithm
```r
plot(rules_pruned)
```
![VisRule](https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/a_rulespruned.png?raw=true)
```r
plot(rules_pruned, method="grouped")
```
![VisRule](https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/a_rulespruned_grouped.png?raw=true)
```r
plot(rules_pruned, method="graph", shading="confidence")
```
![VisRule](https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/a_rulespruned_graph_conf.png?raw=true)

```r
plot(rules_pruned, measure = c("support", "lift"), shading="confidence")
```
![VisRule](https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/a_rulespruned-supp-lift_conf.png?raw=true)

***
We then try it with **eclat algorithm**
```r
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
```
Output:
```r
> inspect(head(rules_ec_pruned, 10))
     lhs                                               rhs                  support confidence
[1]  {Apricot Croissant,Hot Coffee}                 => {Blueberry Tart}     0.032   1.0000000 
[2]  {Apple Croissant,Apple Dansh,Cherry Soda}      => {Apple Tart}         0.031   1.0000000 
[3]  {Apple Tart,Apple Dansh,Cherry Soda}           => {Apple Croissant}    0.031   1.0000000 
[4]  {Apple Tart,Apple Croissant,Cherry Soda}       => {Apple Dansh}        0.031   1.0000000 
[5]  {Raspberry Cookie,Raspberry Lemonade}          => {Lemon Cookie}       0.029   1.0000000 
[6]  {Lemon Cookie,Lemonade,Raspberry Lemonade}     => {Raspberry Cookie}   0.028   1.0000000 
[7]  {Raspberry Cookie,Lemonade,Raspberry Lemonade} => {Lemon Cookie}       0.028   1.0000000 
[8]  {Raspberry Cookie,Lemon Cookie,Lemonade}       => {Raspberry Lemonade} 0.028   1.0000000 
[9]  {Apple Tart,Apple Dansh}                       => {Apple Croissant}    0.040   0.9756098 
[10] {Casino Cake,Chocolate Coffee}                 => {Chocolate Cake}     0.038   0.9743590 
     lift     itemset
[1]  12.34568 228    
[2]  12.65823 331    
[3]  10.98901 331    
[4]  11.90476 331    
[5]  15.15152 262    
[6]  12.19512 159    
[7]  15.15152 159    
[8]  13.88889 159    
[9]  10.72099 334    
[10] 11.59951 219    
> summary(rules_ec_pruned)
set of 40 rules

rule length distribution (lhs + rhs):sizes
 3  4 
33  7 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.000   3.000   3.000   3.175   3.000   4.000 

summary of quality measures:
    support         confidence          lift           itemset     
 Min.   :0.0240   Min.   :0.8000   Min.   : 8.511   Min.   :159.0  
 1st Qu.:0.0280   1st Qu.:0.9032   1st Qu.:11.136   1st Qu.:161.8  
 Median :0.0310   Median :0.9447   Median :11.980   Median :228.0  
 Mean   :0.0313   Mean   :0.9332   Mean   :12.132   Mean   :238.8  
 3rd Qu.:0.0320   3rd Qu.:0.9744   3rd Qu.:12.783   3rd Qu.:331.0  
 Max.   :0.0400   Max.   :1.0000   Max.   :15.152   Max.   :341.0  

mining info:
       data ntransactions support confidence
 data_trans          1000   0.005        0.8
> 
> proc.time() - ptm
   user  system elapsed 
   0.25    0.02    0.34 
```

Visualize the rules from eclat algorithm
```r
plot(rules_ec_pruned)
```
![VisRule](https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/e_rulesEcPruned.png?raw=true)
```r
plot(rules_ec_pruned, method="grouped")
```
![VisRule](https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/e_rulesEcPruned_grouped1.png?raw=true)
```r
plot(rules_ec_pruned, method="graph", shading="confidence")
```
![VisRule](https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/e_rulesEcPruned_graph_conf.png?raw=true)
```r
plot(rules_ec_pruned, measure = c("support", "lift"), shading="confidence")
```
![VisRule](https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/e_rulesEcPruned-supp-lift_conf.png?raw=true)

We can also identify the most bought item from the transactions, and use ARM to discover the items that are bought frequently with it.
```r
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
```
Plot: 
<p align="center">
     <img src="https://github.com/DMining/Extended-Bakery-ARM/blob/master/Images/barplot_occurrentOfTransaction%20.png?raw=true"/>
</p>
From the plot, we can see that Gongolals Cookie appears most often in transactions for "1000-out1.csv"
```r
##Most bought: Gongolals Cookie for 1000-out.csv
rules_pop1<-apriori(data_trans, parameter=list(supp=0.003,conf = 0.5), 
                   appearance = list(default="lhs",rhs="Gongolals Cookie"))
rules_pop2<-apriori(data_trans, parameter=list(supp=0.003,conf = 0.5), 
                   appearance = list(default="rhs",lhs="Gongolals Cookie"))
inspect(rules_pop1)
inspect(rules_pop2)
```
Output:
```r
> inspect(rules_pop1)
     lhs                                  rhs                support confidence lift    
[1]  {Truffle Cake}                    => {Gongolals Cookie} 0.058   0.5631068  5.213952
[2]  {Chocolate Meringue,Truffle Cake} => {Gongolals Cookie} 0.003   0.7500000  6.944444
[3]  {Apricot Tart,Truffle Cake}       => {Gongolals Cookie} 0.003   0.6000000  5.555556
[4]  {Green Tea,Truffle Cake}          => {Gongolals Cookie} 0.004   0.6666667  6.172840
[5]  {Almond Twist,Truffle Cake}       => {Gongolals Cookie} 0.003   1.0000000  9.259259
[6]  {Lemon Tart,Truffle Cake}         => {Gongolals Cookie} 0.003   0.5000000  4.629630
[7]  {Casino Cake,Cheese Croissant}    => {Gongolals Cookie} 0.003   0.5000000  4.629630
[8]  {Vanilla Frappucino,Truffle Cake} => {Gongolals Cookie} 0.004   0.8000000  7.407407
[9]  {Lemon Cake,Truffle Cake}         => {Gongolals Cookie} 0.003   0.5000000  4.629630
[10] {Truffle Cake,Coffee Eclair}      => {Gongolals Cookie} 0.004   0.5714286  5.291005
[11] {Apple Croissant,Truffle Cake}    => {Gongolals Cookie} 0.006   0.6000000  5.555556
> inspect(rules_pop2)
    lhs                   rhs            support confidence lift    
[1] {Gongolals Cookie} => {Truffle Cake} 0.058   0.537037   5.213952
```

###Recommendations
After identifying the items that are frequently bought together, the bakery shop owner can invest more in buying the ingredients for the popular products. From the result found in the rules, we can see that customers who bought Apricot Croissant and Hot Coffee would also buy Blueberry Tart. While customers who bought Apple Croissant, Apple Danish and Cherry Soda would buy Apple Tart. The result shows there is a high probability that customers would likely buy Apple Croissant and a beverage along with tarts such as Blueberry Tart or Apple Tart. The bakery can rearrange in a way that the related products are put closely to each other. Beverages can be put at some where obvious and with ease of access, for instance near to the cashier. This is because most of the customers would buy a beverage along with other products but the beverage varies. From the results, customers who bought Raspberry Cookie and Raspberry Lemonade would buy Lemon Cookie. On the other hand, customers who bought Lemon Cookie, Lemonade and Raspberry Lemonade would also buy Raspberry Cookie. It shows that customers tend to buy raspberry products along with lemon products. The bakery can focus on innovate more products using raspberry and lemon to attract customers to purchase.


