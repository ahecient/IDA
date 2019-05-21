library(arules)
library(arulesViz)

#data("AdultUCI")

data("Groceries")

class(Groceries)

inspect(head(Groceries, 5))

rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))

##-------INFO ABOUT ALGO----
#minval is the minimum value of the support an itemset should satisfy to be a part of a rule.

#smax is the maximum support value for an itemset

#arem is an Additional Rule Evaluation Parameter. In the above code we have constrained the number of rules using Support and Confidence. 
#There are several other ways to constrain the rules using the arem parameter in the 
#function and we will discuss more about it later in the article

#aval is a logical indicating whether to return the additional rule evaluation measure selected with arem.

#originalSupport The traditional support value only considers both LHS and RHS items for calculating support.
#If you want to use only the LHS items for the calculation then you need to set this to FALSE

#maxtime is the maximum amount of time allowed to check for subsets

#minlen is the minimum number of items required in the rule

#maxlen is the maximum number of items that can be present in the rule.

##-------

inspect(head(sort(rules, by = "confidence"), 5))

wholemilk_rules <- apriori(data=Groceries, parameter=list (supp=0.001,conf = 0.08),
                           appearance = list (rhs="whole milk"))

grocery_rules_increased_support <- apriori(Groceries, parameter = list(support = 0.02, confidence = 0.5))



#--------------------

data <-  read.csv("Market_Basket_Optimisation.csv", header = FALSE)
head(data)

data <- read.transactions("Market_Basket_Optimisation.csv", sep =',', rm.duplicates = TRUE )

summary(data)

itemFrequencyPlot(data, topN=10)

##Training apriori

rules <-  apriori(data = data, parameter = list(support = 0.003 , confidence = 0.2) )

inspect(sort(rules, by = 'lift')[1:20])
