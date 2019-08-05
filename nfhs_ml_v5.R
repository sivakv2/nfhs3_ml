library(haven)
nfhs <- read_sav("NFHS 3 march 14 2018.sav")
nrow(NFHS_3_march_14_2018)

summary(as.factor(nfhs$socgrp))
library(dplyr)

# women data

library(haven)
NFHS_3_Women_selected_data_dec_2_2015 <- read_sav("NFHS 3 Women selected data dec 2 2015.sav")

nfhs_wd15 <- NFHS_3_Women_selected_data_dec_2_2015

nfhs_wd15$place_delivery

library (dplyr)

nfhs_wd15 %>% 
  mutate(pd = case_when(place_delivery <= 20 ~ 1,
                        (place_delivery >=21 & place_delivery <=30) ~ 2,
                        (place_delivery >=31 & place_delivery <=40) ~ 3,
                        (place_delivery >=41 & place_delivery <=50) ~ 4,
                        )
         )%>%
  select(place_delivery, state, pd)%>%
  group_by(pd)%>%
  filter(!is.na(pd))%>%
  summarize(x = n())
  

nfhs_wd15 %>% 
  mutate(pd = case_when(place_delivery <= 20 ~ 0,
                        (place_delivery >= 021 ~ 1))) %>%
  filter(!is.na(pd))%>%
  select (pd,place_delivery,TYPE_RESIDENCE,V149,AgeatMarriage,women_Occupation,womenAge_5year)


nfhs_wd15d <- nfhs_wd15 %>% 
  mutate(pd = case_when(place_delivery <= 20 ~ 0,
                        (place_delivery >= 21 ~ 1))) %>%
  filter(!is.na(pd))%>%
  select (pd,women_literate,TYPE_RESIDENCE,socgrp)
    
summary(as.factor(nfhs_wd15d$pd))
summary(as.factor(nfhs_wd15d$TYPE_RESIDENCE))
#summary(as.factor(nfhs_wd15d$V106))
# Set random seed. Don't remove this line
set.seed(1)

library(rpart)
# A decision tree classification model is built on the data
tree <- rpart(pd ~ ., data = nfhs_wd15d, method = "class")

# Use the predict() method to make predictions, assign to pred
pred <- predict(tree,nfhs_wd15d,type="class")

# Use the table() method to make the confusion matrix
table(nfhs_wd15d$pd,pred)

#train the data set

n <- nrow(nfhs_wd15d)
shuffled <- nfhs_wd15d[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Fill in the model that has been learned.
tree <- rpart(pd ~ ., data= train, method = "class")

# Predict the outcome on the test set with tree: pred
pred <- predict(tree, test,type="class")

# Calculate the confusion matrix: conf
conf <- table(test$pd,pred)

# Print this confusion matrix
conf

# decision tree

# The train and test set are loaded into your workspace.

# Set random seed. Don't remove this line
set.seed(1)

# Load the rpart, rattle, rpart.plot and RColorBrewer package
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Fill in the ___, build a tree model: tree
tree <- rpart(pd ~ ., data=train, method = "class")

# Draw the decision tree
#x


pruned <- prune(tree,cp=0.000001)

# Draw pruned
fancyRpartPlot(pruned)



set.seed(1)

tree <- rpart(pd ~ ., train, method = "class", control = rpart.control(cp=0.0000001))

fancyRpartPlot(tree)




