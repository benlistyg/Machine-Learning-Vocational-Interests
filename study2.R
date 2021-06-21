library(readxl)
library(data.table)
library(randomForest)
library(dplyr)
library(caret)
library(tm)
library(fastNaiveBayes)

# Helper Functions --------------------------------------------------------

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

# Reading in data ---------------------------------------------------------

time_data <- read_xlsx('Time vocational interest data - OSF.xlsx', sheet = 3)

onet_data <-  read.csv(file = 'All_Career_Clusters.csv')

full_data <- left_join(
  time_data,
  onet_data, 
  by = c("code" = "Code")
) %>% 
  select(Career.Cluster, Career.Pathway, Occupation, everything()) %>% 
  data.frame()

processed_data <- full_data %>% 
  .[complete.cases(.),] %>% 
  filter(country == 'United States',
         employed == 'Yes') %>% 
  select(Career.Cluster, Career.Pathway, Occupation, question1:question20) %>% 
  mutate(R = question5 + question20 + question10 + question15,
         I = question6 + question16 + question19,
         A = question7 + question17 + question8,
         S = question8 + question18 + question9,
         E = question2 + question12,
         C = question3 + question4 + question13 + question14) %>% 
  select(Career.Cluster, Career.Pathway, Occupation,R:C) %>% 
  janitor::clean_names()

processed_data <- processed_data %>% 
  group_by(occupation) %>% 
  mutate(n1 = n()) %>% 
  group_by(career_cluster) %>% 
  mutate(n2 = n()) %>% 
  group_by(career_pathway) %>% 
  mutate(n3 = n()) %>% 
  filter(n1 > 1, n2 > 1, n3 > 1) %>% 
  ungroup(career_pathway)



# Career Cluster  ---------------------------------------------------------

# Setting seed

set.seed(04101994)

career_cluster <- select(processed_data, career_cluster, r:c)
career_cluster$career_cluster <- as.factor(as.character(career_cluster$career_cluster))

# LDA
cc_lda_fit <- caret::train(career_cluster ~ .,
                           select(career_cluster, career_cluster, r:e),
                           method = "lda",
                           trControl = trainControl(method = "cv",
                                                    number = 10,
                                                    savePredictions = 'final')
)
cc_lda_fit$results$Accuracy

# kNN

cc_knn_fit <- caret::train(career_cluster ~ .,
                           select(career_cluster, career_cluster, r:e),
                           method = "knn",
                           trControl = trainControl(method = "cv",
                                                    number = 10,
                                                    savePredictions = 'final')
)

# Random Forest

cc_RF_fit <- caret::train(x = select(career_cluster, -career_cluster),
                          y = career_cluster$career_cluster,
                          method = "ranger",
                          trControl = trainControl(method = "cv",
                                                   number = 10,
                                                   savePredictions = 'final')
)

# NB
NB <- career_cluster

#Randomly shuffle the data
NB<-NB[sample(nrow(NB)),]
NB<- data.frame(NB)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(NB)),breaks=10,labels=FALSE)

cc_nb_fit <- list()

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  NBIndexes <- which(folds==i,arr.ind=TRUE)
  NBData <- NB[NBIndexes, ]
  trainData <- NB[-NBIndexes, ]
  model       <- fastNaiveBayes(x = trainData[,-1], y = matrix(trainData[,1]))
  NBData$career_cluster <- as.factor(NBData$career_cluster)
  levels(NBData$career_cluster) <- levels(predict(model, select(NBData,-career_cluster)))
  cc_nb_fit[[i]] <- confusionMatrix(predict(model, select(NBData,-career_cluster)), 
                                    NBData$career_cluster)$overall
}

cc_nb_fit %>% do.call(rbind,.) %>% .[,1] %>% mean()

# MLR

cc_MLR_fit <- caret::train(x = select(career_cluster, -career_cluster),
                           y = career_cluster$career_cluster,
                           method = "multinom",
                           trControl = trainControl(method = "cv",
                                                    number = 10,
                                                    savePredictions = 'final'),
                           MaxNWts = 10000000
)

# Career Cluster Hit Rates ---------------------------------------------------------------

# arrange(data.frame(table(career_cluster$career_cluster)), -Freq)
#                                            CC  Freq
#          Business Management & Administration  5295
#                                     Marketing  4527
#                          Education & Training  3337
#                                Health Science  1903
# Science, Technology, Engineering & Mathematics 1508
#    Law, Public Safety, Corrections & Security  1506
# > 5295/25132
# 0.2106876

# Multinomial Logistic Regression
cc_MLR_fit$results$Accuracy %>% max()
# 0.2342032
# (0.2342032 - 0.2106876) / (1 - 0.2106876)
# 0.02979251

# Linear Discriminant Analysis
cc_lda_fit$results$Accuracy %>% max()
# 0.2282747
# (0.2282747 - 0.2106876) / (1 - 0.2106876)
# 0.02228155

# Random Forest
cc_RF_fit$results$Accuracy %>% max()
# 0.2124369
# (0.2124369 - 0.2106876) / (1 - 0.2106876)
# 0.002216233

# k-Nearest Neighbors
cc_knn_fit$results$Accuracy %>% max()
# 0.1897221
# (0.1897221 - 0.2106876) / (1 - 0.2106876)
# -0.02656173

# Naive Bayes
cc_nb_fit %>% do.call(rbind,.) %>% .[,1] %>% mean()
# 0.227877
# (0.227877 - 0.2106876) / (1 - 0.2106876)
# 0.02177769

# Career Cluster Chisquare Test ----------------------------------------------------------

MLR_acc <- unname(as.integer(as.numeric(cc_MLR_fit$pred$pred == cc_MLR_fit$pred$obs) %>% table %>% .[2]))
LDA_acc <- unname(as.integer(as.numeric(cc_lda_fit$pred$pred == cc_lda_fit$pred$obs) %>% table %>% .[2]))
kNN_acc <- unname(as.integer(as.numeric(cc_knn_fit$pred$pred == cc_knn_fit$pred$obs) %>% table %>% .[2]))
RF_acc <- unname(as.integer(as.numeric(cc_RF_fit$pred$pred == cc_RF_fit$pred$obs) %>% table %>% .[2]))
NB_acc <- unname(as.integer(nrow(career_cluster) * do.call(rbind, cc_nb_fit)[,1] %>% mean))

accuracy_vector <- c(MLR_acc,
                     LDA_acc,
                     RF_acc ,
                     kNN_acc,
                     NB_acc )

total_vector <- rep(nrow(career_cluster), 5)

# Omnibus Test

prop.test(accuracy_vector, total_vector)

# Pairwise

prop.test(c(MLR_acc, LDA_acc), c(nrow(career_cluster), nrow(career_cluster)))
prop.test(c(MLR_acc, RF_acc), c(nrow(career_cluster), nrow(career_cluster)))
prop.test(c(MLR_acc, kNN_acc), c(nrow(career_cluster), nrow(career_cluster)))
prop.test(c(MLR_acc, NB_acc), c(nrow(career_cluster), nrow(career_cluster)))

prop.test(c(LDA_acc, RF_acc), c(nrow(career_cluster), nrow(career_cluster)))
prop.test(c(LDA_acc, kNN_acc), c(nrow(career_cluster), nrow(career_cluster)))
prop.test(c(LDA_acc, NB_acc), c(nrow(career_cluster), nrow(career_cluster)))

prop.test(c(RF_acc, kNN_acc), c(nrow(career_cluster), nrow(career_cluster)))
prop.test(c(RF_acc, NB_acc), c(nrow(career_cluster), nrow(career_cluster)))

prop.test(c(kNN_acc, NB_acc), c(nrow(career_cluster), nrow(career_cluster)))

# Career Pathway ----------------------------------------------------------

career_pathway <- select(processed_data, career_pathway, r:c)

career_pathway <- select(processed_data, career_pathway, r:c)
career_pathway$career_pathway <- as.factor(as.character(career_pathway$career_pathway))

# LDA
cp_lda_fit <- caret::train(career_pathway ~ .,
                           select(career_pathway, career_pathway, r:e),
                           method = "lda",
                           trControl = trainControl(method = "cv",
                                                    number = 10,
                                                    savePredictions = 'final')
)

# kNN

cp_knn_fit <- caret::train(career_pathway ~ .,
                           select(career_pathway, career_pathway, r:e),
                           method = "knn",
                           trControl = trainControl(method = "cv",
                                                    number = 10,
                                                    savePredictions = 'final')
)

# Random Forest

cp_RF_fit <- caret::train(x = select(career_pathway, -career_pathway),
                          y = career_pathway$career_pathway,
                          method = "ranger",
                          trControl = trainControl(method = "cv",
                                                   number = 10,
                                                   savePredictions = 'final')
)

# NB
NB <- career_pathway

#Randomly shuffle the data
NB<-NB[sample(nrow(NB)),]
NB<- data.frame(NB)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(NB)),breaks=10,labels=FALSE)

cp_nb_fit <- list()

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  NBIndexes <- which(folds==i,arr.ind=TRUE)
  NBData <- NB[NBIndexes, ]
  trainData <- NB[-NBIndexes, ]
  model       <- fastNaiveBayes(x = trainData[,-1], y = matrix(trainData[,1]))
  NBData$career_pathway <- as.factor(NBData$career_pathway)
  levels(NBData$career_pathway) <- levels(predict(model, select(NBData,-career_pathway)))
  cp_nb_fit[[i]] <- confusionMatrix(predict(model, select(NBData,-career_pathway)), 
                                    NBData$career_pathway)$overall
}

cp_nb_fit %>% do.call(rbind,.) %>% .[,1] %>% mean()

# MLR

cp_MLR_fit <- caret::train(x = select(career_pathway, -career_pathway),
                           y = career_pathway$career_pathway,
                           method = "multinom",
                           trControl = trainControl(method = "cv",
                                                    number = 10,
                                                    savePredictions = 'final'),
                           MaxNWts = 10000000
)

# Career Pathway Hit Rates ---------------------------------------------------------------
arrange(data.frame(table(career_pathway$career_pathway)), -Freq)

      #                   Var1 Freq
      #      Teaching/Training 2323
      # Administrative Support 2288
      #     Professional Sales 2198
      #   Therapeutic Services 1389
      #   Marketing Management 1289

# > 2323/25132
# [1] 0.09243196

# Multinomial Logistic Regression
cp_MLR_fit$results$Accuracy %>% max()
# 0.1305523
# (0.1305523 - 0.09243196) / (1 - 0.09243196)
# 0.04200274

# Linear Discriminant Analysis
cp_lda_fit$results$Accuracy %>% max()
# 0.1336896
# (0.1336896 - 0.09243196) / (1 - 0.09243196)
# 0.04545956

# Random Forest
cp_RF_fit$results$Accuracy %>% max()
# 0.1050829
# (0.1050829 - 0.09243196) / (1 - 0.09243196)
# 0.01393938

# k-Nearest Neighbors
cp_knn_fit$results$Accuracy %>% max()
# 0.08447783
# (0.08447783 - 0.09243196) / (1 - 0.09243196)
# -0.008764224

# Naive Bayes
cp_nb_fit %>% do.call(rbind,.) %>% .[,1] %>% mean()
# 0.1251392
# (0.1251392 - 0.09243196) / (1 - 0.09243196)
# 0.03603833

# Career Pathway Chisquare Test ----------------------------------------------------------

MLR_acc <- unname(as.integer(as.numeric(cp_MLR_fit$pred$pred == cp_MLR_fit$pred$obs) %>% table %>% .[2]))
LDA_acc <- unname(as.integer(as.numeric(cp_lda_fit$pred$pred == cp_lda_fit$pred$obs) %>% table %>% .[2]))
kNN_acc <- unname(as.integer(as.numeric(cp_knn_fit$pred$pred == cp_knn_fit$pred$obs) %>% table %>% .[2]))
RF_acc <- unname(as.integer(as.numeric(cp_RF_fit$pred$pred == cp_RF_fit$pred$obs) %>% table %>% .[2]))
NB_acc <- unname(as.integer(nrow(career_pathway) * do.call(rbind, cp_nb_fit)[,1] %>% mean))

accuracy_vector <- c(MLR_acc,
                     LDA_acc,
                     RF_acc ,
                     kNN_acc,
                     NB_acc )

total_vector <- rep(nrow(career_cluster), 5)

# Omnibus Test

prop.test(accuracy_vector, total_vector)

# Pairwise

prop.test(c(MLR_acc, LDA_acc), c(nrow(career_pathway), nrow(career_pathway)))
prop.test(c(MLR_acc, RF_acc), c(nrow(career_pathway), nrow(career_pathway)))
prop.test(c(MLR_acc, kNN_acc), c(nrow(career_pathway), nrow(career_pathway)))
prop.test(c(MLR_acc, NB_acc), c(nrow(career_pathway), nrow(career_pathway)))

prop.test(c(LDA_acc, RF_acc), c(nrow(career_pathway), nrow(career_pathway)))
prop.test(c(LDA_acc, kNN_acc), c(nrow(career_pathway), nrow(career_pathway)))
prop.test(c(LDA_acc, NB_acc), c(nrow(career_pathway), nrow(career_pathway)))

prop.test(c(RF_acc, kNN_acc), c(nrow(career_pathway), nrow(career_pathway)))
prop.test(c(RF_acc, NB_acc), c(nrow(career_pathway), nrow(career_pathway)))

prop.test(c(kNN_acc, NB_acc), c(nrow(career_pathway), nrow(career_pathway)))

# Descriptives ------------------------------------------------------------

# Age

time_data <- read_xlsx('Time vocational interest data - OSF.xlsx', sheet = 3)

onet_data <-  read.csv(file = 'All_Career_Clusters.csv')

full_data <- left_join(
  time_data,
  onet_data, 
  by = c("code" = "Code")
) %>% 
  select(Career.Cluster, Career.Pathway, Occupation, everything()) %>% 
  data.frame()

processed_data <- full_data %>% 
  .[complete.cases(.),] %>% 
  filter(country == 'United States',
         employed == 'Yes') %>% 
  select(Career.Cluster, Career.Pathway, Occupation, question1:question20, age, gender) %>% 
  mutate(R = question5 + question20 + question10 + question15,
         I = question6 + question16 + question19,
         A = question7 + question17 + question8,
         S = question8 + question18 + question9,
         E = question2 + question12,
         C = question3 + question4 + question13 + question14 ) %>% 
  select(Career.Cluster, Career.Pathway, Occupation,R:C, age, gender,
         question5 , question20 , question10 , question15,
         question6 , question16 , question19,
         question7 , question17 , question8,
         question8 , question18 , question9,
         question2 , question12,
         question3 , question4 , question13 , question14 ) %>% 
  janitor::clean_names()

processed_data <- processed_data %>% 
  group_by(occupation) %>% 
  mutate(n1 = n()) %>% 
  group_by(career_cluster) %>% 
  mutate(n2 = n()) %>% 
  group_by(career_pathway) %>% 
  mutate(n3 = n()) %>% 
  filter(n1 > 1, n2 > 1, n3 > 1) %>% 
  ungroup(career_pathway)

mean(processed_data$age[nchar(processed_data$age) == 2])
sd(processed_data$age[nchar(processed_data$age) == 2])

# Gender

processed_data$gender %>% table()
round((processed_data$gender %>% table())/sum(processed_data$gender %>% table()),2)

# Reliability

  # Item Info

    # 5  Install electrical wiring - R
    # 20 Carry and load containers - R
    # 10 Drive a bus - R
    # 15 Oversee building construction - R
    # 6  Categorize different types of wildlife - I
    # 16 Write a scientific article - I
    # 19 Study the effects of elections - I
    # 7  Sculpt a statue - A
    # 17 Paint a portrait - A
    # 8  Help children with learning problems - S
    # 18 Teach people to dance - S
    # 9  Give lecture to large groups - S
    # 2  Oversee a hotel - E
    # 12 Manage an office - E
    # 3  Prepare financial reports - C
    # 4  Oversee a data analysis group - C
    # 13 Maintain office financial records - C
    # 14 Manage an electrical power station - C
    
    # 11 Interview people for a survey - E/S
    # 1  Seat patrons at a restaurant - E/S

processed_data %>% 
  select(question5 , question20 , question10 , question15) %>% 
  psych::alpha()

processed_data %>% 
  select(question6 , question16 , question19) %>% 
  psych::alpha()

processed_data %>% 
  select( question7 , question17 , question8) %>% 
  psych::alpha()

processed_data %>% 
  select( question8 , question18 , question9) %>% 
  psych::alpha()

processed_data %>% 
  select(question2 , question12) %>% 
  psych::alpha()

processed_data %>% 
  select(question3 , question4 , question13 , question14) %>% 
  psych::alpha()

# Occupational Predictions ------------------------------------------------

occupation <- select(processed_data, occupation, r:c)
occupation <- select(processed_data, occupation, r:c)
occupation$occupation <- as.factor(as.character(occupation$occupation))

# LDA
occ_lda_fit <- caret::train(occupation ~ .,
                            select(occupation, occupation, r:e),
                            method = "lda",
                            trControl = trainControl(method = "cv",
                                                     number = 10,
                                                     savePredictions = 'final')
)

# kNN

occ_knn_fit <- caret::train(occupation ~ .,
                            select(occupation, occupation, r:e),
                            method = "knn",
                            trControl = trainControl(method = "cv",
                                                     number = 10,
                                                     savePredictions = 'final')
)

# Random Forest

occ_RF_fit <- caret::train(x = select(occupation, -occupation),
                           y = occupation$occupation,
                           method = "ranger",
                           trControl = trainControl(method = "cv",
                                                    number = 10,
                                                    savePredictions = 'final')
)

# NB
NB <- occupation

#Randomly shuffle the data
NB <- NB[sample(nrow(NB)),]
NB <- data.frame(NB)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(NB)),breaks=10,labels=FALSE)

occ_nb_fit <- list()

#Perform 10 fold cross validation
for(i in 1:10){
  print(i)
  #Segment your data by fold using the which() function 
  NBIndexes <- which(folds==i,arr.ind=TRUE)
  NBData <- NB[NBIndexes, ]
  NBData$occupation <- as.factor(NBData$occupation)
  trainData <- NB[-NBIndexes, ]
  model       <- fastNaiveBayes(x = trainData[,-1], y = matrix(trainData[,1]))
  output <- predict(model, select(NBData,-occupation), type = 'class')
  levels(output) <- levels(NBData$occupation)
  occ_nb_fit[[i]] <- confusionMatrix(output, as.factor(NBData$occupation))$overall
}

occ_nb_fit %>% do.call(rbind,.) %>% .[,1] %>% mean()

# MLR

occ_MLR_fit <- caret::train(x = select(occupation, -occupation),
                            y = occupation$occupation,
                            method = "multinom",
                            trControl = trainControl(method = "cv",
                                                     number = 10,
                                                     savePredictions = 'final'),
                            MaxNWts = 10000000
)

MLR_acc <- floor(0.042*nrow(occupation))
LDA_acc <- floor(0.044*nrow(occupation))
RF_acc <- floor(0.029*nrow(occupation))
kNN_acc <- floor(0.020*nrow(occupation))
NB_acc <- floor(0.030*nrow(occupation))

total_vector <- rep(nrow(occupation), 5)

# Omnibus Test

prop.test(accuracy_vector, total_vector)

# Pairwise

prop.test(c(MLR_acc, LDA_acc), c(nrow(occupation), nrow(occupation)))
prop.test(c(MLR_acc, RF_acc), c(nrow(occupation), nrow(occupation)))
prop.test(c(MLR_acc, kNN_acc), c(nrow(occupation), nrow(occupation)))
prop.test(c(MLR_acc, NB_acc), c(nrow(occupation), nrow(occupation)))

prop.test(c(LDA_acc, RF_acc), c(nrow(occupation), nrow(occupation)))
prop.test(c(LDA_acc, kNN_acc), c(nrow(occupation), nrow(occupation)))
prop.test(c(LDA_acc, NB_acc), c(nrow(occupation), nrow(occupation)))

prop.test(c(RF_acc, kNN_acc), c(nrow(occupation), nrow(occupation)))
prop.test(c(RF_acc, NB_acc), c(nrow(occupation), nrow(occupation)))

prop.test(c(kNN_acc, NB_acc), c(nrow(occupation), nrow(occupation)))
