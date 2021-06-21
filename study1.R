library(data.table)
library(randomForest)
library(dplyr)
library(caret)
library(tm)
library(reshape2)
library(fastNaiveBayes)
library(ranger)

# Helper functions (redundant step) ---------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

# Reading in data ---------------------------------------------------------

educational_choice <- fread('Study 1/RIASEC_data12Dec2018/data.csv', sep = '\t', na.strings = "NA") %>%
  filter(education %in% c(3,4), # Completed college or graduate degree
         country == 'US') %>%  # Limited to US
  mutate(major = major %>% trimws() %>% removePunctuation() %>% toupper()) %>% 
  filter(nchar(major) > 1, 
         !grepl('AND', major)) %>% 
  select(R1:C8, major, gender, age, race) #28320 rows

prejoin_rows <- nrow(educational_choice)

educational_choice$major <- gsub(' +',' ',educational_choice$major) 

educational_choice <- left_join(educational_choice, combined_majors, by = c("major" = "original_major")) %>% 
  data.table() # 28320 rows

postjoin_rows <- nrow(educational_choice)

# prejoin_rows == postjoin_rows # SHOULD BE TRUE

educational_choice <- educational_choice %>% 
  mutate(R = rowSums(dplyr::select(., grep("R[0-9]", names(.)))),
         I = rowSums(dplyr::select(., grep("I[0-9]", names(.)))),
         A = rowSums(dplyr::select(., grep("A[0-9]", names(.)))),
         S = rowSums(dplyr::select(., grep("S[0-9]", names(.)))),
         E = rowSums(dplyr::select(., grep("E[0-9]", names(.)))),
         C = rowSums(dplyr::select(., grep("C[0-9]", names(.))))) %>% 
  dplyr::select(R,I,A,S,E,C,everything(),Specific.Major,General.Major) %>% 
  janitor::clean_names()

# table(is.na(educational_choice$general_major), 
#       is.na(educational_choice$specific_major))

#       FALSE  TRUE
# FALSE  1885  1819
# TRUE      0 24616

educational_choice <- educational_choice[!is.na(educational_choice$specific_major) & !is.na(educational_choice$general_major),] #24616

educational_choice <- educational_choice %>% group_by(specific_major) %>% mutate(n = n()) %>% filter(n > 1) # 24610

educational_choice$specific_major <- as.factor(educational_choice$specific_major)

educational_choice$general_major <- as.factor(educational_choice$general_major)

educational_choice <- data.frame(educational_choice)

# General Major Prediction ------------------------------------------------

# Set seed for reproducing output

set.seed(19940410)

general <- select(educational_choice, r:c, general_major) %>% 
  .[!is.na(.$general_major),]

general_edu_LDA_fit <- caret::train(x = select(general, -general_major),
                                    y = general$general_major,
                                    method = "lda",
                                    trControl = trainControl(method = "cv",
                                                             number = 10, 
                                                             savePredictions = 'final')
)

# kNN

general_edu_kNN_fit <- caret::train(x = select(general, -general_major),
                                    y = general$general_major,
                                    method = "knn",
                                    trControl = trainControl(method = "cv",
                                                             number = 10, 
                                                             savePredictions = 'final')
)

# Random Forest

general_edu_RF_fit <- caret::train(x = select(general, -general_major),
                                   y = general$general_major,
                                   method = "ranger",
                                   trControl = trainControl(method = "cv",
                                                            number = 10, 
                                                            savePredictions = 'final')
)

# NB

NB <- general

#Randomly shuffle the data
NB<-NB[sample(nrow(NB)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(NB)),breaks=10,labels=FALSE)

general_edu_NB_fit <- list()

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  NBIndexes <- which(folds==i,arr.ind=TRUE)
  NBData <- NB[NBIndexes, ]
  trainData <- NB[-NBIndexes, ]
  model       <- fastNaiveBayes(x = trainData[,-7], y = matrix(trainData[,7]))
  general_edu_NB_fit[[i]] <- confusionMatrix(predict(model, select(NBData,-general_major)), NBData$general_major)$overall
}

# MLR

general_edu_MLR_fit <- caret::train(x = select(general, -general_major),
                                    y = general$general_major,
                                    method = "multinom",
                                    trControl = trainControl(method = "cv",
                                                             number = 10, 
                                                             savePredictions = 'final'),
                                    MaxNWts = 10000000
)


# Hit Rates ---------------------------------------------------------------
# > 8488/24610
# [1] 0.3449004

# Multinomial Logistic Regression
general_edu_MLR_fit$results$Accuracy
# 0.3753356
# (0.3753356 - 0.3449004) /
#   (1 - 0.3449004)
# 0.04645889

# Linear Discriminant Analysis
general_edu_LDA_fit$results$Accuracy
# 0.376799
# > (0.37699 - 0.3449004) /
#   +     (1 - 0.3449004)
# [1] 0.04898431

# Random Forest
general_edu_RF_fit$results$Accuracy %>% max()
# 0.3637163
# (0.3637163 - 0.3449004) /
#   +     (1 - 0.3449004)
# [1] 0.0287222

# k-Nearest Neighbors
general_edu_kNN_fit$results
# 0.3216611
# > (0.3216611 - 0.3449004) /
#   +   +     (1 - 0.3449004)
# [1] -0.03547445

# Naive Bayes
general_edu_NB_fit %>% do.call(rbind,.) %>% .[,1] %>% mean()
# 0.3443722
# > (0.3443722 - 0.3449004) /
#   +   +     (1 - 0.3449004)
# [1] -0.0008062896

# Chi-Squared Test of Comparisons -----------------------------------------

MLR_acc <- unname(as.integer(as.numeric(general_edu_MLR_fit$pred$pred == general_edu_MLR_fit$pred$obs) %>% table %>% .[2]))
LDA_acc <- unname(as.integer(as.numeric(general_edu_LDA_fit$pred$pred == general_edu_LDA_fit$pred$obs) %>% table %>% .[2]))
kNN_acc <- unname(as.integer(as.numeric(general_edu_kNN_fit$pred$pred == general_edu_kNN_fit$pred$obs) %>% table %>% .[2]))
RF_acc <- unname(as.integer(as.numeric(general_edu_RF_fit$pred$pred == general_edu_RF_fit$pred$obs) %>% table %>% .[2]))
NB_acc <- unname(as.integer(nrow(general) * do.call(rbind, general_edu_NB_fit)[,1] %>% mean))

accuracy_vector <- c(MLR_acc,
                     LDA_acc,
                     RF_acc ,
                     kNN_acc,
                     NB_acc )

total_vector <- rep(nrow(general), 5)

# Omnibus Test

prop.test(accuracy_vector, total_vector)

# Pairwise

prop.test(c(MLR_acc, LDA_acc), c(nrow(general), nrow(general)))
prop.test(c(MLR_acc, RF_acc), c(nrow(general), nrow(general)))
prop.test(c(MLR_acc, kNN_acc), c(nrow(general), nrow(general)))
prop.test(c(MLR_acc, NB_acc), c(nrow(general), nrow(general)))

prop.test(c(LDA_acc, RF_acc), c(nrow(general), nrow(general)))
prop.test(c(LDA_acc, kNN_acc), c(nrow(general), nrow(general)))
prop.test(c(LDA_acc, NB_acc), c(nrow(general), nrow(general)))

prop.test(c(RF_acc, kNN_acc), c(nrow(general), nrow(general)))
prop.test(c(RF_acc, NB_acc), c(nrow(general), nrow(general)))

prop.test(c(kNN_acc, NB_acc), c(nrow(general), nrow(general)))

# Descriptives ------------------------------------------------------------

# Gender
# "What is your gender?", 1=Male, 2=Female, 3=Other

round((educational_choice$gender %>% table()) / nrow(educational_choice),2)
educational_choice$gender %>% table()

# Race
# "What is your race?", 1=Asian, 2=Arab, 3=Black, 4=Indigenous Australian / Native American / White, 5=Other (There was a coding error in the survey, and three different options were given the same value)

round((educational_choice$race %>% table()) / nrow(educational_choice),2)
educational_choice$race %>% table()

educational_choice$age[nchar(educational_choice$age) == 2] %>% mean()
educational_choice$age[nchar(educational_choice$age) == 2] %>% sd()

# Reliability

educational_choice %>% 
  select(r1:r8) %>% 
  psych::alpha()

educational_choice %>% 
  select(i1:i8) %>% 
  psych::alpha()

educational_choice %>% 
  select(a1:a8) %>% 
  psych::alpha()

educational_choice %>% 
  select(s1:s8) %>% 
  psych::alpha()

educational_choice %>% 
  select(e1:e8) %>% 
  psych::alpha()

educational_choice %>% 
  select(c1:c8) %>% 
  psych::alpha()

# Specific Major Prediction -----------------------------------------------

# LDA

Specific <- select(educational_choice, r:c, specific_major) %>% .[!is.na(.$specific_major),]

specific_edu_LDA_fit <- caret::train(x = select(Specific, -specific_major),
                                     y = Specific$specific_major,
                                     method = "lda",
                                     trControl = trainControl(method = "cv",
                                                              number = 10, 
                                                              savePredictions = 'final')
)

# kNN

specific_edu_kNN_fit <- caret::train(x = select(Specific, -specific_major),
                                     y = Specific$specific_major,
                                     method = "knn",
                                     trControl = trainControl(method = "cv",
                                                              number = 10, 
                                                              savePredictions = 'final')
)

# Random Forest

specific_edu_RF_fit <- caret::train(x = select(Specific, -specific_major),
                                    y = Specific$specific_major,
                                    method = "ranger",
                                    trControl = trainControl(method = "cv",
                                                             number = 10, 
                                                             savePredictions = 'final')
)

# NB

NB <- Specific

#Randomly shuffle the data
NB<-NB[sample(nrow(NB)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(NB)),breaks=10,labels=FALSE)

specific_edu_NB_fit <- list()

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  NBIndexes <- which(folds==i,arr.ind=TRUE)
  NBData <- NB[NBIndexes, ]
  trainData <- NB[-NBIndexes, ]
  model       <- fastNaiveBayes(x = trainData[,-7], y = matrix(trainData[,7]))
  specific_edu_NB_fit[[i]] <- confusionMatrix(predict(model, select(NBData,-specific_major)), NBData$specific_major)$overall
}

# MLR

specific_edu_MLR_fit <- caret::train(x = select(Specific, -specific_major),
                                     y = Specific$specific_major,
                                     method = "multinom",
                                     trControl = trainControl(method = "cv",
                                                              number = 10, 
                                                              savePredictions = 'final'),
                                     MaxNWts = 10000000
)

# specific_edu_LDA_fit$results
# specific_edu_kNN_fit$results
# specific_edu_RF_fit$results
# specific_edu_NB_fit %>% do.call(rbind,.) %>% .[,1] %>% mean()
# specific_edu_MLR_fit$results

# Chi-Squared Test of Comparisons -----------------------------------------

MLR_acc <- floor(0.172*nrow(Specific))
LDA_acc <- floor(0.262*nrow(Specific))
RF_acc <- floor(0.249*nrow(Specific))
kNN_acc <- floor(0.209*nrow(Specific))
NB_acc <- floor(0.231*nrow(Specific))

total_vector <- rep(nrow(Specific), 5)

# Omnibus Test

prop.test(accuracy_vector, total_vector)

# Pairwise

prop.test(c(MLR_acc, LDA_acc), c(nrow(Specific), nrow(Specific)))
prop.test(c(MLR_acc, RF_acc), c(nrow(Specific), nrow(Specific)))
prop.test(c(MLR_acc, kNN_acc), c(nrow(Specific), nrow(Specific)))
prop.test(c(MLR_acc, NB_acc), c(nrow(Specific), nrow(Specific)))

prop.test(c(LDA_acc, RF_acc), c(nrow(Specific), nrow(Specific)))
prop.test(c(LDA_acc, kNN_acc), c(nrow(Specific), nrow(Specific)))
prop.test(c(LDA_acc, NB_acc), c(nrow(Specific), nrow(Specific)))

prop.test(c(RF_acc, kNN_acc), c(nrow(Specific), nrow(Specific)))
prop.test(c(RF_acc, NB_acc), c(nrow(Specific), nrow(Specific)))

prop.test(c(kNN_acc, NB_acc), c(nrow(Specific), nrow(Specific)))