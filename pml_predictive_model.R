library(tidyverse)
pmltest <- read_csv("pml-testing.csv")
pmltrain <- read_csv("pml-training.csv")
pmltrain
dim(pmltrain)
any(is.na(pmltrain))
sum(is.na(pmltrain))
na_ <- function(x) ! sum(is.na(x))/length(x) > 0.6
pmltrain <- pmltrain %>% select(where(na_))
pmltrain
pmltrain <- subset(pmltrain[,-c(1:5)])
pmltrain_num <- pmltrain %>% select(where(is.numeric))
correlation <- cor(pmltrain_num)
diag(correlation) <- 0
correlation[upper.tri(correlation)] <- 0
any(abs(correlation) > 0.6)
abs_thr <- function(datafrm , thr){
        col = c()
        for(i in 1:length(names(datafrm))){
                for(j in 1:i){
                        if (abs(datafrm[i,j]) > thr){
                                col = c(col,names(datafrm[i,j]))
                        }
                }
        }
        return(unique(col))
}
dt <- data.frame(correlation)
dt <- tibble(dt)
abs_thr(dt,0.7)
col <- abs_thr(dt,0.7)
pmltrain <- pmltrain %>% select(-col)
pmltrain
pmltrain$new_window <- 1*(pmltrain$new_window == "yes")

library(caret)
model <- train(classe~.,data = pmltrain,method = 'rpart')
confusionMatrix(factor(pmltrain$classe),predict(model,pmltrain_totest))
model2 <- train(classe~.,data = pmltrain,method = 'naive_bayes')
confusionMatrix(factor(pmltrain$classe),predict(model2,pmltrain_totest))
model3 <- train(classe~.,data = pmltrain,method = 'lda')
confusionMatrix(factor(pmltrain$classe),predict(model3,pmltrain_totest))
