#Facebook metrics
#Alfonso Berumen
#SatRday

##############
#packages
##############
#for some prepping
#install.packages("dplyr")
library(dplyr)

#for EDA
#install.packages("ggplot2")
library(ggplot2)

#for error metrics
#install.packages("MLmetrics")
library(MLmetrics)

#install.packages("stargazer")
library(stargazer)

#for a pretty plot
#install.packages("lattice")
library(lattice)

#for a regression tress
#install.packages("rpart")
library(rpart)
#install.packages("rattle")
library(rattle)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("RColorBrewer")
library(RColorBrewer)

##################################################
#set working directory
setwd("/Users/alfonsoberumen/Desktop/SatRDay")
##################################################

#import data from the UCI machine learning repository
#https://archive.ics.uci.edu/ml/datasets/Facebook+metrics
#facebook metrics data
facebook <- read.csv("Facebook_metrics/dataset_Facebook.csv",
                     sep = ";",
                  header = TRUE)
glimpse(facebook)
str(facebook)#if you don't have dplyr
summary(facebook)

#some NAs; let's use complete data
facebook <- na.omit(facebook)#excluded 5 records 
summary(facebook)

#category: {action, product, inspiration}
#as indicated by the manager
#however there is no information on the numbers; 1? 2? 3?
#let's just use it as a factor for now, ignoring the description

#create factors
facebook$Category <- as.factor(facebook$Category)
facebook$Post.Month <- as.factor(facebook$Post.Month)
facebook$Post.Weekday <- as.factor(facebook$Post.Weekday)
facebook$Post.Hour <- as.factor(facebook$Post.Hour)
facebook$Paid <- as.factor(facebook$Paid)
#->See list and descriptions from publication
glimpse(facebook)

##################
#BACK TO THE PPT
##################
#for illustrative purposes
#let's look at the model the author analyzed in depth
#target: Lifetime Post Consumers
##################
#EDA
##################
#quick look at Lifetime.Post.Consumers
boxplot(facebook$Lifetime.Post.Consumers)
#drop records over 3000-11 additional records
facebook_excl<-facebook %>%
              filter(Lifetime.Post.Consumers<=3000)
11/495

#EDA BASED ON TARGET
#seven possible features:
#Type
p1<-ggplot(facebook_excl, aes(x=Type, y=Lifetime.Post.Consumers, 
                        color=Type)) +
  geom_boxplot()
p1
table(facebook_excl$Type)

#Category
p2<-ggplot(facebook_excl, aes(x=Category, y=Lifetime.Post.Consumers, 
                        color=Category)) +
  geom_boxplot()
p2
table(facebook_excl$Category)

#Post month
p3<-ggplot(facebook_excl, aes(x=Post.Month, y=Lifetime.Post.Consumers, 
                         color=Post.Month)) +
  geom_boxplot()
p3
table(facebook_excl$Post.Month)

#Post hour
p4<-ggplot(facebook_excl, aes(x=Post.Hour, y=Lifetime.Post.Consumers, 
                              color=Post.Hour)) +
  geom_boxplot()
p4
table(facebook_excl$Post.Hour)

#Post weekday
p5<-ggplot(facebook_excl, aes(x=Post.Weekday, y=Lifetime.Post.Consumers, 
                              color=Post.Weekday)) +
  geom_boxplot()
p5
table(facebook_excl$Post.Weekday)

#Paid
p6<-ggplot(facebook_excl, aes(x=Paid, y=Lifetime.Post.Consumers, 
                              color=Paid)) +
  geom_boxplot()
p6
table(facebook_excl$Paid)

#Page total likes
ggplot(facebook_excl, 
       aes(x=Page.total.likes, 
       y=Lifetime.Post.Consumers)) +
  geom_point(color='blue')
cor(facebook_excl$Page.total.likes,
    facebook_excl$Lifetime.Post.Consumers)
#well we know it the data covers a year: 2014
p7<-ggplot(facebook_excl, aes(x=Post.Month, 
                              y=Page.total.likes, 
                              color=Post.Month)) +
  geom_boxplot()
p7
##############################
#let's add a couple variables
##############################
Like_change<-facebook_excl %>% 
  group_by(Post.Month) %>% 
  summarise(Page.total.likes = max(Page.total.likes)) %>%
  mutate(pct_change = (Page.total.likes/lag(Page.total.likes) - 1) * 100)
Like_change<-Like_change %>%
  mutate(pct_change=ifelse(is.na(pct_change),0,pct_change))%>%
  select(-c(Page.total.likes))

################
#merge back on
################
facebook_excl<-facebook_excl%>%
  left_join(Like_change,
            by = c("Post.Month"))
#hour variable
facebook_excl<-facebook_excl%>%
  mutate(Post.Hour.Block=as.factor(ifelse(
    Post.Hour=="1"|Post.Hour=="2"|Post.Hour=="3"|Post.Hour=="4","1) 1am-4am",
    ifelse(
    Post.Hour=="5"|Post.Hour=="6"|Post.Hour=="7"|Post.Hour=="8","2) 5am-8am",
    ifelse(
    Post.Hour=="9"|Post.Hour=="10"|Post.Hour=="11"|Post.Hour=="12","3) 9am-12pm",
    ifelse(
    Post.Hour=="13"|Post.Hour=="14","4) 1pm-2pm",
    "5) 3pm-11pm"))))))
table(facebook_excl$Post.Hour,
      facebook_excl$Post.Hour.Block)

#qtr variable
facebook_excl<-facebook_excl%>%
  mutate(Post.Qtr=as.factor(ifelse(
    Post.Month=="1"|Post.Month=="2"|Post.Month=="3","1) Q1",
    ifelse(
    Post.Month=="4"|Post.Month=="5"|Post.Month=="6","2) Q2",
    ifelse(
    Post.Month=="7"|Post.Month=="8"|Post.Month=="9","3) Q3",
    "4) Q4")))))
      
table(facebook_excl$Post.Month,
      facebook_excl$Post.Qtr)
      
#####################################
#just take a look again at our target
#####################################
p<-ggplot(facebook_excl, aes(x=Lifetime.Post.Consumers)) + 
  geom_histogram(color="black", fill="blue",
                 binwidth = 50)
#add an average line
p+geom_vline(aes(xintercept=mean(Lifetime.Post.Consumers)),
             color="yellow", linetype="dashed", size=1)
summary(facebook_excl$Lifetime.Post.Consumers)

#####################################
#ADDED FEATURES
#####################################
#LIKE CHANGE
ggplot(facebook_excl, 
       aes(x=pct_change, 
           y=Lifetime.Post.Consumers)) +
  geom_point(color='orange')

#Post.Hour.Block
p8<-ggplot(facebook_excl, aes(x=Post.Hour.Block, 
                              y=Lifetime.Post.Consumers, 
                              color=Post.Hour.Block)) +
  geom_boxplot()
p8

#Post.Qtr
p9<-ggplot(facebook_excl, aes(x=Post.Qtr, 
                              y=Lifetime.Post.Consumers, 
                              color=Post.Qtr)) +
  geom_boxplot()
p9

######################################
#since we know our target and features
#let's restrict the data
######################################
facebook_final<-facebook_excl %>% 
  select(c(Lifetime.Post.Consumers,
           Category,
           Type,
           Page.total.likes,
           pct_change,
           Post.Month,
           Post.Qtr,
           Post.Weekday,
           Post.Hour,
           Post.Hour.Block,
           Paid
  ))
glimpse(facebook_final)
#BACK TO PPT

##############################
#split into training and test
##############################
set.seed(123456)
#80/20 split
training_test <- c(rep(1,length=trunc((8/10)*nrow(facebook_final))),
                   rep(2,length=(nrow(facebook_final) - trunc((8/10)*nrow(facebook_final)))))

facebook_final$training_test <- sample(training_test) #random permutation 
facebook_final$training_test <- factor(facebook_final$training_test, 
                                levels=c(1,2), labels=c("TRAIN","TEST"))
table(facebook_final$training_test)
387/484
97/484

#training
facebook.train <- subset(facebook_final, training_test == "TRAIN")
glimpse(facebook.train)
#test
facebook.test <- subset(facebook_final, training_test == "TEST")
glimpse(facebook.test)

######################
######################
######################
#MODELLING
######################
######################
######################
#MULTIPLE LINEAR REGRESSION
reg_model1 <- lm(Lifetime.Post.Consumers~
                Category+
                Type+
                Page.total.likes+
                Post.Month+
                Post.Weekday+
                Post.Hour+
                Paid, 
                data = facebook.train)
summary(reg_model1)
#PROBLEM: I HAVE TOTAL PAGE LIKES AND MONTH

reg_model2 <- lm(Lifetime.Post.Consumers~
                  Category+
                  Type+
                  #Page.total.likes+
                  Post.Month+
                  Post.Weekday+
                  Post.Hour+
                  Paid, 
                data = facebook.train)
summary(reg_model2)
#NEXT: TRY REMOVING MONTH AND USE LIKES

reg_model3 <- lm(Lifetime.Post.Consumers~
                   Category+
                   Type+
                   Page.total.likes+
                   #Post.Month+
                   Post.Weekday+
                   Post.Hour+
                   Paid, 
                 data = facebook.train)
summary(reg_model3)
#NEXT: 
#ANOTHER PROBLEM, HOUR IS SPARSE IN RECORD COUNT

reg_model4 <- lm(Lifetime.Post.Consumers~
                   Category+
                   Type+
                   Post.Month+
                   Post.Weekday+
                   Post.Hour.Block+
                   Paid, 
                 data = facebook.train)
summary(reg_model4)
#PRETTY CLOSE FIT TO FIRST MODEL
#AND->DON'T HAVE TO DEAL WITH HOUR COUNT ISSUE

#better output
stargazer(reg_model4,
          title="Results",
          align=TRUE,
          out="reg_table.html",
          digits=3)

#TRIED PCT CHANGE AND SOME OTHER COMBOS BUT
#LET'S MOVE ON
#BACK TO PPT TO LOOK AT ERROR

############################################
#LET'S LOOK AT THE ERROR MEASURES FIRST
############################################
################################
#we could create some functions
################################
#MSE
compute_mse <- function(predictions, actual) {
  mean((predictions - actual) ^ 2 )
}
#RMSE
compute_rmse <- function(predictions, actual) {
  sqrt(mean((predictions - actual) ^ 2 ))
}
#MAE
compute_mae <- function(predictions, actual) {
  mean(abs(predictions - actual))
}
#MAPE
compute_mape <- function(predictions, actual) {
  mean(abs((predictions - actual)/actual))
}

######################
#PREDICTED VALUES
######################
#LET'S GO WITH MODEL 4
#training prediction
facebook.train$reg_predicted<-predict(reg_model4,facebook.train)
facebook.train$reg_residual<-facebook.train$Lifetime.Post.Consumers-facebook.train$reg_predicted
#WHAT'S A RESIDUAL?
head(facebook.train,n=10)  
#MAKES SENSE NOW

#test prediction
facebook.test$reg_predicted<-predict(reg_model4,facebook.test)
facebook.test$reg_residual<-facebook.test$Lifetime.Post.Consumers-facebook.test$reg_predicted
head(facebook.test,n=10) 

#example: training error
compute_mse(facebook.train$reg_predicted, 
            facebook.train$Lifetime.Post.Consumers)
compute_rmse(facebook.train$reg_predicted, 
             facebook.train$Lifetime.Post.Consumers)
compute_mae(facebook.train$reg_predicted, 
            facebook.train$Lifetime.Post.Consumers)
compute_mape(facebook.train$reg_predicted, 
             facebook.train$Lifetime.Post.Consumers)
#let's look at the stats
summary(facebook.train$Lifetime.Post.Consumers)
summary(facebook.train$reg_predicted)

######################################
#or we could use the MLmetrics package
######################################
regression_training_MSE<-MLmetrics::MSE(y_true=facebook.train$Lifetime.Post.Consumers,
               y_pred=facebook.train$reg_predicted)

regression_training_RMSE<-MLmetrics::RMSE(y_true=facebook.train$Lifetime.Post.Consumers,
                y_pred=facebook.train$reg_predicted)

regression_training_MAE<-MLmetrics::MAE(y_true=facebook.train$Lifetime.Post.Consumers,
               y_pred=facebook.train$reg_predicted)

regression_training_MAPE<-MLmetrics::MAPE(y_true=facebook.train$Lifetime.Post.Consumers,
               y_pred=facebook.train$reg_predicted)

#error summary
regression_results_training<-data.frame(regression_training_MSE,
                                       regression_training_RMSE,
                                       regression_training_MAE,
                                       regression_training_MAPE)
regression_results_training

#test
regression_test_MSE<-MLmetrics::MSE(y_true=facebook.test$Lifetime.Post.Consumers,
                                        y_pred=facebook.test$reg_predicted)

regression_test_RMSE<-MLmetrics::RMSE(y_true=facebook.test$Lifetime.Post.Consumers,
                                          y_pred=facebook.test$reg_predicted)

regression_test_MAE<-MLmetrics::MAE(y_true=facebook.test$Lifetime.Post.Consumers,
                                        y_pred=facebook.test$reg_predicted)

regression_test_MAPE<-MLmetrics::MAPE(y_true=facebook.test$Lifetime.Post.Consumers,
                                          y_pred=facebook.test$reg_predicted)

#error summary
regression_results_test<-data.frame(regression_test_MSE,
                                  regression_test_RMSE,
                                  regression_test_MAE,
                                  regression_test_MAPE)
regression_results_test

################################
#PLOT
################################
#set together training and test sets for plotting
plotting.frame <- rbind(facebook.train,facebook.test)

#generate predictive modeling visual
group.symbols <- c(21,21)
group.colors <- c("black","black") 
group.fill <- c("yellow","yellow")  
xyplot(reg_predicted ~ Lifetime.Post.Consumers | training_test, 
       data = plotting.frame,cex = 1,
       pch = group.symbols, 
       col = group.colors, 
       fill = group.fill, 
       layout = c(2, 1), xlim = c(0,3000), ylim = c(0,3000), 
       aspect=1, type = c("p","g"),
       panel=function(x,y, ...)
       {panel.xyplot(x,y,...)
         panel.segments(0,0,3000,3000,col="blue",cex=1)
       },
       strip=function(...) strip.default(..., style=1),
       xlab = "Actual", 
       ylab = "Predicted") 

#########################
#DECISION TREE
#########################
#same inputs
regtree <- rpart(Lifetime.Post.Consumers~
                   Category+
                   Type+
                   Post.Month+
                   Post.Weekday+
                   Post.Hour.Block+
                   Paid, 
                 data = facebook.train)

plot(regtree, uniform = TRUE)
text(regtree, use.n = FALSE, all = TRUE, cex = 1)

#wow, that can't be read
#fancy plot (rattle)
fancyRpartPlot(regtree)

#variable importance
barplot(regtree$variable.importance)

######################################
#error
######################################
#training prediction
facebook.train$tree_predicted<-predict(regtree,facebook.train)
facebook.train$tree_residual<-facebook.train$Lifetime.Post.Consumers-facebook.train$tree_predicted
head(facebook.train)  
#test prediction
facebook.test$tree_predicted<-predict(regtree,facebook.test)
facebook.test$tree_residual<-facebook.test$Lifetime.Post.Consumers-facebook.test$tree_predicted
head(facebook.test)

#training error
tree_training_MSE<-MLmetrics::MSE(y_true=facebook.train$Lifetime.Post.Consumers,
                                        y_pred=facebook.train$tree_predicted)

tree_training_RMSE<-MLmetrics::RMSE(y_true=facebook.train$Lifetime.Post.Consumers,
                                          y_pred=facebook.train$tree_predicted)

tree_training_MAE<-MLmetrics::MAE(y_true=facebook.train$Lifetime.Post.Consumers,
                                        y_pred=facebook.train$tree_predicted)

tree_training_MAPE<-MLmetrics::MAPE(y_true=facebook.train$Lifetime.Post.Consumers,
                                          y_pred=facebook.train$tree_predicted)

#error summary
tree_results_training<-data.frame(tree_training_MSE,
                                           tree_training_RMSE,
                                           tree_training_MAE,
                                           tree_training_MAPE)
tree_results_training

#test error
tree_test_MSE<-MLmetrics::MSE(y_true=facebook.test$Lifetime.Post.Consumers,
                                    y_pred=facebook.test$tree_predicted)

tree_test_RMSE<-MLmetrics::RMSE(y_true=facebook.test$Lifetime.Post.Consumers,
                                      y_pred=facebook.test$tree_predicted)

tree_test_MAE<-MLmetrics::MAE(y_true=facebook.test$Lifetime.Post.Consumers,
                                    y_pred=facebook.test$tree_predicted)

tree_test_MAPE<-MLmetrics::MAPE(y_true=facebook.test$Lifetime.Post.Consumers,
                                      y_pred=facebook.test$tree_predicted)

#error summary
treee_results_test<-data.frame(tree_test_MSE,
                                       tree_test_RMSE,
                                       tree_test_MAE,
                                       tree_test_MAPE)
treee_results_test

########################
########################
########################
########################
#THE END - THANK YOU
########################
########################
########################
########################

#UPDATE: SVM
library(e1071)
tuned_results=tune(svm, 
                   log(Lifetime.Post.Consumers)~
                     Page.total.likes+
                     Category+
                     Type+
                     Post.Month+
                     Post.Weekday+
                     Post.Hour.Block+
                     Paid, 
                 data=facebook.train,
                 ranges=list(epsilon=seq(0,1,0.1), 
                             cost=1:100))
plot(tuned_results)

tuned_SVM <- tuned_results$best.model

######################################
#error
######################################
#training prediction
facebook.train$svm_predicted<-exp(predict(tuned_SVM,facebook.train))
facebook.train$svm_residual<-facebook.train$Lifetime.Post.Consumers-facebook.train$svm_predicted
head(facebook.train)  
#test prediction
facebook.test$svm_predicted<-exp(predict(tuned_SVM,facebook.test))
facebook.test$svm_residual<-facebook.test$Lifetime.Post.Consumers-facebook.test$svm_predicted
head(facebook.test)

#training error
svm_training_MSE<-MLmetrics::MSE(y_true=facebook.train$Lifetime.Post.Consumers,
                                  y_pred=facebook.train$svm_predicted)

svm_training_RMSE<-MLmetrics::RMSE(y_true=facebook.train$Lifetime.Post.Consumers,
                                    y_pred=facebook.train$svm_predicted)

svm_training_MAE<-MLmetrics::MAE(y_true=facebook.train$Lifetime.Post.Consumers,
                                  y_pred=facebook.train$svm_predicted)

svm_training_MAPE<-MLmetrics::MAPE(y_true=facebook.train$Lifetime.Post.Consumers,
                                    y_pred=facebook.train$svm_predicted)

#error summary
svm_results_training<-data.frame(svm_training_MSE,
                                  svm_training_RMSE,
                                  svm_training_MAE,
                                  svm_training_MAPE)
svm_results_training

#test error
svm_test_MSE<-MLmetrics::MSE(y_true=facebook.test$Lifetime.Post.Consumers,
                              y_pred=facebook.test$svm_predicted)

svm_test_RMSE<-MLmetrics::RMSE(y_true=facebook.test$Lifetime.Post.Consumers,
                                y_pred=facebook.test$svm_predicted)

svm_test_MAE<-MLmetrics::MAE(y_true=facebook.test$Lifetime.Post.Consumers,
                              y_pred=facebook.test$svm_predicted)

svm_test_MAPE<-MLmetrics::MAPE(y_true=facebook.test$Lifetime.Post.Consumers,
                                y_pred=facebook.test$svm_predicted)

#error summary
svm_results_test<-data.frame(svm_test_MSE,
                               svm_test_RMSE,
                               svm_test_MAE,
                               svm_test_MAPE)
svm_results_test