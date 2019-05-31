
library(dplyr)
library(caret)
require(smbinning)
library(vars)
#install.packages("smbinning", repo="http://cran.rstudio.com/")
library(caret)
library(plyr)
library(corrplot)


path = 'Z:/Model Risk/Adam/IFRS9_CECL_MV/data/chap2oneypd.csv'
data = read.csv2(path, sep=",")

# round max arrears 12m
data$max_arrears_12m <- round(as.numeric(as.character(data$max_arrears_12m)),4)

# round arrears_months
data$arrears_months <- round(as.numeric(as.character(data$arrears_months)), 4)

#convert to numeric
data$cc_util <- as.numeric(as.character(data$cc_util))
data$ltv <- as.numeric(as.character(data$ltv))
data$loan_balance <- as.numeric(as.character(data$loan_balance))
data$monthly_installment <- as.numeric(as.character(data$monthly_installment))

data <- mutate(data, 
               default_event = 
                 if_else(data$arrears_event == 1 | 
                           data$bankrupt_event == 1 |
                           data$term_expiry_event == 1,
                         1,
                         0)
)

# Record default event variables for more convenient use
#0-default, 1-non-default
data$default_flag <- if_else(data$default_event==1,0,1)

#iv_analysis <- smbinning.sumiv(df=data, y='default_flag')

#smbinning.sumiv.plot(iv_analysis)

bins = list()
bins$bureau_score = smbinning(df=data, y='default_flag', x='bureau_score')
bins$cc_util = smbinning(df=data, y='default_flag', x='cc_util')
bins$num_ccj = smbinning(df=data, y='default_flag', x='num_ccj')
bins$max_arrears_12 = smbinning(df=data, y='default_flag', x='max_arrears_12m')
bins$max_arrears_bal_6m = smbinning(df=data, y='default_flag', x='max_arrears_bal_6m')
bins$emp_length = smbinning(df=data, y='default_flag', x='emp_length')
bins$months_since_recent_cc_delinq = smbinning(df=data, y='default_flag', x='months_since_recent_cc_delinq')
bins$annual_income = smbinning(df=data, y='default_flag', x='annual_income')

# need to eliminate Infs
bins$cc_util = smbinning.custom(df=data, y='default_flag', x='cc_util',
                                cuts=c(.5283, .6502, .7642))

bins$months_since_recent_cc_delinq = smbinning.custom(
  df=data, y='default_flag', x='months_since_recent_cc_delinq', cuts=c(10,12))

for (name in names(bins)){
  data <- smbinning.gen(df = data, ivout = bins[[name]], paste('woe_', trimws(name), sep=''))
}

#Perform stratified sampling

woe_vars <- data %>% 
  dplyr::select(starts_with("woe"))

# convert bins to numeric
for (column_title in colnames(woe_vars)){
  
  factor_labels = list()
  ix_label = list()
  contains_missing_label = 0
  
  #print(column_title)
  
  for (level in levels(data[[column_title]])){
    factor_labels <- c(factor_labels, level)
    numeric_value = as.numeric(substr(level,1,2))
    #print(numeric_value)
    contains_missing_label = contains_missing_label + if_else(numeric_value == 0, 1, 0)
    ix_label <- c(ix_label, as.numeric(substr(level,1,2)))
  }
  
  if (contains_missing_label == 1){
    missing_woe_ix = which(bins[[substr(column_title,5,1000)]][["ivtable"]][['Cutpoint']] == 'Missing')
    
    replacement_vals = bins[[substr(column_title,5,1000)]][["ivtable"]][
      1:(nrow(bins[[substr(column_title,5,1000)]][["ivtable"]])),][['WoE']][missing_woe_ix]
    
    replacement_vals = c(replacement_vals, bins[[substr(column_title,5,1000)]][["ivtable"]][
      1:(nrow(bins[[substr(column_title,5,1000)]][["ivtable"]])-2),][['WoE']])
  }
  else {
    replacement_vals = bins[[substr(column_title,5,1000)]][["ivtable"]][
      1:(nrow(bins[[substr(column_title,5,1000)]][["ivtable"]])-2),][['WoE']]
  }
  #print(levels(train[[column_title]]))
  #print(replacement_vals)
  #replacement_vals = replace(replacement_vals, which(is.infinite(replacement_vals)), 999)
  #print(replacement_vals)
  data[[column_title]] = as.numeric(as.character(
    mapvalues(data[[column_title]], levels(data[[column_title]]), replacement_vals)))
}

set.seed(2122)
train.index <- createDataPartition(data$default_flag, p=.7, list = FALSE)
train <- data[train.index,]
test <- data[-train.index,] 

# create correlation plot

woe_vars <- train %>% 
  dplyr::select(starts_with("woe"))

woe_corr <- cor(as.matrix(woe_vars), method='spearman')
corrplot(woe_corr, method='number')

# throw out too-highly correlated values, say threshold >.5
woe_vars_clean = woe_vars %>% dplyr::select(-c(woe_max_arrears_bal_6m,woe_emp_length))
                                     
# stepwise regression
# support functions and databases
library(MASS)
attach(train)

# Stepwise model fitting
logit_full = glm(default_event ~ woe_bureau_score + woe_annual_income + woe_max_arrears_12 + woe_months_since_recent_cc_delinq
                 + woe_num_ccj + woe_cc_util, family=binomial(link='logit'), data=train)

logit_stepwise = stepAIC(logit_full, k=qchisq(0.05, 1, lower.tail=F), direction='both')
summary(logit_stepwise)
detach(train)


# From score to points
# define scaling function
scaled_score <- function(logit, odds, offset=500, pdo=20){
  b = pdo/log(2)
  a = offset - b * log(odds)
  round(a + b * log((1-logit)/logit))
}

#Score the entire dataset
#Use fitted model to score both test and train datasets
predict_logit_test <- predict(logit_stepwise, newdata = test, type='response')
predict_logit_train <- predict(logit_stepwise, newdata = train, type='response')

test$predict_logit <- predict_logit_test
train$predict_logit <- predict_logit_train

train$sample ='train'
test$sample = 'test'

data_whole <- rbind(train, test)

data_score <- data_whole %>%
  dplyr::select(id, default_event, default_flag, woe_bureau_score,
                woe_annual_income, woe_max_arrears_12, woe_months_since_recent_cc_delinq,
                woe_cc_util, sample, predict_logit)


# define scoring params inline with objectives
data_score$score <- scaled_score(data_score$predict_logit, 72, 660, 40)
hist(data_score$score)

#rm(list=c('bins','logit_full', 'test' , 'woe_vars','woe_vars_clean',
#          'logit_stepwise', 'data_whole', 'data', 'predict_logit_train', 'predict_logit_test',
#))

# PD Calibration
attach(data_score)

#fit logistic regression
pd_model <- glm(default_event ~ score, family=binomial(link='logit'), data=data_score)
summary(pd_model)


# graph pd_model
library(ggplot2)
score_range <- as.data.frame(seq(200,900))
colnames(score_range) <- c('score')

score_range <- cbind(score_range, predict(pd_model, newdata=score_range, type='response'))
colnames(score_range) <- c('score','PD')

ggplot(score_range, aes(x=score, y=PD)) + geom_point(aes(color=score)) + scale_colour_gradient(low='red', high='green')

# Calibrate data
data_score$pd <- predict(pd_model, newdata=data_score, type='response')
head(data_score)

# GLM validation
# Gini index
library(optiRum)

giniChart(pred=data_score$pd, act=data_score$default_event)

library(pROC)
train$roc <- roc(train$default_event, train$predict_logit, direction="<")

roc_plot_df <- as.data.frame(cbind(train$roc$specificities,train$roc$sensitivities))
colnames(roc_plot_df) <- c('specificities', 'sensitivities')

ggplot(data=roc_plot_df, aes(x=specificities, y=sensitivities)) + geom_line(color='blue', lwd=1) +
  xlim(1,0) + ggtitle(paste("ROC\nAUC: ", round(train$roc$auc,5)*100, '%',sep=''))


# Create a validation database
# Create score bands
score_cust <- smbinning.custom(data_score, y='default_flag',
                               x='score', cuts=c(517,576,605,632,667,716,746,773))

# Group by bands
data_score <- smbinning.gen(data_score, score_cust, chrname='score_band')

# Compare actual against fitted PDs
# Compute mean analysis
data_pd <- data_score %>% 
  dplyr::select(score, score_band, pd, default_event) %>%
  dplyr::group_by(score_band) %>%
  dplyr::summarize(mean_dr = round(mean(default_event),4),
                   mean_pd = round(mean(pd), 4))
  
# Compute RMSE
rmse <- sqrt(mean((data_pd$mean_dr - data_pd$mean_pd)^2))
round(rmse,5)

ggplot(data=data_pd, aes(x=score_band)) + 
  geom_line(y=data_pd$mean_pd, group=1, color='blue', lwd=1,alpha=.25, linetype='dotted') +
  geom_boxplot(aes(y=data_pd$mean_dr, color='blue')) + 
  
  
  geom_line(y=data_pd$mean_pd, group=1, color='red', lwd=1,alpha=.25, linetype='dashed') +
  geom_boxplot(aes(y=data_pd$mean_pd, color='red')) + 
  
  
  ylab('Default Rate') +
  xlab('Score Band') +
  ggtitle("Default Rates: Actual v. Fitted") +
  scale_color_discrete(name = "", labels = c("Actual Default %", "Fitted Default %")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# prepare the cross-validation dataset
data_subset <- data_whole %>%
  dplyr::select(id, default_event, default_flag, woe_bureau_score,
                woe_annual_income, woe_max_arrears_12, woe_months_since_recent_cc_delinq,
                woe_cc_util, sample)

# perform cross validation loop
j <- 1
m <- 50   # number of folds
n = floor(nrow(data_subset)/m)  #size of each fold
auc_vector <- rep(NA, m)
gini_vector <- rep(NA, m)
ks_vector <- rep(NA, m)

# run the loop
for (j in 1:m){
  s1 = ((j-1) * n + 1) # start of the subset (fold)
  s2 = (j * n) # end of the subset
  
  data_cv_subset = s1:s2   # range of the fold
  
  train_set <- data_subset[-data_cv_subset, ]
  test_set <- data_subset[data_cv_subset, ]
  
  # model fitting
  model <- glm(default_event ~ woe_bureau_score + woe_annual_income + woe_max_arrears_12 +
                 woe_months_since_recent_cc_delinq + woe_cc_util, family=binomial(link='logit'),
               data = train_set)
  
  #Predict results
  predict_cv <- predict(model, newdata = test_set, type='response')
  
  pred_obj <- ROCR::prediction(predict_cv, test_set[,2])
  perf_obj <- ROCR::performance(pred_obj, 'tpr', 'fpr')
  
  #Calc performance metrics for each fold/run
  test_auc <- ROCR::performance(pred_obj, 'auc')
  auc_vector[j] <- test_auc@y.values[[1]]
  gini_vector[j] <- optiRum::giniCoef(predict_cv, test_set[,2])
}

par(mfrow=c(1,2))
hist(auc_vector)
hist(gini_vector)