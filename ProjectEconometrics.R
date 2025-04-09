rm(list=ls());graphics.off();cat("\014")



setwd("C:/Users/giaco/Desktop/Soil")

validation <- read.csv("validation_timeseries/validation_timeseries.csv")
test <- read.csv("test_timeseries/test_timeseries.csv")
soil <- read.csv("soil_data.csv")

library(MASS)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(ggpubr)
library(cowplot)
library(leaflet)
library(ordinalNet) # for ordinalNet
library(caret)
library(geosphere)
library(gamlss)
library(betareg)
library(statmod)
library(glmnet)

dataset <- rbind(validation, test)
rm(test)
rm(validation)


#### EXPLORATORY ANALYSIS ####

df <- dataset %>% select(-c(T2M_RANGE, WS10M_RANGE, WS50M_RANGE))

corr <- round(cor(df[,3:18]),3)
ggcorrplot(corr, title = "Correlation Matrix",
           type = "upper", lab = T, lab_size = 3,colors = c("indianred1","white","indianred1")) + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 90))

# eliminare T2M e T2MDEW
df <- df %>% select(-c(T2M, T2MDEW))

corr <- round(cor(df[,3:16]),3)
ggcorrplot(corr, title = "Correlation Matrix",
           type = "upper", lab = T, colors = c("indianred1","white","indianred1")) + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 90))

df <- df %>%
  mutate(across(
    .cols = -c(1:2, ncol(df)),
    .fns = list(
      lag_1 = ~ lag(., 1),
      lag_2 = ~ lag(., 2),
      lag_3 = ~ lag(., 3),
      lag_4 = ~ lag(., 4),
      lag_5 = ~ lag(., 5),
      lag_6 = ~ lag(., 6)
    ),
    .names = "{.col}_{.fn}"
  ))

df <- df %>%
  mutate(across(
    .cols = names(df)[!grepl("_lag", names(df)) & !(names(df) %in% c('fips','date','score'))],
    .fns = list(
      week_2 = ~ rowMeans(cbind(lag(., 8), lag(., 9), lag(., 10), lag(., 11), lag(., 12), lag(., 13), lag(., 14))),
      week_3 = ~ rowMeans(cbind(lag(., 15), lag(., 16), lag(., 17), lag(., 18), lag(., 19), lag(., 20), lag(., 21))),
      week_4 = ~ rowMeans(cbind(lag(., 22), lag(., 23), lag(., 24), lag(., 25), lag(., 26), lag(., 27), lag(., 28))),
      week_5 = ~ rowMeans(cbind(lag(., 29), lag(., 30), lag(., 31), lag(., 32), lag(., 33), lag(., 34), lag(., 35))),
      week_6 = ~ rowMeans(cbind(lag(., 36), lag(., 37), lag(., 38), lag(., 39), lag(., 40), lag(., 41), lag(., 42))),
      week_7 = ~ rowMeans(cbind(lag(., 43), lag(., 44), lag(., 45), lag(., 46), lag(., 47), lag(., 48), lag(., 49))),
      week_8 = ~ rowMeans(cbind(lag(., 50), lag(., 51), lag(., 52), lag(., 53), lag(., 54), lag(., 55), lag(., 56)))      
      ),
    .names = "{.col}_{.fn}"
  ))



df <- df %>% na.omit() %>% mutate(date=as.Date(date)) %>% filter(date>="2017-02-28")

summary(df[, 1:18])

df <- df %>% mutate(class_score = cut(score, 
                                      breaks = c(-0.1, 0, 1, 2, 3, 4, 5, 5.1),
                                      labels = c("None", "D0", "D1", "D2", "D3", "D4", "D4"),
                                      right = T))
df$class_score <- as.ordered(df$class_score)

table(df$class_score)
100*table(df$class_score)/nrow(df)

str(df$class_score)
plot(df$class_score, col = 2:7)


# grafico tutte le contee
joined <- full_join(df,soil[,c(1,2,3)])
joined <- na.omit(joined)
aggregated_data <- joined %>%
  group_by(fips, lon, lat) %>%
  summarise(count = mean(score))

pal <- colorNumeric(
  palette = "Reds",  
  domain = aggregated_data$count 
)

leaflet(aggregated_data) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = 4,
    color = ~pal(count),
    fillOpacity = 0.7, stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~count,
    title = "Average Score",
    opacity = 0.7
  )

var_num <- colnames(df[,3:15])

k <- 1
boxlist <- list()
for(i in var_num) {
  g <- ggplot(df, aes_string(x=i, fill = "class_score"))+
    geom_boxplot() + theme_test() + coord_flip() +
    labs(x = "", y = i) + guides(fill=guide_legend(title=NULL)) +
    theme(legend.text = element_text(size=12))
  boxlist[[k]] <- g
  k <- k+1
}
griglia <- ggarrange(plotlist = boxlist, common.legend = T, legend = "right")
titolo <- ggdraw() + draw_label("Boxplots conditioned on class",
                                fontface = "bold", size = 22)
plot_grid(titolo, griglia, ncol = 1, rel_heights = c(0.1, 0.9))

plotHist <- function(dati, var_num, var_fact = NULL, title, legenda = NULL,
                     palette = "Darjeeling1") {
  k <- 1
  histlist <- list()
  if (is.null(var_fact)) {
    for(i in var_num) {
      g <- ggplot(dati, mapping = aes_string(x = i)) +
        geom_histogram(aes(y = after_stat(density)), fill = "darkorchid3",
                       color = "black", bins = 20, alpha = 0.5) +
        geom_density(alpha = 0.5, fill = "mediumorchid3", kernel = "gaussian") +
        labs(y = "")
      histlist[[k]] <- g
      k <- k+1}
    griglia <- ggarrange(plotlist = histlist)
  } else {
    for(i in var_num) {
      g <- ggplot(dati, mapping = aes_string(x = i)) +
        geom_histogram(aes(y = after_stat(density), fill = var_fact),
                       color = "black", bins = 20, alpha = 0.4, position = "identity") +
        geom_density(aes(fill = var_fact), kernel = "gaussian",
                     linewidth = 0.6, alpha = 0.7) +
        labs(y = "") + guides(fill=guide_legend(title=legenda))
      histlist[[k]] <- g
      k <- k+1}
    griglia <- ggarrange(plotlist = histlist, common.legend = T, legend = "bottom")
  }
  titolo <- ggdraw() + draw_label(title, fontface = "bold", size = 22)
  plot_grid(titolo, griglia, ncol = 1, rel_heights = c(0.1, 0.9))
}

var_num <- colnames(df[,3:15])
plotHist(df, var_num, title = "Histograms")

plotHist(df, var_num, var_fact = df$class_score, legenda = "",
         title = "Histograms conditioned on class")
hist(df$PRECTOT[df$PRECTOT!=0])
hist(log(df$PRECTOT+0.1))


for (i in 3:15){
  k=17+(i-3)*6
  l=95+(i-3)*7
  corr <- round(cor(df[,c(k:(k+5),(l:(l+6)))]),3)
  p <- ggcorrplot(corr, title = "Correlation Matrix",
                  type = "upper", lab = T, colors = c("indianred1","white","indianred1")) + 
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 90))
  print(p)
}

# Remove lags for PS:
data <- df[, !(grepl("PS", names(df)) & names(df) != "PS")]




#### COUNTIES SELECTION ####

set.seed(42)

km <- kmeans(aggregated_data[,-c(1,4)], centers = 3)
table(km$cluster)
aggregated_cluster <- aggregated_data %>% cbind(cluster = km$cluster)

aggregated_data_cluster1 <- aggregated_cluster %>% filter(cluster == 1)
aggregated_data_cluster2 <- aggregated_cluster %>% filter(cluster == 2)
aggregated_data_cluster3 <- aggregated_cluster %>% filter(cluster == 3)


dist_matrix <- distm(aggregated_data_cluster2[,2:3],fun=distHaversine)/1000
max(dist_matrix)
mean(dist_matrix)
distance_min <- 200

selected <- rep(TRUE, nrow(dist_matrix))

for (i in 1:(nrow(dist_matrix) - 1)) {
  if (selected[i]) {
    close_points <- which(dist_matrix[i, ] < distance_min)
    close_points <- close_points[close_points > i]
    selected[close_points] <- FALSE
  }
}

aggregated_data_cluster2_new <- aggregated_data_cluster2[selected, ]

leaflet(aggregated_data_cluster2_new) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = 4,
    color = ~pal(count),
    fillOpacity = 0.7, stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~count,
    title = "Average Score",
    opacity = 0.7
  )

dist_matrix <- distm(aggregated_data_cluster1[,2:3],fun=distHaversine)/1000
distance_min <- 240

selected <- rep(TRUE, nrow(dist_matrix))

for (i in 1:(nrow(dist_matrix) - 1)) {
  if (selected[i]) {
    close_points <- which(dist_matrix[i, ] < distance_min)
    close_points <- close_points[close_points > i]
    selected[close_points] <- FALSE
  }
}

aggregated_data_cluster1_new <- aggregated_data_cluster1[selected, ]

leaflet(aggregated_data_cluster1_new) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = 4,
    color = ~pal(count),
    fillOpacity = 0.7, stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~count,
    title = "Average Score",
    opacity = 0.7
  )

dist_matrix <- distm(aggregated_data_cluster3[,2:3],fun=distHaversine)/1000
distance_min <- 220

selected <- rep(TRUE, nrow(dist_matrix))

for (i in 1:(nrow(dist_matrix) - 1)) {
  if (selected[i]) {
    close_points <- which(dist_matrix[i, ] < distance_min)
    close_points <- close_points[close_points > i]
    selected[close_points] <- FALSE
  }
}

aggregated_data_cluster3_new <- aggregated_data_cluster3[selected, ]

leaflet(aggregated_data_cluster3_new) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = 4,
    color = ~pal(count),
    fillOpacity = 0.7, stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~count,
    title = "Average Score",
    opacity = 0.7
  )

aggregated_data_cluster_new <- rbind(aggregated_data_cluster1_new,
                                     aggregated_data_cluster2_new,
                                     aggregated_data_cluster3_new)

leaflet(aggregated_data_cluster_new) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = 4,
    color = ~pal(count),
    fillOpacity = 0.7, stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~count,
    title = "Average Score",
    opacity = 0.7
  )

df_new <- data %>% filter(fips %in% aggregated_data_cluster_new$fips)
100*table(df_new$class_score)/nrow(df_new)
100*table(df$class_score)/nrow(df)
plot(df_new$class_score, col = 2:7)



#### NEW VARIABLES FOR COUNTIES AND TIME ####
df_new$month <- as.factor(month(df_new$date))

# corr2 <- round(cor(soil[,-c(1:3)]),3)
# ggcorrplot(corr2, title = "Correlation Matrix",
#            type = "upper", lab = T, colors = c("indianred1","white","indianred1")) + 
#   theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
#         axis.text.x = element_text(angle = 90))
# 
# soil.join <- soil[,-(26:32)]
# soil.join <- soil.join[,-c(2,3,5:17)]
# 
# soil.join <- soil.join
# df_new.join <- left_join(x = df_new,y = soil.join,by = 'fips')



#### TRAINING, VALIDATION & TEST ####

training <- df_new %>% filter(date < "2020-01-01")
test <- df_new %>% filter(date >= "2020-01-01")

nrow(test)/nrow(df_new)*100



####  MODELS  ####
set.seed(123)


#### 1. ORDERDED MULTINOMIAL REGRESSION ####

training.po <- training[,-2]
test.po <- test[,-2]

wg <- as.numeric(1/(table(df$class_score)/sum(table(df$class_score))))
wg_vector <- wg[as.numeric(training.po$class_score)]
wg_vector <- round(wg_vector,0)

propodds.j <- polr(class_score~.-score-fips,data=training.po,weights = wg_vector)
summary(propodds.j)

pred1_train <- predict(propodds.j)
confusionMatrix(data = pred1_train,reference= training.po$class_score)

pred1_test <- predict(propodds.j,newdata = test.po)
confusionMatrix(data = pred1_test,reference= test.po$class_score)



#### 2. ORDERDED MULTINOMIAL REGRESSION WITH LASSO ####
trn.o <- model.matrix(propodds.j)[,-1]
trn.y <- as.ordered(training$class_score)

ord.tune <- ordinalNetTune(x = trn.o, y = trn.y, nLambda=5, lambdaMinRatio = 0.001, nFolds = 5)
ord.tune

par(mfrow=c(2,2))
plot(log(ord.tune$lambdaVals),rowMeans(ord.tune$loglik),type = 'b',xlab = 'logLambda',ylab = 'loglik_avg')
plot(log(ord.tune$lambdaVals),rowMeans(ord.tune$misclass),type = 'b',xlab = 'logLambda',ylab = 'misclass_avg')
plot(log(ord.tune$lambdaVals),rowMeans(ord.tune$brier),type = 'b',xlab = 'logLambda',ylab = 'brier_avg')
plot(log(ord.tune$lambdaVals),rowMeans(ord.tune$devPct),type = 'b',xlab = 'logLambda',ylab = 'devPct_avg')
par(mfrow=c(1,1))

best.lambda <- ord.tune$lambdaVals[4]

ord.reg_best <- ordinalNet(x = trn.o, y = trn.y,
                           alpha = 1,family = 'cumulative',link = 'logit',lambdaVals = best.lambda)

pred.t <- predict(ord.reg_best)
pred_labels.t <- as.factor(apply(pred.t, 1, which.max))
levels(pred_labels.t) <- levels(trn.y)
confusionMatrix(data = pred_labels.t,reference = trn.y)

test.o <- model.matrix(class_score ~ . - score - fips -date, data = test)[,-1]
test.y <- as.ordered(test$class_score)

pred.test <- predict(ord.reg_best, newx = test.o)
pred_labels.test <- as.ordered(apply(pred.test, 1, which.max))
levels(pred_labels.test) <- levels(test.y)
confusionMatrix(data = pred_labels.test, reference = test.y)


# Undersampling and oversampling
mm <- round(median(table(trn.y)),0)

t.n <- training[trn.y=='None',]
t.0 <- training[trn.y=='D0',]
t.1 <- training[trn.y=='D1',]
t.2 <- training[trn.y=='D2',]
t.3 <- training[trn.y=='D3',]
t.4 <- training[trn.y=='D4',]

t.n_s <- t.n[sample(1:nrow(t.n),size = mm,replace = F),]
t.0_s <- t.0[sample(1:nrow(t.0),size = mm,replace = F),]
t.1_s <- t.1[sample(1:nrow(t.1),size = mm,replace = F),]
t.2_s <- t.2[sample(1:nrow(t.2),size = mm,replace = T),]
t.3_s <- t.3[sample(1:nrow(t.3),size = mm,replace = T),]
t.4_s <- t.4[sample(1:nrow(t.4),size = mm,replace = T),]

training_s <- as.data.frame(rbind(t.n_s,t.0_s,t.1_s,t.2_s,t.3_s,t.4_s))

trn.o_s <- model.matrix(class_score ~ . - score - fips -date, data = training_s)[,-1]
trn.y_s <- as.ordered(training_s$class_score)

ord.tune_s <- ordinalNetTune(x = trn.o_s, y = trn.y_s, nLambda=5, lambdaMinRatio = 0.001, nFolds = 5)
ord.tune_s$fit

par(mfrow=c(2,2))
plot(log(ord.tune_s$lambdaVals),rowMeans(ord.tune_s$loglik),type = 'b',xlab = 'logLambda',ylab = 'loglik_avg')
abline(v=log(ord.tune_s$lambdaVals[4]),col='red',lty=2)
plot(log(ord.tune_s$lambdaVals),rowMeans(ord.tune_s$misclass),type = 'b',xlab = 'logLambda',ylab = 'misclass_avg')
abline(v=log(ord.tune_s$lambdaVals[4]),col='red',lty=2)
plot(log(ord.tune_s$lambdaVals),rowMeans(ord.tune_s$brier),type = 'b',xlab = 'logLambda',ylab = 'brier_avg')
abline(v=log(ord.tune_s$lambdaVals[4]),col='red',lty=2)
plot(log(ord.tune_s$lambdaVals),rowMeans(ord.tune_s$devPct),type = 'b',xlab = 'logLambda',ylab = 'devPct_avg')
abline(v=log(ord.tune_s$lambdaVals[4]),col='red',lty=2)
par(mfrow=c(1,1))

best.lambda <- ord.tune_s$lambdaVals[4]

ord.reg_best_s <- ordinalNet(x = trn.o_s, y = trn.y_s,
                           alpha = 1,family = 'cumulative',link = 'logit',lambdaVals = best.lambda)

pred.t_s <- predict(ord.reg_best_s)
pred_labels.t_s <- as.factor(apply(pred.t_s, 1, which.max))
levels(pred_labels.t_s) <- levels(trn.y_s)
confusionMatrix(data = pred_labels.t_s,reference = trn.y_s)


pred.test_s <- predict(ord.reg_best_s, newx = test.o)
pred_labels.test_s <- as.ordered(apply(pred.test_s, 1, which.max))
levels(pred_labels.test_s) <- levels(test.y)
confusionMatrix(data = pred_labels.test_s, reference = test.y)

zero_vars <- colnames(ord.reg_best_s$coefs)[ord.reg_best_s$coefs==0]
zero_vars



#### 3. LOGISTIC REGRESSION ####

df_two <- df_new
df_two$class_score <- as.factor(ifelse(df_two$class_score=="None",0,1))

training_reglog <- df_two %>% filter(date < "2020-01-01" )
test_reglog <- df_two %>% filter(date >= "2020-01-01" )


calculate_metric <- function(threshold, true_labels, predicted_probs) {
  predicted_classes <- as.factor(ifelse(predicted_probs > threshold, 1, 0))
  confusion <- confusionMatrix(predicted_classes, true_labels, positive = "1")
  bc_score <- confusion$byClass["Balanced Accuracy"]
  return(bc_score)
}

thresholds <- seq(0.1, 0.9, by = 0.05)

folds <- createFolds(training_reglog$class_score, k = 5)

results <- sapply(thresholds, function(threshold) {
  fold_metrics <- sapply(folds, function(fold) {
    train_data <- training_reglog[-fold, ]
    valid_data <- training_reglog[fold, ]

    reglog <- glm(class_score ~ . -score -fips, data = train_data[, -2], family = binomial)
    
    probpred_valid <- predict(reglog, newdata = valid_data[, -2], type = "response")

    calculate_metric(threshold, valid_data$class_score, probpred_valid)
  })
  mean(fold_metrics)
})

reglog <- glm(class_score ~ . -score -fips, data = training_reglog[,-2], family = binomial)

optimal_threshold <- thresholds[which.max(results)]
optimal_threshold

plot(thresholds, results, type = "b", pch = 19, col = "blue", 
     xlab = "Threshold", ylab = "Mean BA-Score", main = "Optimal Threshold Selection")



probpred_test <- predict(reglog, newdata = test_reglog[, -2], type = "response")
class_pred_test <- as.factor(ifelse(probpred_test > optimal_threshold, 1, 0))

confusionMatrix(data = class_pred_test, reference = test_reglog$class_score, positive = '1')



#### 4. LOGISTIC REGRESSION WITH LASSO ####
df_three <- df_new
df_three$class_score <- as.factor(ifelse(df_three$class_score=="None",0,1))

training_reglog.l <- df_three %>% filter(date < "2019-01-01" )
validation_reglog.l <- df_three %>% filter(date >= "2019-01-01" & date < "2020-01-01")
test_reglog.l <- df_three %>% filter(date >= "2020-01-01" )

x_train <- model.matrix(class_score ~ . - score - fips, data = training_reglog.l[,-2])[,-1]
y_train <- as.numeric(training_reglog.l$class_score) - 1

x_val <- model.matrix(class_score ~ . - score - fips, data = validation_reglog.l[,-2])[,-1]
y_val <- as.numeric(validation_reglog.l$class_score) - 1

x_test <- model.matrix(class_score ~ . - score - fips, data = test_reglog.l[,-2])[,-1]
y_test <- as.numeric(test_reglog.l$class_score) - 1

cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1,type.measure = 'class')

plot(cv_lasso)

best_lambda <- cv_lasso$lambda.1se
cat("Best Lambda:", best_lambda, "\n")


lasso_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)

probpred_valid <- predict(lasso_model, newx = x_val, type = "response")
thresholds <- seq(0.1, 0.9, by = 0.05)

results.l <- sapply(thresholds, function(threshold) {
  class_pred_valid <- as.factor(ifelse(probpred_valid > threshold, 1, 0))
  
  cm <- confusionMatrix(class_pred_valid, as.factor(y_val), positive = '1')
  sensitivity <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  
  balanced_acc <- as.numeric((sensitivity + specificity)/2)
  return(balanced_acc)
})

plot(thresholds, results.l, type = "b", pch = 19, col = "blue", 
     xlab = "Threshold", ylab = "BA-Score", main = "Optimal Threshold Selection")

optimal_threshold.l <- thresholds[which.max(results.l)]
cat("Optimal Threshold:", optimal_threshold.l, "\n")
abline(v=optimal_threshold.l,lty=2,col='red')


x_train2 <- model.matrix(class_score ~ . - score - fips, data = training_reglog[,-2])[,-1]
y_train2 <- as.numeric(training_reglog$class_score) - 1

x_test2 <- model.matrix(class_score ~ . - score - fips, data = test_reglog[,-2])[,-1]
y_test2 <- as.numeric(test_reglog$class_score) - 1

lasso_model.f <- glmnet(x_train2, y_train2, family = "binomial", alpha = 1, lambda = best_lambda)

probpred_test <- predict(lasso_model.f, newx = x_test2, type = "response")
class_pred_test <- as.factor(ifelse(probpred_test > optimal_threshold.l, 1, 0))
confusionMatrix(data = class_pred_test, reference = as.factor(y_test), positive = '1')



## Grouping lasso
variable_names <- colnames(x_train)
groups <- numeric(length(variable_names))

main_vars <- c("PRECTOT", "QV2M", "T2MWET", "T2M_MAX", "T2M_MIN", 
               "TS", "WS10M", "WS10M_MAX", "WS10M_MIN", "WS50M", "WS50M_MAX", "WS50M_MIN")

for (i in seq_along(main_vars)) {
  pattern <- paste0("^", main_vars[i], "(_lag_|_week_|$)")
  groups[grep(pattern, variable_names)] <- i
}

groups[grep("^PS$", variable_names)] <- length(main_vars) + 1

groups[grep("^month", variable_names)] <- length(main_vars) + 2

cat("Gruppi assegnati:\n")
print(data.frame(Variable = variable_names, Group = groups))

library(grpreg)
cv_lasso.g <- cv.grpreg(x_train2, y_train2, group = groups, family = "binomial", penalty = "grLasso", seed=2525)
plot(cv_lasso.g,type = 'pred')


coefs_matrix <- t(as.matrix(coef(cv_lasso.g, lambda = cv_lasso.g$lambda)))

coefs_df <- as.data.frame(coefs_matrix)
coefs_df$Lambda <- cv_lasso.g$lambda

cf_df <- apply(X = coefs_df[,-c(1,ncol(coefs_df))],MARGIN = 2,function(x) which.min(x==0))

group_names <- setNames(groups, colnames(coefs_df)[-c(1, ncol(coefs_df))])

names(cf_df) <- group_names[names(cf_df)]

cf_grouped <- tapply(cf_df, names(cf_df), min)
cf_grouped <- cf_grouped[order(-cf_grouped) ]

cf_grouped_df <- data.frame(Group = names(cf_grouped), Lambda_Index = cv_lasso.g$lambda[cf_grouped])
cf_grouped_df$Group <- as.numeric(cf_grouped_df$Group)

# Dati con i numeri e le variabili come nella tua lista
df_names <- data.frame(
  Group = c(1, 13, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14),
  Variable = c("PRECTOT", "PS", "QV2M", "T2MWET", "T2M_MAX", "T2M_MIN", "TS", 
                "WS10M", "WS10M_MAX", "WS10M_MIN", "WS50M", "WS50M_MAX", 
                "WS50M_MIN", "month")
)

df_join_nm <- left_join(cf_grouped_df,df_names,by = 'Group')
df_join_nm



#### 5. BETA REGRESSION ####
training.b <- training
test.b <- test

training.b$score <- (training.b$score - min(training.b$score)) / (max(training.b$score) - min(training.b$score))
epsilon <- 1e-2
training.b$score <- (training.b$score * (1 - 2 * epsilon)) + epsilon
hist(training.b$score)

test.b$score <- (test.b$score - min(test.b$score)) / (max(test.b$score) - min(test.b$score))
test.b$score <- (test.b$score * (1 - 2 * epsilon)) + epsilon

b.reg <- betareg(score~.-class_score-fips,data=training.b[,-2])
summary(b.reg)

train_breg <- predict(b.reg)
test_breg <- predict(b.reg,newdata = test.b[,-2])

sd(training.b$score)
(RMSE_betainfl.train <- sqrt(mean((training.b$score - train_breg)^2)))
(MAE_betainfl.train <- mean(abs(training.b$score - train_breg)))

sd(test.b$score)
(RMSE_betainfl.test <- sqrt(mean((test.b$score - test_breg)^2)))
(MAE_betainfl.test <- mean(abs(test.b$score - test_breg)))

ggplot(data = test.b, aes(x = score, y = test_breg)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Previsioni vs Valori Osservati", x = "Osservato", y = "Previsto") +
  theme_minimal()



#### 6. BETA INFLATED REGRESSION ####
training.bi <- training
test.bi <- test

training.bi$score <- (training.bi$score - min(training.bi$score)) / (max(training.bi$score) - min(training.bi$score))
test.bi$score <- (test.bi$score - min(test.bi$score)) / (max(test.bi$score) - min(test.bi$score))

betainfl <- gamlss(score~.-class_score-fips,data=training.bi[,-2],family=BEINF)

plot(betainfl)

# Probabilità di 0
predict(betainfl, what = "nu", type = "response")
# Probabilità di 1
predict(betainfl, what = "tau", type = "response")

# Previsioni per la media
y_pred <- predict(betainfl, newdata = test.b[,-2], what = "mu", type = "response")

sd(test.b$score)

RMSE_betainfl <- sqrt(mean((test.b$score - y_pred)^2))
RMSE_betainfl
MAE_betainfl <- mean(abs(test.b$score - y_pred))
MAE_betainfl

SST <- sum((test.b$score - mean(test.b$score))^2)  # Totale della varianza
SSE <- sum((test.b$score - y_pred)^2)                  # Errore residuo
R2 <- 1 - (SSE / SST)
R2

ggplot(data = test.b, aes(x = score, y = y_pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Previsioni vs Valori Osservati", x = "Osservato", y = "Previsto") +
  theme_minimal()

sd(test.b$score)




#### 7. BINARY MODEL ####

training_boost <- df_new %>% filter(date < "2020-01-01")
test_boost <- df_new %>% filter(date >= "2020-01-01" )

training_boost$score <- (training_boost$score - min(training_boost$score)) / (max(training_boost$score) - min(training_boost$score))
test_boost$score <- (test_boost$score - min(test_boost$score)) / (max(test_boost$score) - min(test_boost$score))

training_boost[training_boost$score==1,]$score <- 1-epsilon

# Creazione della variabile indicatrice
training_boost$y_bin <- ifelse(training_boost$score == 0, 1,0)  # 1 = esattamente 0, 0 = maggiore di 0
test_boost$y_bin <- ifelse(test_boost$score == 0, 1,0)

# Regressione logistica per la probabilità di 0
modello_logit <- glm(y_bin ~ .,
                     family = binomial, data = training_boost[,-c(1,2,16,173)])
summary(modello_logit)
prob_zero <- predict(modello_logit, newdata = test_boost, type = "response")

# Subset per i valori y > 0
dati_continuo <- subset(training_boost, score > 0)

# Modello beta per valori in (0,1)
modello_beta <- betareg(score ~ ., data = dati_continuo[,-c(1,2,173,175)])
summary(modello_beta)

# Combina le previsioni
y_pred <- rep(NA, nrow(test_boost))
for (i in 1:nrow(test_boost)) {
  if (prob_zero[i] > 0.5) {y_pred[i] <- 0}
  else {y_pred[i] <- predict(modello_beta, newdata = test_boost[i,],
                             type = "response")}
}

ggplot(data = test_boost, aes(x = score, y = y_pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Previsioni vs Valori Osservati", x = "Osservato", y = "Previsto") +
  theme_minimal()

RMSE_boost <- sqrt(mean((test_boost$score - y_pred)^2))
MAE_boost <- mean(abs(test_boost$score - y_pred))
RMSE_boost
MAE_boost



#### 8. BETA REGRESSION WITH LASSO ####

# Funzione per log-verosimiglianza penalizzata
log_likelihood_penalized <- function(beta, X, y, lambda) {
  X <- as.matrix(X)
  beta <- as.numeric(beta)
  mu <- exp(X %*% beta) / (1 + exp(X %*% beta))  # Media
  phi <- 10  # parametro di precisione
  
  loglik <- sum(dbeta(y, shape1 = mu * phi, shape2 = (1 - mu) * phi, log = TRUE))
  penalty <- lambda * sum(abs(beta))
  
  return(-loglik + penalty)
}


optimize_lasso <- function(X, y, lambda, beta_init = NULL) {
  if (is.null(beta_init)) {
    beta_init <- rep(0, ncol(X))
  }
  
  fit <- optim(
    par = beta_init,
    fn = log_likelihood_penalized,
    X = X,
    y = y,
    lambda = lambda,
    method = "L-BFGS-B",  # Algoritmo per ottimizzazione vincolata
    control = list(maxit = 100)
  )
  
  return(fit)
}

lambda <- 1  # also other values
# beta_init <- rep(0, ncol(model.matrix(betainfl)))
beta_init <- coef(betainfl)

fit <- optimize_lasso(X = model.matrix(betainfl),
                      y = training.b[,]$score,
                      lambda = lambda, beta_init = beta_init)

months <- test.b[, 174]
months_matrix <- matrix(NA,ncol=12,nrow=6188)
colnames(months)
for (i in 1:12) {
  months_matrix[,i] <- ifelse(months == i, 1, 0)
}
X_test <- cbind("(Intercept)" = 1, as.matrix(test.b[,-c(1,2,16,173,174)]), months_matrix[,-1]) # INTERCETTA
X_test <- matrix(as.numeric(X_test), nrow = nrow(X_test))

beta_hat <- fit$par  
mu_pred <- (exp(X_test %*% beta_hat)) / (1 + exp(X_test %*% beta_hat))

RMSE_blasso <- sqrt(mean((test.b$score - mu_pred)^2))
MAE_blasso <- mean(abs(test.b$score - mu_pred))
RMSE_blasso
MAE_blasso


# Risultati
cat("Coefficienti stimati:\n")
print(fit$par)

cat("\nValore della funzione obiettivo:\n")
print(fit$value)


# Estimate phi
log_likelihood_penalized <- function(params, X, y, lambda) {
  X <- as.matrix(X)
  beta <- as.numeric(params[-1])  # Extract beta coefficients
  phi <- exp(params[1])           # Transform phi to ensure positivity
  
  mu <- exp(X %*% beta) / (1 + exp(X %*% beta))  # Mean
  
  # Compute log-likelihood for beta regression
  loglik <- sum(dbeta(y, shape1 = mu * phi, shape2 = (1 - mu) * phi, log = TRUE))
  
  # Add Lasso penalty on beta
  penalty <- lambda * sum(abs(beta))
  
  return(-loglik + penalty)  # Minimize negative log-likelihood
}

optimize_lasso_phi <- function(X, y, lambda, beta_init = NULL, phi_init = 1) {
  if (is.null(beta_init)) {
    beta_init <- rep(0, ncol(X))
  }
  
  # Combine phi and beta for optimization
  params_init <- c(log(phi_init), beta_init)  # Log-transform phi
  
  fit <- optim(
    par = params_init,
    fn = log_likelihood_penalized,
    X = X,
    y = y,
    lambda = lambda,
    method = "L-BFGS-B",  # Algorithm for constrained optimization
    control = list(maxit = 100)
  )
  
  # Extract phi and beta
  phi_hat <- exp(fit$par[1])  # Inverse log-transform to get phi
  beta_hat <- fit$par[-1]     # Beta coefficients
  
  return(list(phi = phi_hat, beta = beta_hat, fit = fit))
}

# Fit the model
fit_phi <- optimize_lasso_phi(
  X = model.matrix(betainfl),
  y = training.b[,]$score,
  lambda = lambda
)

beta_hat_phi <- fit_phi$beta
phi_hat_phi <- fit_phi$phi

mu_pred_phi <- exp(X_test %*% beta_hat_phi) / (1 + exp(X_test %*% beta_hat_phi))

# Evaluate the performance
RMSE_blasso_phi <- sqrt(mean((test.b$score - mu_pred_phi)^2))
MAE_blasso_phi <- mean(abs(test.b$score - mu_pred_phi))
RMSE_blasso_phi
MAE_blasso_phi



#### 9. DECISION TREE AND RANDOM FOREST ####

library(mlrMBO)
library(randomForest)
library(rpart)
library(rpart.plot)


plotResPath <- function(results, modelname = NULL, rounding = 1) {
  # results$bestSeen <- cummax(results$acc.test.mean)
  # best <- which.max(results$acc.test.mean)
  # minval <- max(0.8, min(results$acc.test.mean))
  results$bestSeen <- cummin(results$ber.test.mean)
  best <- which.min(results$ber.test.mean)
  minval <- min(results$ber.test.mean)-0.005
  labelValues <- results[best, 1:(ncol(results)-2), drop = F]
  labelValues <- mutate_if(labelValues, is.numeric,
                           function(x) round(x,rounding))
  labelText <- ifelse(ncol(labelValues) > 1,
                      "Migliori iperparametri: ","Miglior iperparametro: ")
  for (i in 1:length(labelValues)) {
    if (!is.na(labelValues[i])) {
      labelText <- paste0(labelText, names(labelValues)[i], "=",
                          labelValues[i], "; ")}}
  labelText <- substr(labelText, 1, nchar(labelText)-2)
  ggplot(data = results, aes(x = 1:nrow(results))) +
    geom_line(aes(y = ber.test.mean), color = "red3") +
    geom_point(aes(y = ber.test.mean)) +
    geom_line(aes(y = bestSeen), color = "green3", linewidth = 1) +
    labs(x = "Iterazioni", y = "ber.test.mean") +
    theme_test() + coord_cartesian(ylim = c(minval,NA)) +
    ggtitle(paste("Ottimizzazione Bayesiana", modelname)) +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
    annotate("point", x = best, y = results$ber.test.mean[best],
             shape = "diamond", color = "forestgreen", size = 5) +
    annotate("label", x = 0, y = minval, label = labelText,
             hjust = 0.03, vjust = 0, label.padding = unit(0.35, "lines"))
}


#3.5 DT

class_weights <- table(training$class_score)^-1
class_weights <- class_weights / sum(class_weights)
weights <- class_weights[as.character(training$class_score)]

training_RF <- training %>%
  mutate(class_score = factor(class_score, levels = levels(class_score),
                              ordered = FALSE)) %>% #,
  # weights = weights) %>%
  select(-c(score, fips, date))
str(training_RF$class_score)

task <- makeClassifTask(data = training_RF, target = "class_score")

set.seed(123)
par.set.DT <- makeParamSet(
  makeIntegerParam("maxdepth", lower = 3, upper = 20),
  makeIntegerParam("minsplit", lower = 1, upper = 100),
  makeNumericParam("cp", lower = -4, upper = -1, trafo = function(x) 10^x))
ctrl.DT <- makeMBOControl()
ctrl.DT <- setMBOControlTermination(ctrl.DT, iters = 15)
tune.ctrl.DT <- makeTuneControlMBO(mbo.control = ctrl.DT)
learner <- makeLearner("classif.rpart")
run.DT <- tuneParams(learner, task,
                     resampling = makeResampleDesc("CV", iters = 5),
                     measures = ber, par.set = par.set.DT,
                     control = tune.ctrl.DT, show.info = T)

#[Tune] Result: maxdepth=20; minsplit=1; cp=0.0001000011 : ber.test.mean=0.6876120

results.DT <- as.data.frame(run.DT$opt.path)[,1:4]
results.DT$cp <- 10^results.DT$cp
plotResPath(results.DT, "Albero Decisionale",5)

mod.DT <- rpart(class_score ~ ., data = training_RF,
                maxdepth = run.DT$x$maxdepth,
                minsplit = run.DT$x$minsplit,
                cp = run.DT$x$cp, weights = weights)
#mod.DT <- rpart(class ~ ., data = trainingbil,
#                maxdepth = 13, minsplit = 32, cp = 10^-3.825923)

confusionMatrix(predict(mod.DT, type = "class"), training_RF$class_score)
preds.DT <- predict(mod.DT, test, type = "class")
confusionMatrix(preds.DT, test$class_score)


# 3.6 RF

set.seed(123)
par.set.RF <- makeParamSet(
  makeIntegerParam("ntree", lower = 10, upper = 250), #1000
  makeIntegerParam("mtry", lower = 2, upper = 50),
  makeIntegerParam("nodesize", lower = 5, upper = 50))
ctrl.RF <- makeMBOControl()
ctrl.RF <- setMBOControlTermination(ctrl.RF, iters = 10)
ctrl.RF <- setMBOControlInfill(ctrl.RF, opt.focussearch.points = 20)
tune.ctrl.RF <- makeTuneControlMBO(mbo.control = ctrl.RF)
learner <- makeLearner("classif.randomForest")
run.RF <- tuneParams(learner, task,
                     resampling = makeResampleDesc("CV", iters = 5),
                     measures = ber, par.set = par.set.RF,
                     control = tune.ctrl.RF, show.info = T)

#[Tune] Result: ntree=227; mtry=5; nodesize=5 : ber.test.mean=0.7019612

results.RF <- as.data.frame(run.RF$opt.path)[,1:4]
plotResPath(results.RF, "Random Forest")

mod.RF <- randomForest(class_score ~ ., data = training_RF,
                       ntree = run.RF$x$ntree,
                       mtry = run.RF$x$mtry,
                       nodesize = run.RF$x$nodesize)

# mod.RF <- rrandomForest(class_score ~ ., data = training_RF,
#                         ntree = 227,
#                         mtry = 5,
#                         nodesize = 5)

confusionMatrix(predict(mod.RF), training_RF$class_score)
preds.RF <- predict(mod.RF, test)
confusionMatrix(preds.RF, test$class_score)


#### 3.6 RF weighted ####

set.seed(123)
# par.set.RF <- makeParamSet(
#   makeIntegerParam("ntree", lower = 10, upper = 250), #1000
#   makeIntegerParam("mtry", lower = 2, upper = 50),
#   makeIntegerParam("nodesize", lower = 5, upper = 50))
# ctrl.RF <- makeMBOControl()
# ctrl.RF <- setMBOControlTermination(ctrl.RF, iters = 10)
# ctrl.RF <- setMBOControlInfill(ctrl.RF, opt.focussearch.points = 20)
# tune.ctrl.RF <- makeTuneControlMBO(mbo.control = ctrl.RF)
learner_wt <- makeLearner("classif.randomForest", classwt = class_weights)
run.RF_wt <- tuneParams(learner_wt, task,
                        resampling = makeResampleDesc("CV", iters = 5),
                        measures = ber, par.set = par.set.RF,
                        control = tune.ctrl.RF, show.info = T)

#[Tune] Result: ntree=249; mtry=12; nodesize=17 : ber.test.mean=0.5817110

results.RF_wt <- as.data.frame(run.RF_wt$opt.path)[,1:4]
plotResPath(results.RF_wt, "Random Forest")

mod.RF_wt <- randomForest(class_score ~ ., data = training_RF,
                          ntree = run.RF_wt$x$ntree,
                          mtry = run.RF_wt$x$mtry,
                          nodesize = run.RF_wt$x$nodesize,
                          classwt = class_weights,
                          importance = T)

mod.RF_wt <- randomForest(class_score ~ ., data = training_RF,
                          ntree = 249,
                          mtry = 12,
                          nodesize = 17,
                          classwt = class_weights,
                          importance = T)

confusionMatrix(predict(mod.RF_wt), training_RF$class_score)

plot(mod.RF_wt, main = "Random Forest using weights Error Rates")
legend("topright", legend = c("Error Rate: None",
                              "Error Rate: D0",
                              "Error Rate: D1",
                              "Error Rate: D2",
                              "Error Rate: D3",
                              "Error Rate: D4",
                              "Overall Error Rate"),
       col = c(2, 3, 4, 5, 6, 1, 1),  
       lty = c(2, 3, 4, 2, 1, 2, 1),
       cex = 0.8)

varImpPlot(mod.RF_wt, n.var = 20, main = "Variables Importance")

preds.RF_wt <- predict(mod.RF_wt, test)
confusionMatrix(preds.RF_wt, test$class_score)


# 3.6 RF cutoff

set.seed(123)
# par.set.RF <- makeParamSet(
#   makeIntegerParam("ntree", lower = 10, upper = 250), #1000
#   makeIntegerParam("mtry", lower = 2, upper = 5),
#   makeIntegerParam("nodesize", lower = 5, upper = 50))
# ctrl.RF <- makeMBOControl()
# ctrl.RF <- setMBOControlTermination(ctrl.RF, iters = 10)
# ctrl.RF <- setMBOControlInfill(ctrl.RF, opt.focussearch.points = 20)
# tune.ctrl.RF <- makeTuneControlMBO(mbo.control = ctrl.RF)
learner <- makeLearner("classif.randomForest",
                       cutoff = table(training_RF$class_score)/nrow(training_RF))
run.RF_cut <- tuneParams(learner, task,
                         resampling = makeResampleDesc("CV", iters = 5),
                         measures = ber, par.set = par.set.RF,
                         control = tune.ctrl.RF, show.info = T)

#[Tune] Result: ntree=250; mtry=21; nodesize=17 : ber.test.mean=0.5031334

results.RF_cut <- as.data.frame(run.RF_cut$opt.path)[,1:4]
plotResPath(results.RF_cut, "Random Forest")

mod.RF_cut <- randomForest(class_score ~ ., data = training_RF,
                           ntree = run.RF_cut$x$ntree,
                           mtry = run.RF_cut$x$mtry,
                           nodesize = run.RF_cut$x$nodesize,
                           cutoff = table(training_RF$class_score)/nrow(training_RF),
                           importance = T)

mod.RF_cut <- randomForest(class_score ~ ., data = training_RF,
                           ntree = 250,
                           mtry = 21,
                           nodesize = 17,
                           cutoff = table(training_RF$class_score)/nrow(training_RF),
                           importance = T)

confusionMatrix(predict(mod.RF_cut), training_RF$class_score)

plot(mod.RF_cut, main = "Random Forest using weights Error Rates")
legend("topright", legend = c("Error Rate: None",
                              "Error Rate: D0",
                              "Error Rate: D1",
                              "Error Rate: D2",
                              "Error Rate: D3",
                              "Error Rate: D4",
                              "Overall Error Rate"),
       col = c(2, 3, 4, 5, 6, 1, 1),  
       lty = c(2, 3, 4, 2, 1, 2, 1),
       cex = 0.8)

varImpPlot(mod.RF_cut, n.var = 20, main = "Variables Importance")

preds.RF_cut <- predict(mod.RF_cut, test)
confusionMatrix(preds.RF_cut, test$class_score)