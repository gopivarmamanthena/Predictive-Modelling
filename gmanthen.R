
# Loading data and cleaning-------------

fin.df <- read.csv('Predfinaldata.csv')
fin1.df <- fin.df[which(complete.cases(fin.df)),]

# converting catagorical data to factors--------------

fin1.df[,1] <- as.numeric(fin1.df[,1])
fin1.df[,2] <- as.numeric(fin1.df[,2])
fin1.df[,3] <- as.factor(fin1.df[,3])
fin1.df[,4] <- as.factor(fin1.df[,4])
fin1.df[,5] <- as.numeric(fin1.df[,5])
fin1.df[,6] <- as.factor(fin1.df[,6])
fin1.df[,7] <- as.factor(fin1.df[,7])
fin1.df[,8] <- as.numeric(fin1.df[,8])
fin1.df[,9] <- as.numeric(fin1.df[,9])
fin1.df[,10] <- as.numeric(fin1.df[,10])
fin1.df[,11] <- as.factor(fin1.df[,11])
fin1.df[,12] <- as.factor(fin1.df[,12])
fin1.df[,13] <- as.numeric(fin1.df[,13])
fin1.df[,14] <- as.numeric(fin1.df[,14])
fin1.df[,15] <- as.numeric(fin1.df[,15])
fin1.df[,16] <- as.numeric(fin1.df[,16])
fin1.df[,17] <- as.numeric(fin1.df[,17])
fin1.df[,18] <- as.numeric(fin1.df[,18])
fin1.df[,19] <- as.factor(fin1.df[,19])
fin1.df[,20] <- as.factor(fin1.df[,20])
fin1.df[,21] <- as.factor(fin1.df[,21])
fin1.df[,22] <- as.factor(fin1.df[,22])
fin1.df[,23] <- as.factor(fin1.df[,23])
fin1.df[,24] <- as.factor(fin1.df[,24])
fin1.df[,25] <- as.factor(fin1.df[,25])


fin2.df$TOTALEN <- fin2.df$TOTALEN/1000

# Removing Outliers----------------------

fin2.df <- fin1.df[-which(fin1.df$TOTALEN>25000000 |
                            fin1.df$TOTALEN<10000),]
sum(fin1.df$TOTALEN>25000000 |
      fin1.df$TOTALEN<10000)

fin2.df$TOTALEN <- fin2.df$TOTALEN/1000

# Corrplot-------------

fin4.df <- fin2.df
for (i in 1:25){
  fin4.df[,i] <- as.numeric(fin2.df[,i])
}

library(corrplot)
M <- cor(fin4.df[,-c(2,3,20,23,24)])
corrplot(M)

# Exploratory Data Analysis-------------
# Density plot for Total Energy
hist(fin2.df$TOTALEN, 
     main="Histogram and Density plot for Total Energy", 
     xlab="Energy (10 ^ 6 BTU)", 
     border="blue", 
     col="green",
     probability = TRUE)
lines(density(fin2.df$TOTALEN))

# QQNorm plot for Total Energy--------
qqnorm(fin2.df$TOTALEN)
qqline(fin2.df$TOTALEN)

# Boxplot for Total Energy----------
boxplot(fin2.df$TOTALEN, 
        ylab = 'Total Energy in 10^6 BTU',
        col = 'gold',
        main ='Boxplot for Total Energy')

# checking assumptions of Linear model through linear model plot--------
fin3.df <- fin2.df[,-c(2,3,20,24,23)]
x <- lm(fin3.df$TOTALEN~.,data = fin3.df)
par(mfrow=c(2,2))
plot(x)


# Various Histograms, Boxplots and Violin plots----------
library('ggplot2')

ggplot(fin2.df,
       aes(TOTALEN)) + xlab('Total Energy in 10^6 BTU') + 
  ggtitle('Density Plot')+
  geom_density()

fin2.df$PUBCLIM = factor(fin2.df$PUBCLIM, 
                         levels = c(2,3),
                         labels = c('Mixed-humid',
                                    'Hot-dry/Mixed-dry/Hot-humid'))

ggplot(fin2.df, aes(x=fin2.df$TOTALEN, color = fin2.df$PUBCLIM))+
  geom_density() + labs(color = 'Climate Regions') +
  ggtitle("Density plot for two types of climate regions")+
  xlab("Total Energy used for space conditioning")

fin2.df$PBA = factor(fin2.df$PBA, 
                     levels = c(1,2,4,5,6,7,8,11,12,13,
                                14,15,16,17,18,23,25,26,91),
                     labels = c('Vacant','Office',
                                'Laboratory',
                                'Nonrefrigerated warehouse',
                                'Food sales',
                                'Public order and safety',
                                'Outpatient health care',
                                'Refrigerated warehouse',
                                'Religious worship',
                                'Public assembly','Education',
                                'Food service',
                                'Inpatient health care',
                                'Nursing','Lodging',
                                'Strip shopping mall',
                                'Retail other than mall',
                                'Service','Other'))

ggplot(fin2.df, aes(x=fin2.df$PBA, y=fin2.df$TOTALEN))
+ggtitle('Boxplots for various type of Building activities')
+geom_boxplot(color = 'red',fill = 'yellow')
+xlab('Public Building Activities')
+ ylab('Total Energy in 10^6 BTU')
+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


fin2.df$RFCNS = factor(fin2.df$RFCNS, 
                       levels = c(1,2,3,4,5,6,7,8,9),
                       labels = c('Built-up','Slate or tile shingles',
                                  'Wood shingles, shakes',
                                  'Asphalt, fiberglass',
                                  'Metal surfacing',
                                  'Plastic, rubber, or synthetic sheeting',
                                  'Concrete','No one major type','Other'))




ggplot(fin2.df, aes(factor(fin2.df$RFCNS), fin2.df$TOTALEN))
+ geom_boxplot(aes(fill=fin2.df$RFCNS))
+xlab('Roof Material')
+ ylab('Total Energy in 10^6 BTU')
+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


fin2.df$WLCNS = factor(fin2.df$WLCNS, 
                       levels = c(1,2,3,4,5,6,7,8,9),
                       labels = c('Brick, stone, or stucco',
                                  'Pre-cast concrete panels',
                                  'Concrete block or poured concrete',
                                  'Aluminum, asbestos, plastic',
                                  'Sheet metal panels',
                                  'Window or vision glass',
                                  'Decorative or construction glass',
                                  'No one major type','Other'))

ggplot(fin2.df, aes(factor(fin2.df$WLCNS), fin2.df$TOTALEN))+
  geom_boxplot(aes(fill=fin2.df$WLCNS))+
  xlab('Roof Material')+
  ylab('Total Energy in 10^6 BTU')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


fin2.df$WLCNS = factor(fin2.df$WLCNS, 
                       levels = c(1,2,3,4,5,6,7,8,9),
                       labels = c('Brick, stone, or stucco',
                                  'Pre-cast concrete panels',
                                  'Concrete block or poured concrete',
                                  'Aluminum, asbestos, plastic',
                                  'Sheet metal panels',
                                  'Window or vision glass',
                                  'Decorative or construction glass',
                                  'No one major type','Other'))

ggplot(fin2.df, aes(factor(fin2.df$WLCNS), fin2.df$TOTALEN))+
  geom_boxplot(aes(fill=fin2.df$WLCNS))+
  xlab('Roof Material')+
  ylab('Total Energy in 10^6 BTU')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

fin2.df$YRCONC = factor(fin2.df$YRCONC, 
                        levels = c(1:10),
                        labels = c('Before 1920',
                                   '1920 to 1945',
                                   '1946 to 1959',
                                   '1960 to 1969',
                                   '1970 to 1979',
                                   '1980 to 1989',
                                   '1990 to 1999',
                                   '2000 to 2003',
                                   '2004 to 2007',
                                   '2008 to 2012'))


ggplot(fin2.df, aes(factor(fin2.df$YRCONC), fin2.df$TOTALEN)) + 
  geom_violin(aes(fill=fin2.df$YRCONC)) +xlab('Year Category')+
  ylab('Total Energy in 10^6 BTU')+ labs(fill = 'Year Category')+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(fin2.df, aes(factor(fin2.df$PUBCLIM), fin2.df$TOTALEN)) + 
  geom_violin(aes(fill=fin2.df$PUBCLIM)) +xlab('Climate Region')+
  ylab('Total Energy in 10^6 BTU')+ labs(fill = 'Climate Region')

ggplot(fin2.df, aes(factor(fin2.df$PUBCLIM), fin2.df$TOTALEN)) + 
  geom_boxplot(aes(fill=fin2.df$PUBCLIM))+xlab('Climate Region')+
  ylab('Total Energy in 10^6 BTU')+ labs(fill = 'Climate Region')


fin2.df$OWNTYPE = factor(fin2.df$OWNTYPE, 
                         levels = c(1:10,97),
                         labels = c('Real estate',
                                    ' partnership, LLC, or LLP',
                                    'Individual owner(s)',
                                    'Religious organization',
                                    'Non-profit organization',
                                    'Private academic',
                                    'Other non-government',
                                    'Federal government',
                                    'State government',
                                    'Local government',
                                    'Confidential'))


ggplot(fin2.df, aes(y=(fin2.df$TOTALEN/1000), x=fin2.df$OWNTYPE,
                    fill=fin2.df$OWNTYPE)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~fin2.df$PUBCLIM)+ xlab('Owners')+
  ylab('Total Energy in 10^6 BTU')+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  labs(fill = 'Owner Type')+
  scale_fill_brewer(palette = 'Paired')

ggplot(fin2.df, aes(y=(fin2.df$TOTALEN/1000), x=fin2.df$PBA,
                    fill=fin2.df$PBA)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~fin2.df$PUBCLIM)+ 
  labs(fill = 'Principle Building Activity')+
  xlab('Principle Building Activity')+
  ylab('Total Energy in 10^6 BTU')+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())

# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(fin4.df)

summary(fin2.df)


# Fitting Various Model with Cross Validation Loop-------------
final.df <- fin2.df[,c(1,4:11,13:17,19,21,22,25)]

for (i in 1:18){
  final.df[,i] <- as.integer(final.df[,i])
}


folds <- 10

final.df$folds <- sample(seq(1:folds),size = nrow(final.df),
                         replace = T) 


rmse.train.df <- data.frame(model1 = numeric(folds),
                            model2 = numeric(folds),
                            model3 = numeric(folds),
                            model4 = numeric(folds),
                            model5 = numeric(folds),
                            model6 = numeric(folds),
                            model7 = numeric(folds),
                            model8 = numeric(folds),
                            model9 = numeric(folds))

rmse.test.df  <- data.frame(model1 = numeric(folds),
                            model2 = numeric(folds),
                            model3 = numeric(folds),
                            model4 = numeric(folds),
                            model5 = numeric(folds),
                            model6 = numeric(folds),
                            model7 = numeric(folds),
                            model8 = numeric(folds),
                            model9 = numeric(folds))


r2.train.df   <- data.frame(model1 = numeric(folds),
                            model2 = numeric(folds),
                            model3 = numeric(folds),
                            model4 = numeric(folds),
                            model5 = numeric(folds),
                            model6 = numeric(folds),
                            model7 = numeric(folds),
                            model8 = numeric(folds),
                            model9 = numeric(folds))


#install.packages('earth')
library('earth')
library('ModelMetrics')
library("randomForest")
library('rpart')
library('rpart.plot')
#install.packages('rJava')
#install.packages('bartMachine')

options(java.parameters = " -Xmx5g")

library('rJava')
library('bartMachine')
#install.packages('neuralnet')
library(neuralnet)
#install.packages('e1071')
library('e1071')
set.seed(10)

# Loop Starts here for cross validation------------
for( i in 1:folds){
  
  final.df.train <- final.df[-which(final.df$folds == i),]
  final.df.test  <- final.df[ which(final.df$folds == i),]
  
  
  # MARS Unpruned Model------------
  model1 <- earth(TOTALEN ~ ., data = final.df.train,
                  pmethod = "none")
  
  model1.pred.train <- predict(model1,final.df.train)
  model1.pred.test  <- predict(model1,final.df.test)
  
  rmse.train.df$model1[i] <- rmse(actual = final.df.train$TOTALEN,
                                  predicted = model1.pred.train)
  
  rmse.test.df$model1[i] <- rmse(actual = final.df.test$TOTALEN,
                                 predicted = model1.pred.test)
  
  r2.train.df$model1[i] <- 1 - (sum((model1.pred.train 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  # MARS Pruned Model-------------
  model2 <- earth(TOTALEN ~ ., data = final.df.train)
  
  model2.pred.train <- predict(model2,final.df.train)
  model2.pred.test  <- predict(model2,final.df.test)
  
  rmse.train.df$model2[i] <- rmse(actual = final.df.train$TOTALEN,
                                  predicted = model2.pred.train)
  
  rmse.test.df$model2[i] <- rmse(actual = final.df.test$TOTALEN,
                                 predicted = model2.pred.test)
  
  r2.train.df$model2[i] <- 1 - (sum((model2.pred.train 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  # Random Forest Model-----------
  model3 <- randomForest(TOTALEN ~ ., data = final.df.train, 
                         na.action = na.rpart,OOB = T) 
  model3.pred.train <- predict(model3,final.df.train)
  model3.pred.test  <- predict(model3,final.df.test)    
  
  rmse.train.df$model3[i] <- rmse(actual = final.df.train$TOTALEN,
                                  predicted = model3.pred.train)
  
  rmse.test.df$model3[i] <- rmse(actual = final.df.test$TOTALEN,
                                 predicted = model3.pred.test)
  
  r2.train.df$model3[i] <- 1 - (sum((model3.pred.train 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  varImpPlot(model3, sort = TRUE, n.var = 8,
             nrow(model8$importance))
  
  # Random Forest with important variables----------
  
  model6 <- randomForest(TOTALEN ~ SQFT + PCTERMN +
                           LAPTPN + NFLOOR + WKHRS +
                           HDD65 + GLSSPC +
                           FLCEILHT
                         , data = final.df.train, 
                         na.action = na.rpart,OOB = T) 
  model6.pred.train <- predict(model6,final.df.train)
  model6.pred.test  <- predict(model6,final.df.test)    
  
  rmse.train.df$model6[i] <- rmse(actual = final.df.train$TOTALEN,
                                  predicted = model6.pred.train)
  
  rmse.test.df$model6[i] <- rmse(actual = final.df.test$TOTALEN,
                                 predicted = model6.pred.test)
  
  r2.train.df$model6[i] <- 1 - (sum((model6.pred.train 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  # CART Model----------
  model4 <- rpart(TOTALEN~.,data = final.df.train)
  
  model4.pred.train <- predict(model4,final.df.train)
  model4.pred.test  <- predict(model4,final.df.test)    
  
  rmse.train.df$model4[i] <- rmse(actual = final.df.train$TOTALEN,
                                  predicted = model4.pred.train)
  
  rmse.test.df$model4[i] <- rmse(actual = final.df.test$TOTALEN,
                                 predicted = model4.pred.test)
  
  r2.train.df$model4[i] <- 1 - (sum((model4.pred.train 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  # BART Model----------
  model5.train.cov <- final.df.train[,2:18]
  model5.train.res <- final.df.train$TOTALEN
  
  model5 <- bartMachine(X = model5.train.cov, y = model5.train.res)
  
  model5.pred.train <- predict(model5, new_data = final.df.train[,2:18])
  model5.pred.test <- predict(model5, new_data = final.df.test[,2:18])
  rmse.train.df$model5[i] <- rmse(actual = final.df.train$TOTALEN,
                                  predicted = model5.pred.train)
  
  rmse.test.df$model5[i] <- rmse(actual = final.df.test$TOTALEN,
                                 predicted = model5.pred.test)
  
  r2.train.df$model5[i] <- 1 - (sum((model5.pred.train 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  
  
  
  # SVM-----------
  model7 = svm(final.df.train$TOTALEN~., final.df.train, kernel= 'radial',
               gamma = 0.1, scale =T)
  
  model7.pred.train <- predict(model7,final.df.train)
  model7.pred.test  <- predict(model7,final.df.test)
  
  rmse.train.df$model7[i] <- rmse(actual = final.df.train$TOTALEN,
                                  predicted = model7.pred.train)
  
  rmse.test.df$model7[i] <- rmse(actual = final.df.test$TOTALEN,
                                 predicted = model7.pred.test)
  
  r2.train.df$model7[i] <- 1 - (sum((model7.pred.train 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  # Neural Net-------------
  xyz <- as.formula(paste
                    ("~", paste(names(final.df.train)
                                , collapse="+")))
  
  mat.train <-as.data.frame(model.matrix
                            (xyz, final.df.train))
  
  mat.test<-as.data.frame(model.matrix
                          (xyz,final.df.test))
  
  f1 <- as.formula(paste("TOTALEN~", 
                         paste((names(mat.train)
                                [!names(mat.train)%in%c
                                  ("(Intercept)","TOTALEN")]),
                               collapse="+")))
  
  model8 <- neuralnet(f1,data = mat.train, 
                      hidden = c(6,3), 
                      err.fct="sse", linear.output = T)
  
  
  model8.pred.train <- compute(model8,final.df.train[,1:18])
  model8.pred.test  <- compute(model8,final.df.test[,1:18])
  
  
  rmse.train.df$model8[i] <- sqrt(mean((model8.pred.train$net.result-
                                          final.df.train$TOTALEN)^2))
  
  rmse.test.df$model8[i] <- sqrt(mean((model8.pred.test$net.result-
                                         final.df.test$TOTALEN)^2))
  
  r2.train.df$model8[i] <- 1 - (sum((model8.pred.train$net.result 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  # GAM model---------
  library("gam")
  mod.gam.obj <- gam(TOTALEN ~ ., data = final.df.train)
  
  
  model9 <- step.Gam(mod.gam.obj,scope =
                       list(
                         "PBA" = ~ 1+ PBA,
                         "SQFT" = ~ 1+ SQFT 
                         + s(SQFT, df = 2)
                         + s(SQFT, df = 3) + s(SQFT, df = 4)
                         + s(SQFT, df = 5),
                         "WLCNS" = ~ 1+ WLCNS,
                         "RFCNS" = ~ 1+ RFCNS,
                         "GLSSPC" = ~ 1+ GLSSPC 
                         + s(GLSSPC, df = 2)
                         + s(GLSSPC, df = 3) + s(GLSSPC, df = 4)
                         + s(GLSSPC, df = 5),
                         "NFLOOR" = ~ 1+ NFLOOR +
                           s(NFLOOR, df = 2)
                         + s(NFLOOR, df = 3) 
                         + s(NFLOOR, df = 4)
                         + s(NFLOOR, df = 5),
                         "FLCEILHT" = ~ 1+ FLCEILHT + 
                           s(FLCEILHT, df = 2)
                         + s(FLCEILHT, df = 3) + 
                           s(FLCEILHT, df = 4)
                         + s(FLCEILHT, df = 5),
                         "YRCONC" = ~ 1+ YRCONC,
                         "MONUSE" = ~ 1+ MONUSE + 
                           s(MONUSE, df = 2)
                         + s(MONUSE, df = 3) + 
                           s(MONUSE, df = 4)
                         + s(MONUSE, df = 5),
                         "WKHRS" = ~ 1+ WKHRS +
                           s(WKHRS, df = 2)
                         + s(WKHRS, df = 3) + s(WKHRS, df = 4)
                         + s(WKHRS, df = 5),
                         "PCTERMN" = ~ 1+ PCTERMN 
                         + s(PCTERMN, df = 2)
                         + s(PCTERMN, df = 3) 
                         + s(PCTERMN, df = 4)
                         + s(PCTERMN, df = 5),
                         "LAPTPN" = ~ 1+ LAPTPN + 
                           s(LAPTPN, df = 2)
                         + s(LAPTPN, df = 3) +
                           s(LAPTPN, df = 4)
                         + s(LAPTPN, df = 5),
                         "HDD65" = ~ 1+ HDD65 + 
                           s(HDD65, df = 2)
                         + s(HDD65, df = 3) + s(HDD65, df = 4)
                         + s(HDD65, df = 5),
                         "PUBCLIM" = ~ 1+ PUBCLIM,
                         "NGUSED" = ~ 1+ NGUSED,
                         "PRUSED" = ~ 1+ PRUSED,
                         "OWNTYPE" = ~ 1+ OWNTYPE),
                     
                     direction = "both", 
                     trace =2)
  
  model9.pred.train <- predict(model9,final.df.train)
  model9.pred.test  <- predict(model9,final.df.test)
  
  rmse.train.df$model9[i] <- rmse(actual = final.df.train$TOTALEN,
                                  predicted = model9.pred.train)
  
  rmse.test.df$model9[i] <- rmse(actual = final.df.test$TOTALEN,
                                 predicted = model9.pred.test)
  
  r2.train.df$model9[i] <- 1 - (sum((model9.pred.train 
                                     - final.df.train$TOTALEN)^2)/
                                  sum((final.df.train$TOTALEN - 
                                         mean(final.df.train$TOTALEN))^2))
  
  
}

# Results--------------

results.df  <- data.frame(avg.rmse.test = numeric(9),
                          avg.rmse.train = numeric(9),
                          avg.r2.train = numeric(9))

results.df$avg.rmse.test[1] <- mean(rmse.test.df$model1)
results.df$avg.rmse.test[2] <- mean(rmse.test.df$model2)
results.df$avg.rmse.test[3] <- mean(rmse.test.df$model3)
results.df$avg.rmse.test[4] <- mean(rmse.test.df$model4)
results.df$avg.rmse.test[5] <- mean(rmse.test.df$model5)
results.df$avg.rmse.test[6] <- mean(rmse.test.df$model6)
results.df$avg.rmse.test[7] <- mean(rmse.test.df$model7)
results.df$avg.rmse.test[8] <- mean(rmse.test.df$model8)
results.df$avg.rmse.test[9] <- mean(rmse.test.df$model9)

results.df$avg.rmse.train[1] <- mean(rmse.train.df$model1)
results.df$avg.rmse.train[2] <- mean(rmse.train.df$model2)
results.df$avg.rmse.train[3] <- mean(rmse.train.df$model3)
results.df$avg.rmse.train[4] <- mean(rmse.train.df$model4)
results.df$avg.rmse.train[5] <- mean(rmse.train.df$model5)
results.df$avg.rmse.train[6] <- mean(rmse.train.df$model6)
results.df$avg.rmse.train[7] <- mean(rmse.train.df$model7)
results.df$avg.rmse.train[8] <- mean(rmse.train.df$model8)
results.df$avg.rmse.train[9] <- mean(rmse.train.df$model9)

results.df$avg.r2.train[1] <- mean(r2.train.df$model1)
results.df$avg.r2.train[2] <- mean(r2.train.df$model2)
results.df$avg.r2.train[3] <- mean(r2.train.df$model3)
results.df$avg.r2.train[4] <- mean(r2.train.df$model4)
results.df$avg.r2.train[5] <- mean(r2.train.df$model5)
results.df$avg.r2.train[6] <- mean(r2.train.df$model6)
results.df$avg.r2.train[7] <- mean(r2.train.df$model7)
results.df$avg.r2.train[8] <- mean(r2.train.df$model8)
results.df$avg.r2.train[9] <- mean(r2.train.df$model9)

# Partial Plots for the most Important variables-----------

varImpPlot(model3, sort = TRUE, n.var = 6,
           nrow(model8$importance), 
           main = 'Variable Importance Plot ')
SQFT + PCTERMN +
  LAPTPN + NFLOOR + WKHRS +
  HDD65 

partialPlot(model3,pred.data = final.df, x.var = 'SQFT',
            y.var ='TOTALEN',
            main = 'Partial Dependence on Square Footage')

partialPlot(model3,pred.data = final.df, x.var = 'PCTERMN',
            y.var ='TOTALEN',
            main = 'Partial Dependence on Number of computers')

partialPlot(model3,pred.data = final.df, x.var = 'LAPTPN',
            y.var ='TOTALEN',
            main = 'Partial Dependence on Number of Laptops')

partialPlot(model3,pred.data = final.df, x.var = 'NFLOOR',
            y.var ='TOTALEN',
            main = 'Partial Dependence on Number of Floors')

partialPlot(model3,pred.data = final.df, x.var = 'WKHRS',
            y.var ='TOTALEN',
            main = 'Partial Dependence on Working hours per week')

partialPlot(model3,pred.data = final.df, x.var = 'HDD65',
            y.var ='TOTALEN',
            main = 'Partial Dependence on Heating Degree Days')

# Actual VS predicted--------------

plot(model3.pred.train~final.df.train$TOTALEN,col=19,pch=20,lwd=0.25)
title("Random Forest - Actual vs Fit")
abline(0,1,col='black')
plot(model3.pred.test~final.df.test$TOTALEN,col=10,pch=20,lwd=0.25)
title("Random Forest - Actual vs Predicted")
abline(0,1,col='black')

# Results plots------------
par(oma=c(3,3,0,0),mar=c(6,6,4,4))
boxplot(rmse.test.df, 
        ylab = 'RMSE values',
        col = 'gold',
        las = 2,
        names = c('MARS unpruned','MARS pruned',
                  'Random Forest','CART','BART',
                  'RF (imp variables)','SVM','Neural Net',
                  'GAM'),
        main ='Boxplot for Out-sample RMSE')

boxplot(rmse.train.df, 
        ylab = 'RMSE values',
        col = 'gold',
        las = 2,
        names = c('MARS unpruned','MARS pruned',
                  'Random Forest','CART','BART',
                  'RF (imp variables)','SVM','Neural Net',
                  'GAM'),
        main ='Boxplot for In-sample RMSE')

boxplot(r2.train.df, 
        ylab = 'R^2 values',
        col = 'gold',
        las = 2,
        names = c('MARS unpruned','MARS pruned',
                  'Random Forest','CART','BART',
                  'RF (imp variables)','SVM','Neural Net',
                  'GAM'),
        main ='Boxplot for R^2')


# FINAL MODEL--------------

finalmodel <- randomForest(TOTALEN ~ ., data = final.df, 
                           na.action = na.rpart,OOB = T) 
  
save(finalmodel, file = 'gmanthen.RData')
