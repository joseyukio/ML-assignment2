## Assignment 2
## Author: José Yukio Akazawa
###############################

library(data.table)
library(tidyr)
library(caret)
library(mlbench)
library(glmnet)
library(mlogit)
library(mnlogit)
library(logistf)
library(nnet)
library(VGAM)
library(plyr)
library(dplyr)
library(lubridate)
library(ROCR)

## Training/Testing Information:
## You should respect the following train / test split:
## train: ﬁrst 700,000 examples. Well split it into training (.75) and 
## validation (.25). So training=525000, validation=175000
## test: last 178,000 examples
training.size <- 700000
validation.size <- 100000

#training.sample.size <- 350000
## Training
training <- read.csv("data/2015s2-mo444-assignment-02.csv", header = TRUE, sep = ",", nrows = training.size)
## In order to limit the usage of memory use only part of the training.
##
#new.Descript.training <- read.csv("data/pentaho_category_word_wordsplit_day26_trainingR.txt", header = TRUE, sep = ",", nrows = training.size)
## Replace the column Descript by the new Descript
#training$Descript <- new.Descript.training$word

## Validation
validation<- read.csv("data/2015s2-mo444-assignment-02.csv", header = TRUE, sep = ",", nrows = validation.size, skip = training.size)

#training <- sample(training, training.sample.size, replace = TRUE)
## Test
#test <- read.csv("data/2015s2-mo444-assignment-02.csv", header = TRUE, sep = ",", skip = training.size + validation.size)

## Check if there is NA values. There is no NA value.
#TRUE %in% is.na(training)
#TRUE %in% is.na(test)

## Set the column names for validation and test
setnames(validation, colnames(validation), colnames(training))
#setnames(test, colnames(test), colnames(training))

## Reorder the columns so Category is the first one
training <- training[, c("Category", "Dates", "Descript", "DayOfWeek", "PdDistrict", "Resolution", "Address", "X", "Y")]
validation <- validation[, c("Category", "Dates", "Descript", "DayOfWeek", "PdDistrict", "Resolution", "Address", "X", "Y")]



## Split column Dates into Year, Month, day, hour, min.
training <- separate(training, col = Dates, into = c("Date", "Time"), sep = " ")
training <- separate(training, col = Date, into = c("Year", "Month", "Day"), sep = "-")
training <- separate(training, col = Time, into = c("Hour", "Min", "Sec"), sep = ":")

validation <- separate(validation, col = Dates, into = c("Date", "Time"), sep = " ")
validation <- separate(validation, col = Date, into = c("Year", "Month", "Day"), sep = "-")
validation <- separate(validation, col = Time, into = c("Hour", "Min", "Sec"), sep = ":")

#test <- separate(test, col = Dates, into = c("Date", "Time"), sep = " ")
#test <- separate(test, col = Date, into = c("Year", "Month", "Day"), sep = "-")
#test <- separate(test, col = Time, into = c("Hour", "Min", "Sec"), sep = ":")

## Drop column Sec
training <- select(training, -Sec)
validation <- select(validation, -Sec)
#test <- select(test, -Sec)

## Check colnames
colnames(training)
colnames(validation)
#colnames(test)

head(training)

## Check the distribution of the Category
training.distribution <- sort(table(training$Category))
training.distribution <- as.data.frame(training.distribution)

## Check the time distribution
training.hour.distribution <- as.data.frame(sort(table(training$Hour)))
colnames(training.hour.distribution) <- c("#Occurrences")
plot(rownames(training.hour.distribution), training.hour.distribution$`#Occurrences`, xlab = "Hour", ylab = "#Occurrences", main = "Crime occurrences per hour")

##nnet
nn.model <- multinom(Category ~ Hour +  DayOfWeek + X + Y , data = training)
nn.model
nn.predicted <- predict(nn.model)
nn.confm <- confusionMatrix(nn.predicted, training$Category)
nn.confm





#########################################
## Functions
#########################################

###############
# vector of 1 or 0 when find k-value
##############
binomialvector <- function(y,k,ycompare) {
        x<-ycompare[,1]
        ini<-1
        fin<-length(y)
        w<-0
        for (i in ini:fin) {
                if(y[i, 1]==k){
                        x[i]<-ycompare[i]
                        if(w==0){
                                cat(i,ycompare[i]," (",k,") \n")
                                w<-1
                        }
                        
                        else {
                                x[i]<-""
                        }
                }
        }
        return(x)
}
###############
predictvector <- function(y, ypredict) {
        x<-y
        ini<-1
        fin<-length(y)
        w<-0
        for (i in ini:fin) {
                if(y[i,1]==1){
                        #cat("ypredict(",i,")=",ypredict[i,1],"\n")
                        if(ypredict[i,1]>0.5) { # 0.001
                                if(w==0){
                                        cat("ypredict(",i,")=",ypredict[i,1],"\n")
                                        w<-1
                                }
                                
                                x[i,1]<-1
                        } else {
                                x[i,1]<-0
                        }
                }
        }
        
        return(x)
}
###############
binomialpredictvector <- function(y, ypredict,training) {
        x<-y
        ini<-1
        fin<-length(y)
        w<-0
        for (i in ini:fin) {
                if(y[i,1]==1){
                        #cat("ypredict(",i,")=",ypredict[i,1],"\n")
                        if(ypredict[i,1]>0.5) { # 0.001
                                cat("ypredict(",i,")=",ypredict[i,1],"\n")
                                x[i,1]<-training[i,1]
                        } else {
                                x[i,1]<-""
                        }
                } else {
                        x[i,1]<-""
                }
        }
        
        return(x)
}

###############
# vector of 1 or 0 when find k-value
##############
binaryvector <- function(y,k,ycompare) {
        x<-y
        ini<-1
        fin<-length(y)
        w<-0
        for (i in ini:fin) {
                if(y[i,1]==k){
                        x[i,1]<-1
                        if(w==0){
                                cat(ycompare[i,1]," (",k,") \n")
                                w<-1
                        }
                        
                } else {
                        x[i,1]<-0
                }
        }
        
        return(x)
}

logreg <- function(cat,xaux) {
        y<-factor(xaux[,2])
        y<-as.numeric(y)
        y<-as.matrix(y)
        yy<-xaux[,2]
        #yy<-as.matrix(yy)
        x<-xaux
        x[,1]<-factor(x[,1])
        x[,2]<-factor(x[,2])
        #x[,4]<-factor(x[,4])
        #x[,5]<-factor(x[,5])
        #x[,6]<-factor(x[,6])
        x[,7]<-factor(x[,7])
        xx<-x
        xx[,1]<-as.numeric(xx[,1])
        xx[,2]<-as.numeric(xx[,2])
        #xx[,4]<-as.numeric(xx[,4])
        #xx[,5]<-as.numeric(xx[,5])
        #xx[,6]<-as.numeric(xx[,6])
        xx[,7]<-as.numeric(xx[,7])
        head(x,n=10)
        head(xx,n=10)
        yy<-as.matrix(yy)
        head(yy)
        
        y1<-binaryvector(y,cat,yy)
        mylogit <- glm(y1 ~ DayOfWeek + PdDistrict + Address + X + Y, data = xx, family = binomial("logit"))
        #mylogit <- glm(y1 ~ word, data = xx, family = binomial("logit"))
        #mylogit <- glm(y1 ~ Hour, data = xx, family = binomial("logit"))
        #mylogit <- glm(y1 ~ word+DayOfWeek + PdDistrict + Resolution + 
        #           Address + Hour + X + Y + Hour * word + PdDistrict * Address
        #           + word^2
        #           , data = xx, family = binomial("logit"))
        #
        #
        #cf<-coef(mylogit)
        #summary(mylogit)
        #anova(mylogit,test="Chisq")
        
        mylogit.pred<-predict.glm(mylogit, probability=FALSE, type="response")
        head(mylogit.pred)
        
        mylogit.v<-y1
        mylogit.pred<-as.matrix(mylogit.pred)
        mylogit.predt<-predictvector(y1,mylogit.pred)
        #mylogiredt<-ifelse(mylogit.pred>0.5,1,0)
        confm<-confusionMatrix(mylogit.predt,mylogit.v)
        #roc.plot(mylogit.v, mylogt.predt)
        return(list(mylogit,confm))
}


## Training
mlog<-logreg(1,xaux) # k=category, m=matrix
mlog[1] # logistic regression
mlog[2] # confusion matrix after predict
cf<-coef(mlog[[1]]) # logistic regression

## Validation
mvld<-logreg(2,xvld) # k=category, m=matrix
mvld[1] # logistic regression
mvld[2] # confusion matrix after predict
cf<-coef(mlog[[1]]) # logistic regression

## Test
mtst<-logreg(2,xtst) # k=category, m=matrix
mtst[1] # logistic regression
mtst[2] # confusion matrix after predict
cf<-coef(mlog[[1]]) # logistic regression







###########Yukio version##############

###############
#########################################
# Run assignment 2: Logistic Regression #
#########################################
logreg <- function(cat,training) {
        y<-factor(training[,2])
        y<-as.numeric(y)
        y<-as.matrix(y)
        yy<-training[,2]
        #yy<-as.matrix(yy)
        x<-training
        x[,1]<-factor(x[,1])
        x[,2]<-factor(x[,2])
        #x[,4]<-factor(x[,4])
        #x[,5]<-factor(x[,5])
        #x[,6]<-factor(x[,6])
        x[,7]<-factor(x[,7])
        xx<-x
        xx[,1]<-as.numeric(xx[,1])
        xx[,2]<-as.numeric(xx[,2])
        #xx[,4]<-as.numeric(xx[,4])
        #xx[,5]<-as.numeric(xx[,5])
        #xx[,6]<-as.numeric(xx[,6])
        xx[,7]<-as.numeric(xx[,7])
        head(x,n=10)
        head(xx,n=10)
        yy<-as.matrix(yy)
        head(yy)
        
        y1<-binaryvector(y,cat,yy)
        #mylogit <- glm(y1 ~ DayOfWeek + PdDistrict + Address + X + Y, data = xx, family = binomial("logit"))
        mylogit <- glm(y1 ~ DayOfWeek + X + Y, data = xx, family = binomial("logit"))
        #mylogit <- glm(y1 ~ Hour, data = xx, family = binomial("logit"))
        #mylogit <- glm(y1 ~ word+DayOfWeek + PdDistrict + Resolution + 
        #           Address + Hour + X + Y + Hour * word + PdDistrict * Address
        #           + word^2
        #           , data = xx, family = binomial("logit"))
        #
        #
        #cf<-coef(mylogit)
        #summary(mylogit)
        #anova(mylogit,test="Chisq")
        
        mylogit.pred<-predict.glm(mylogit, probability=FALSE, type="response")
        head(mylogit.pred)
        
        mylogit.v<-y1
        mylogit.pred<-as.matrix(mylogit.pred)
        mylogit.predt<-predictvector(y1,mylogit.pred)
        #mylogiredt<-ifelse(mylogit.pred>0.5,1,0)
        confm<-confusionMatrix(mylogit.predt,mylogit.v)
        #roc.plot(mylogit.v, mylogt.predt)
        return(list(mylogit,confm))
}
###########################

#for (i in 17:39){
        print(i)
        
        cat = 1#  
        y<-factor(training[,1]) # Category
        y<-as.numeric(y)
        y<-as.matrix(y)
        yy<-training[,1]
        #yy<-as.matrix(yy)
        x<-training
        x[,1]<-factor(x[,1]) # Category
        x[,7]<-factor(x[,7]) # Descript
        x[,8]<-factor(x[,8]) # DayofWeek
        x[,9]<-factor(x[,9]) # PdDistrict
        x[,9]<-as.numeric(x[,9]) # PdDistrict
        x[,10]<-factor(x[,10]) # Resolution
        x[,11]<-factor(x[,11]) # Address
        xx<-x
        head(x,n=10)
        head(xx,n=10)
        yy<-as.matrix(yy)
        head(yy)
        
        y1<-binaryvector(y,cat,yy)
        
        system.time(mylogit <- glm(y1 ~ Year + Month + Day + Hour + Descript + DayOfWeek + PdDistrict + Resolution + X + Y, data = xx, family = binomial("logit")))
        summary(mylogit)
        
        system.time(mylogit.probs<-predict.glm(mylogit, probability=FALSE, type="response"))
        head(mylogit.probs)
        
        ## Fill in the predicted values based on the 
        mylogit.pred <- ifelse(mylogit.probs > 0.5, 1, 0)
        ## Check if there is probabilities greater than 0.5
        ## If not, the model cannot predict that specific category
        #result <- data.frame()
        result[cat,1] <- as.matrix(table(mylogit.pred > 0.5)[1])
        #result[cat,1] <- table(mylogit.pred > 0.5)
        confusionMatrix(mylogit.pred, y1)
        tmp <- table(mylogit.pred, y1)
        result[cat,2] <- sum(diag(tmp))/sum(tmp)
        saveRDS(result, file = "result.rds")
#}
#









# predob <- prediction(mylogit.probs, y1)
# acc <- performance(predob, "acc")
# plot(acc)
# prec <- performance(predob, "prec")
# plot(prec)
# roc <- performance(predob, "tpr", "fpr")
# plot(roc)
# ## Plot the diagonal 0.5 prob
# abline(0,1, col = "red")



###################
#roc.plot(mylogit.v, mylogt.predt)
#mylogit.v<-y1
#mylogit.pred<-as.matrix(mylogit.pred)
#mylogit.predt<-predictvector(y1,mylogit.pred)
#mylogiredt<-ifelse(mylogit.pred>0.5,1,0)

##########################

## Training
mlog<-logreg(1,training) # k=category, m=matrix
mlog[1] # logistic regression
mlog[2] # confusion matrix after predict
cf<-coef(mlog[[1]]) # logistic regression

## Validation
mvld<-logreg(2,validation) # k=category, m=matrix
mvld[1] # logistic regression
mvld[2] # confusion matrix after predict
cf<-coef(mlog[[1]]) # logistic regression

## Test
mtst<-logreg(2,test) # k=category, m=matrix
mtst[1] # logistic regression
mtst[2] # confusion matrix after predict
cf<-coef(mlog[[1]]) # logistic regression


















##############################################################################
## Attemting using multinomial
## I could not get the proper results usinf mnlogit.
## The accuracy was too low (0.238) which is worst than a flipped coin.
###############################

library(data.table)
library(tidyr)
library(caret)
library(mlbench)
library(glmnet)
library(mlogit)
library(mnlogit)
library(nnet)
library(VGAM)
library(dplyr)
library(lubridate)

## Training/Testing Information:
## You should respect the following train / test split:
## train: ﬁrst 700,000 examples. Well split it into training (.75) and 
## validation (.25). So training=525000, validation=175000
## test: last 178,000 examples
training.size <- 525000 / 2  ## For test we divide by 2
validation.size <- 175000 /2 
test.size <- 178000 /2

training<- fread("data/2015s2-mo444-assignment-02.csv", nrows = training.size)
validation<- fread("data/2015s2-mo444-assignment-02.csv", skip = training.size, nrows = validation.size)
test <- fread("data/2015s2-mo444-assignment-02.csv", skip = training.size + test.size)

## Set the column names for validation and test
setnames(validation, colnames(validation), colnames(training))
setnames(test, colnames(test), colnames(training))

## Split column Dates into Year, Month, day, hour, min.
training <- separate(training, col = Dates, into = c("Date", "Time"), sep = " ")
training <- separate(training, col = Date, into = c("Year", "Month", "Day"), sep = "-")
training <- separate(training, col = Time, into = c("Hour", "Min", "Sec"), sep = ":")

validation <- separate(validation, col = Dates, into = c("Date", "Time"), sep = " ")
validation <- separate(validation, col = Date, into = c("Year", "Month", "Day"), sep = "-")
validation <- separate(validation, col = Time, into = c("Hour", "Min", "Sec"), sep = ":")

test <- separate(test, col = Dates, into = c("Date", "Time"), sep = " ")
test <- separate(test, col = Date, into = c("Year", "Month", "Day"), sep = "-")
test <- separate(test, col = Time, into = c("Hour", "Min", "Sec"), sep = ":")

## Drop column Sec
training <- select(training, -Sec)
validation <- select(validation, -Sec)
test <- select(test, -Sec)


## Check colnames
colnames(training)
colnames(validation)
colnames(test)


## Let's see the frequency of each Category in training
#sorted.Category <- sort(table(training$Category), decreasing = TRUE)
## See the distribution in a pie
#pie(sorted.Category)

########### Using mnlogit.
## We have to prepare the data into long format since this is the only format
## accepted by mlogit or mnlogit.
## I'm also measuring the time to execute this step
system.time(training.long <- mlogit.data(training, shape  = "wide", choice = "Category"))
## You can also use predicted probabilities to help you understand the model
system.time(validation.long <- mlogit.data(validation, shape  = "wide", choice = "Category"))

## Analysis to use mlogit/mnlogit.
## Looking at the data in long format we can see that:
## See ?mformula for help.
## The alternatives are given by Category values. Now in column alt.
## Dates, PdDistrict, Resolution, Address, X and Y do not vary 
## with the alternatives so they are individual specific variables.
## Descript varies with the alternatives. So it is alternative specific.
## In the command we also need to define the reference level from the alternatives.

## Note: For this let's not use variables that vary with the alternatives (Descript).
## If we would use it then it should be the third part of formula which is described as
## "alternative specific variables with alternative specific coefficients"

## Let's use only the ones that do not vary with the alternatives which
## are given in the second part of formula. Also let's not calculate the
## intercept since it is hardly useful in logistic regression.
## Also when the intercep is not used mnlogit show all discrete variables estimates
## and not only the ones based on a reference (reference is not shown)

#system.time(model.baseLine <- mnlogit(Category ~ 0 | DayOfWeek  | 1, data = training.long, ncores= 3, reflevel = "ARSON"))
#summary(model.baseLine)

## The result shows 266 parameters. That is 7 alternatives (DayOfWeek) * (alternaives-1)
## The function always use one of the alternaive as reference.
## The result coefficient are relative to that excluded alternative. In this case
## I explicitle entered via reflevel = "ARSON". So the odds ration of the other
## categories will be in reference to "ARSON".

#system.time(model.baseLine <- mnlogit(Category ~ 1 | DayOfWeek + 0| 1, alt.subset = c("ASSAULT", "ARSON"), data = training.long))
system.time(model.baseLine <- mnlogit(as.factor(Category) ~ 1 | as.factor(DayOfWeek) + 0 | 1, data = training.long, ncores = 3))

## Simple test to check if mnlogit is working. Use the own category as predictor
system.time(model.baseLine <- mnlogit(Category ~ 1 | Address + 0 | 1, data = training.long, ncores = 3))
predicted.training <- predict(model.baseLine, probability = FALSE, newdata = training.long)
conf.matrix.model.baseline <- confusionMatrix(data = predicted.training, reference = as.factor(training$Category))
### There is something wrong with this. Even if we use the category itself
## as predictor the accuracy is not good.
##########

summary(model.baseLine)

## prediction using validation
predicted.validation <- predict(model.baseLine, probability = FALSE, newdata = validation.long)

## Let's use a confusion matrix and evaluate the predicted values compared to the
## observed values.
#conf.matrix.DayXY.no.intercept <- confusionMatrix(data = predicted.validation, validation$Category)
conf.matrix.model.baseline <- confusionMatrix(data = predicted.validation, reference = as.factor(validation$Category))


conf.matrix.Day <- conf.matrix.model.baseline
conf.matrix.month.Pddistric <- conf.matrix.model.baseline
conf.matrix.X.Y <- conf.matrix.model.baseline

## Adding intercept does not changed the accuracy
#conf.matrix.DayXY.with.intercept <- confusionMatrix(data = predicted.validation, validation$Category)

########################################################################
## Test with glmnet
day.f <- factor(training$Day)
my.x <- model.matrix(~ day.f, training)

my.y <- select(training, Category)
model.glmnet <- glmnet(my.x, as.factor(my.y), family="multinomial", type.multinomial="grouped")



## Test with glm

model.glm <- 























## Test with random forest
model.rforest <- randomForest(Category ~ DayOfWeek, data = training, mtry = 5)








#########################################################################
## http://www.ats.ucla.edu/stat/mult_pkg/faq/general/odds_ratio.htm
## Take the exponential of each of the coefficients to generate the odds ratios.
## This tells you how a 1 unit increase or decrease in a variable 
## affects the odds of the category of crime.
## Calculate the odds(Estimate) and confidence interval. 
my.odds <- round(exp(cbind(OddsRatio=coef(model.baseLine), confint(model.baseLine))), 3) # The 3 at the end is the number of decimals we want.
my.odds <- as.data.frame(my.odds)
## Transform rownames into columns
my.odds$Response <- rownames(my.odds)
## Remove the rownames
rownames(my.odds) <- c()

## We can also check the probabilities.
## probability of category = odds/(1 + odds)
my.probs <- round(cbind(my.odds$OddsRatio / (1 + my.odds$OddsRatio)), 3)

my.probs.odds <- cbind(Probs = my.probs, my.odds)
## Reorder the column positions
my.probs.odds <- my.probs.odds[c("Response", "Probs", "OddsRatio", "2.5 %", "97.5 %")]
## Separate the columns Response
my.probs.odds <- separate(my.probs.odds, col = Response, into = c("Day", "Category"), sep = ":")
## Order by Day and Probs
my.probs.odds <- arrange(my.probs.odds, Day, desc(Probs))
#########################################################################



## Compare with nnet
nn.model <- multinom(Category ~  word  + DayOfWeek + X + Y , data = training)
summary(nn.model)
predicted.nn <- predict(nn.model)
comparison.nn <- cbind(Observed = training$Category, as.character(predicted.nn))
tmp.indexes <- (comparison.nn[,1] == comparison.nn[,2])

## http://www.r-bloggers.com/evaluating-logistic-regression-models/
accuracy <- table(predicted.nn, training$Category)
sum(diag(accuracy))/sum(accuracy)
## Or more easily using caret
confusionMatrix(data=predicted.nn, training$Category)


############# Using VGAM
vglm.model <- vglm(Category ~ X, family = multinomial, data = training)
vglm.model_1 <- vglm(Category ~ DayOfWeek, family = multinomial(refLevel = 1), data = small.training)

predicted.vglm <- VGAM::predict(vglm.model, newdata = validation, type = "response")
#######################

############# Using glmnet
glmnet.model <- glmnet(small.training, as.factor(small.training$Category), family = "multinomial")

#######################



























########################################################
## With few modification from:
### https://www.kaggle.com/benhamner/sf-crime/san-francisco-top-crimes-map/files
# Map the occurences of the top 12 crimes in San Francisco

library(dplyr)
library(ggmap)
library(ggplot2)

########################################
## Yukio ## Get the map of SF. This should be done only once.
## It saves the map locally so no need to get the map again.
#map<-get_map(location="sanfrancisco",zoom=12,source="osm")
## Check the map
#plot(map)
#saveRDS(map, file = "data/SFmap.rds")
#######################################

##train <- read_csv("../input/train.csv.zip")
map <- readRDS("data/SFmap.rds")

counts <- summarise(group_by(training, Category), Counts=length(Category))
counts <- counts[order(-counts$Counts),]
# This removes the "Other Offenses" category
top12 <- training[training$Category %in% counts$Category[c(1,3:13)],]

top3 <- training[training$Category %in% counts$Category[c(1,3:4)],]

map.top12 <- ggmap(map) +
        geom_point(data=top12, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
        guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                                     title="Type of Crime")) +
        scale_colour_brewer(type="qual",palette="Paired") + 
        ggtitle("Top 12 Crimes in San Francisco") +
        theme_light(base_size=20) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())

map.top3 <- ggmap(map) +
        geom_point(data=top3, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
        guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                                     title="Type of Crime")) +
        scale_colour_brewer(type="qual",palette="Paired") + 
        ggtitle("Top 3 Crimes in San Francisco") +
        theme_light(base_size=20) +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())

ggsave("sf_top_crimes_map_top12.png", map.top12, width=14, height=10, units="in")

ggsave("sf_top_crimes_map_top3.png", map.top3, width=14, height=10, units="in")




########################################################
## https://nycdatascience.com/correlation-between-weather-condition-and-the-type-of-crime/

library(weatherData)
library(plyr)
library(reshape)
library(RgoogleMaps)
library(ggplot2)
library(lubridate)
library(corrplot)
library(grid)
#Get the weather data in San Francisco from 2014-12-13 to 2015-05-13
d3<- getWeatherForDate("SFO", start_date="2014-12-13",
                       end_date = "2015-05-13",
                       opt_detailed = TRUE,
                       opt_all_columns = TRUE)
temp <- d3[, c(1,2,3,13)]
temp$Time <- format(as.POSIXct(temp$Time, format = "%Y-%m-%d %H:%M%S"), "%m/%d/%Y")
# Coerce the string to PoSIXIt date
temp$TimePST <- format(as.POSIXct(temp$Time, format = "%Y-%m-%d %H:%M%S"), "%H")
temp$Time <- format(as.POSIXct(temp$Time, format = "%Y-%m-%d %H:%M%S"), "%m/%d/%Y")
# Convert weather Condition to 1 for Rain and 0 for Clear or Cloudy day
for (i in 1: length(temp$Conditions)){
        if(temp$Conditions[i] == "Light Rain" || temp$Conditions[i] =="Heavy Rain" ||temp$Conditions[i] =="Rain")
        {temp$Conditions[i] = 1
        }
        else
        {
                temp$Conditions[i] =0
        }
}
#Aggregate the hour of the date for weather in San Francisco
temp$TimePST <- as.integer(temp$TimePST)
temp$Conditions <- as.integer(temp$Conditions)
temp <- ddply(temp, c("TimePST", "Time"),summarise,
              Temperature = mean(TemperatureF),
              Condition = mean(Conditions)
)

########################################################
## https://www.kaggle.com/imchandusg/sf-crime/sfcc-prediction/code

train <- fread("data/2015s2-mo444-assignment-02.csv", nrows = 10000)
#train <- read.csv("F:/San francisco crime classification/train.csv")#
#test <- read.csv("F:/San francisco crime classification/test.csv")#
#Interactive plots reloaded#

table <- table(train$Category,train$PdDistrict)
#color coding for the plot#

#color <- rainbow(n = 7, s = 1, v = 1, start = 0, alpha = 1)#
color<-heat.colors(7, alpha = 1)
#color<-topo.colors(7, alpha = 1)#
#color<-cm.colors(7, alpha = 1)#

#levels <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")#

plot(x = train$Category, y = train$PdDistrict, main = "visualisation of category by district", xlab ="Category",ylab ="PdDistrict",col=color)
plot(x = train$Category, y = train$DayOfWeek, main = "visualisation of category by week", xlab ="Category",ylab ="DayOfWeek",col=color)

#Visualisation with pie chart by Police department district#
pie.train<-table(train$PdDistrict)
pie(pie.train,main = "Visualisation with pie chart by Police department district")





##### 2nd attempt





library(caret)
library(glmnet)
#install.packages("ROCR")
library(ROCR)
library(mlogit)
library(mnlogit)

#xaux <- read.table("data/2015s2-mo444-assignment-02.csv", header = TRUE, sep = ",")
xaux<-read.table("data/pentaho_category_word_wordsplit_day26_trainingR.txt", header = TRUE, sep = ",", nrows = 350000)
xaux<-read.table("data/pentaho_category_word_wordsplit_day26_trainingR.txt", header = TRUE, sep = ",")

xvld<-read.table("data/pentaho_category_word_wordsplit_day26_validationR.txt", header = TRUE, sep = ",")
#xtst<-read.table("data/pentaho_category_word_wordsplit_day26_testR.txt", header = TRUE, sep = ",")



###############
# vector of 1 or 0 when find k-value
##############
binomialvector <- function(y,k,ycompare) {
        x<-ycompare[,1]
        ini<-1
        fin<-length(y)
        w<-0
        for (i in ini:fin) {
                if(y[i, 1]==k){
                        x[i]<-ycompare[i]
                        if(w==0){
                                cat(i,ycompare[i]," (",k,") \n")
                                w<-1
                        }
                        
                        else {
                                x[i]<-""
                        }
                }
        }
        return(x)
}
###############
predictvector <- function(y, ypredict) {
        x<-y
        ini<-1
        fin<-length(y)
        w<-0
        for (i in ini:fin) {
                if(y[i,1]==1){
                        #cat("ypredict(",i,")=",ypredict[i,1],"\n")
                        if(ypredict[i,1]>0.5) { # 0.001
                                if(w==0){
                                        cat("ypredict(",i,")=",ypredict[i,1],"\n")
                                        w<-1
                                }
                                
                                x[i,1]<-1
                        } else {
                                x[i,1]<-0
                        }
                }
        }
        
        return(x)
}
###############
binomialpredictvector <- function(y, ypredict,xaux) {
        x<-y
        ini<-1
        fin<-length(y)
        w<-0
        for (i in ini:fin) {
                if(y[i,1]==1){
                        #cat("ypredict(",i,")=",ypredict[i,1],"\n")
                        if(ypredict[i,1]>0.5) { # 0.001
                                cat("ypredict(",i,")=",ypredict[i,1],"\n")
                                x[i,1]<-xaux[i,1]
                        } else {
                                x[i,1]<-""
                        }
                } else {
                        x[i,1]<-""
                }
        }
        
        return(x)
}

###############
# vector of 1 or 0 when find k-value
##############
binaryvector <- function(y,k,ycompare) {
        x<-y
        ini<-1
        fin<-length(y)
        w<-0
        for (i in ini:fin) {
                if(y[i,1]==k){
                        x[i,1]<-1
                        if(w==0){
                                cat(ycompare[i,1]," (",k,") \n")
                                w<-1
                        }
                        
                } else {
                        x[i,1]<-0
                }
        }
        
        return(x)
}

###############
#########################################
# Run assignment 2: Logistic Regression #
#########################################
logreg2 <- function(cat,xaux,fm) {
        y<-factor(xaux[,1])
        y<-as.numeric(y)
        y<-as.matrix(y)
        yy<-xaux[,1]
        #yy<-as.matrix(yy)
        x<-xaux
        x[,1]<-factor(x[,1])
        #x[,2]<-factor(x[,2])
        #x[,4]<-factor(x[,4])
        #x[,5]<-factor(x[,5])
        #x[,6]<-factor(x[,6])
        x[,7]<-factor(x[,7])
        xx<-x
        xx[,1]<-as.numeric(xx[,1])
        #xx[,2]<-as.numeric(xx[,2])
        #xx[,4]<-as.numeric(xx[,4])
        #xx[,5]<-as.numeric(xx[,5])
        #xx[,6]<-as.numeric(xx[,6])
        xx[,7]<-as.numeric(xx[,7])
        head(x,n=10)
        head(xx,n=10)
        yy<-as.matrix(yy)
        head(yy)
        
        y1<-binaryvector(y,cat,yy)
        fm2<-as.formula(y1~fm)
        #mylogit <- glm(y1~DayOfWeek + PdDistrict + Address + Resolution + X + Y, data = xx, family = binomial("logit"))
        #mylogit <- glm(y1~word+DayOfWeek + PdDistrict + Hours + Address + Resolution + X + Y, data = xx, family = binomial("logit"))
        #
        mylogit <- glm(y1~word+DayOfWeek + PdDistrict + Hours + X + Y, data = xx, family = binomial("logit"))
        #
        #mylogit <- glm(y1 ~ DayOfWeek, data = xx, family = binomial("logit"))
        #mylogit <- glm(y1 ~ Hour, data = xx, family = binomial("logit"))
        #mylogit <- glm(y1 ~ word+DayOfWeek + PdDistrict + Resolution + 
        #           Address + Hour + X + Y + Hour * word + PdDistrict * Address
        #           + word^2
        #           , data = xx, family = binomial("logit"))
        #
        #
        #cf<-coef(mylogit)
        #summary(mylogit)
        #anova(mylogit,test="Chisq")
        
        mylogit.pred<-predict.glm(mylogit, probability=FALSE, type="response")
        #head(mylogit.pred)
        mylogit.v<-y1
        mylogit.pred<-as.matrix(mylogit.pred)
        mylogit.predt<-predictvector(y1,mylogit.pred)
        x<-table(mylogit.predt==1) #check if all zero
        n<-dim(x)
        if(n==2) {cat ("RELY ON CM \n")} else {cat("NOT RELY ON CM \n")}
        #mylogiredt<-ifelse(mylogit.pred>0.5,1,0)
        confm<-confusionMatrix(mylogit.predt,mylogit.v)
        #roc.plot(mylogit.v, mylogt.predt)
        return(list(mylogit,confm))
}

### Run manually
for (i in 1:4){
        print(i)
        
        cat = i
        y<-factor(xaux[,1])
        y<-as.numeric(y)
        y<-as.matrix(y)
        yy<-xaux[,1]
        #yy<-as.matrix(yy)
        x<-xaux
        x[,1]<-factor(x[,1])
        #x[,2]<-factor(x[,2])
        #x[,4]<-factor(x[,4])
        #x[,5]<-factor(x[,5])
        #x[,6]<-factor(x[,6])
        x[,7]<-factor(x[,7])
        xx<-x
        xx[,1]<-as.numeric(xx[,1])
        #xx[,2]<-as.numeric(xx[,2])
        #xx[,4]<-as.numeric(xx[,4])
        #xx[,5]<-as.numeric(xx[,5])
        #xx[,6]<-as.numeric(xx[,6])
        xx[,7]<-as.numeric(xx[,7])
        head(x,n=10)
        head(xx,n=10)
        yy<-as.matrix(yy)
        head(yy)
        
        y1<-binaryvector(y,cat,yy)
        #Using GLM
        mylogit <- glm(y1~ word + DayOfWeek  + X + Y, data = xx, family = binomial("logit"))
        
        ## using mnlogit
        #system.time(training.long <- mlogit.data(xaux, shape  = "wide", choice = "Category"))
        #system.time(model.baseLine <- mnlogit(Category ~ 1 | word + DayOfWeek  + X + Y  + 0 | 1, data = training.long, ncores = 3))
        
        
        mylogit.pred<-predict.glm(mylogit, probability=FALSE, type="response")
        #head(mylogit.pred)
        mylogit.v<-y1
        mylogit.pred<-as.matrix(mylogit.pred)
        mylogit.predt<-predictvector(y1,mylogit.pred)
        #x<-table(mylogit.predt==1) #check if all zero
        #n<-dim(x)
        #if(n==2) {cat ("RELY ON CM \n")} else {cat("NOT RELY ON CM \n")}
        #mylogiredt<-ifelse(mylogit.pred>0.5,1,0)
        #confm<-confusionMatrix(mylogit.predt,mylogit.v)
        
        ### Yukio modification##
        #results <- data.frame()
        ## Store the number
        ## get the number of FALSE and compare to the nrows (605249)
        results[cat,1] <- as.matrix(table(mylogit.pred > 0.5)[1]) 
        print(confusionMatrix(mylogit.predt,mylogit.v))
        tmp <- table(mylogit.predt, mylogit.v)
        results[cat,2] <- sum(diag(tmp))/sum(tmp)
        results[cat,3] <- date()
        saveRDS(results, file = "results.rds")
}
#

##nnet
nn.model <- multinom(Category ~  word  + DayOfWeek + X + Y , data = xaux)
nn.model
nn.predicted <- predict(nn.model)
nn.confm <- confusionMatrix(nn.predicted, xaux$Category)
nn.confm


## Removing Descript
##Is it possible to predict the kind of crime only based on 
## DayOfWeek, PdDistrict, Longitude, Latitude?
nn.model <- multinom(Category ~  DayOfWeek + PdDistrict + X + Y , data = xaux)
nn.model
nn.predicted <- predict(nn.model)
nn.confm <- confusionMatrix(nn.predicted, xaux$Category)
nn.confm

# Removing Descript
## Try to improve the accuracy
nn.model <- multinom(Category ~  Hours + DayOfWeek + PdDistrict + X + Y , data = xaux)
nn.model
nn.predicted <- predict(nn.model)
nn.confm <- confusionMatrix(nn.predicted, xaux$Category)
nn.confm


## using mnlogit
system.time(training.long <- mlogit.data(xaux, shape  = "wide", choice = "Category"))
system.time(model.baseLine <- mnlogit(Category ~ 1 | word + DayOfWeek + 0 | 1, data = training.long, ncores = 3))
predicted.training <- predict(model.baseLine, probability = FALSE, newdata = training.long)
conf.matrix.model.baseline <- confusionMatrix(data = predicted.training, reference = xaux$Category)
### Using the validation
system.time(xvld.long <- mlogit.data(xvld, shape  = "wide", choice = "Category"))
predicted.validation <- predict(model.baseLine, probability = FALSE, newdata = xvld.long)
conf.matrix.model.baseline <- confusionMatrix(data = predicted.validation, reference = xvld$Category)



#system.time(training.long <- mlogit.data(xaux, shape  = "wide", choice = "Category"))
system.time(model.baseLine <- mnlogit(Category ~ 1 | Hours + DayOfWeek + PdDistrict + X + Y + 0 | 1, data = training.long, ncores = 3))
predicted.training <- predict(model.baseLine, probability = FALSE, newdata = training.long)
conf.matrix.model.baseline <- confusionMatrix(data = predicted.training, reference = xaux$Category)
###

## Working with Address
training.table <- sort(table(training$Address), decreasing = T)
nochange <- training.table[1:30]
nochange.df <- data.frame(nochange)
nochange.df$Address <- rownames(nochange.df)
training$Address <- (ifelse(training$Address %in% nochange.df$Address, as.factor(training$Address), as.factor(c("-1000"))))
###########################################################
