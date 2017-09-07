#RECAP
(1:10)^3-5 #numbers 1-10 to the power of three, all then minus five (BODMUS)

iris[1:5, -4] # first five rows of iris but not showing the fourth column

letters[3:1]<-c("a","b","c")

mylist<-list(a="twitter",b=iris,who)
mylist[3] #to extract the who data
mylist["b"] #to extract iris data
mylist[2] #to extract iris data
mylist$b #to extract iris data

cancersite %>% 
  filter(IsCurrent) ->
  cancersite
#use the above to only look at those where IsCurrent is TRUE

cancersite %>% 
  count(CancerSiteLevel2Desc)
#use the above to produce a frequency table showing each of the cancer sites

incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  inner_join(cancersite, by="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc,GenderKey,AgeRangeKey,CountryKey) %>% 
  summarise(IncidenceCount=sum(IncidenceCount))

incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  inner_join(cancersite, by="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc,AgeRangeKey) %>%
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  group_by(CancerSiteLevel2Desc) %>% 
  mutate(Prop=scales::percent(IncidenceCount/sum(IncidenceCount))) %>% 
  select(-IncidenceCount) %>% 
  spread(AgeRangeKey,Prop) %>% 
  View()
#independent (primary) multiple sites is producing NaN for all, so to change this:

incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  inner_join(cancersite, by="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc,AgeRangeKey) %>%
  summarise(IncidenceCount=sum(IncidenceCount)) %>%
  group_by(CancerSiteLevel2Desc) %>%
  mutate(Prop=scales::percent(IncidenceCount/sum(IncidenceCount))) %>% 
  mutate(Prop=ifelse(Prop=="NaN%","-%",Prop)) %>% 
  select(-IncidenceCount) %>%
  spread(AgeRangeKey,Prop) %>%
  View()

incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  inner_join(cancersite, by="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc,AgeRangeKey) %>%
  summarise(IncidenceCount=sum(IncidenceCount)) %>%
  ggplot(aes(x=CancerSiteLevel2Desc,y=IncidenceCount,fill=AgeRangeKey)) + 
  geom_col(position="fill") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()
#need to flip using functions from forcats package so that it runs from young to old, where is currently old to young:
library(forcats)
incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  inner_join(cancersite, by="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc,AgeRangeKey) %>%
  summarise(IncidenceCount=sum(IncidenceCount)) %>%
  ggplot(aes(x=CancerSiteLevel2Desc,y=IncidenceCount,fill=fct_rev(AgeRangeKey))) + 
  geom_col(position="fill") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip() +
  scale_fill_discrete("clarity")

incidence %>% 
  filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
  inner_join(cancersite, by="CancerSiteKey") %>% 
  group_by(CancerSiteLevel2Desc,AgeRangeKey) %>%
  summarise(IncidenceCount=sum(IncidenceCount)) %>% 
  ggplot(aes(x=AgeRangeKey,y=IncidenceCount)) + 
  geom_col(width = 1,fill=rgb(242,76,174,
                                maxColorValue = 255)) +
  facet_wrap(~CancerSiteLevel2Desc, scales="free") +
  theme_void() ->
  plot1

#plotly allows you to produce dynamic charts
library(plotly)

#the following code is not currently running
# incidence %>%
#   filter(IncidenceYearKey==max(IncidenceYearKey)) %>% 
#   inner_join(cancersite,by="CancerSiteKey") %>% 
#   group_by(CancerSiteLevel2Desc,AgeRangeKey) %>% 
#   summarise(IncidenceCount=sum(IncidenceCount)) ->
#   summarydata
# 
# summarydata %>% 
#   plot_ly(x=~AgeRangeKey,y=~IncidenceCount) %>% 
#   add_bars() ->
#   plot2

#plotly chart which will run:
iris %>%
  plot_ly() %>% 
  add_markers(x=~Sepal.Length,y=~Sepal.Width,
              color=~Species)

#can also make 3d charts using plotly:
iris %>%
  plot_ly() %>% 
  add_markers(x=~Sepal.Length,y=~Sepal.Width,z=~Petal.Length,color=~Species)


#MODELLING IN R
#LINEAR REGRESSION: LM FUNCTION

#rm(iris) - as have been working with iris, removing the old version to be able to work from the original raw data

iris %>% 
  lm(Sepal.Length~.,data=.) %>% 
  summary()

iris %>% 
  lm(Sepal.Length~.,data=.) %>% 
  coefficients()

iris %>% 
  lm(Sepal.Length~.,data=.) %>% 
  fitted() ->
  lmfit

iris %>% 
  ggplot(aes(x=1:nrow(.),y=Sepal.Length))+
  geom_point() +
  geom_point(aes(y=lmfit,color="red")) +
  theme(legend.position = "none")

#to look at the error associated with this model fit:
iris %>% 
  ggplot(aes(x=1:nrow(.),y=Sepal.Length-lmfit))+
  geom_point()

#add a line of best fit to see if there is a trend in the errors:
iris %>% 
  ggplot(aes(x=1:nrow(.),y=Sepal.Length-lmfit))+
  geom_point() +
  geom_smooth()

#to view the overall shape of the residuals (NOT RUNNING):
iris %>%
  ggplot(aes(x=Sepal.Length-lmfit))+
  geom_density()

#to view what a roughly normal distribution would look like for this data:
iris %>%
  ggplot(aes(x=Sepal.Length-lmfit))+
  geom_histogram() +
  geom_histogram(aes(x=rnorm(150,
                             mean=mean((Sepal.Length-lmfit)),
                             sd(Sepal.Length-lmfit)),
                     fill="red",alpha=.1)) +
  theme(legend.position = "none")


iris %>% 
  lm(Sepal.Length~.,data=.) %>% 
  plot()
  #need to hit enter 4 times to be able to view all of the plots associated with this output

#to predict using the model:  
iris %>% 
  lm(Sepal.Length~., data=.) %>% 
  predict(iris[5,])

lmfit[5]

iris %>% 
  lm(Sepal.Length~., data=.) %>% 
  predict(iris) ->
  lmpredict

plot(lmfit,lmpredict)

#store the model as using repeatedly:
iris %>% 
  lm(Sepal.Length~., data=.) ->
  lmmodel

#exporting the information from the model:
library(broom)

lmmodel %>% 
  glance() %>% 
  gather(Measure, Value)

#to get original data with additional modelled data, together:  
lmmodel %>% 
  augment() %>% 
  View()

#to visualise:
lmmodel %>% 
  augment() %>% 
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  theme_minimal()

#making predictions from the model in real data:
library(modelr)
iris %>% 
  sample_n(10) %>% 
  modelr::add_predictions(model=lmmodel)

#linear model predicting incidence count data (NORMALLY POISSON AS COUNT DATA):
incidence %>% 
  lm(IncidenceCount ~ CountryKey + GenderKey + AgeRangeKey + IncidenceYearKey , data=.) %>% 
  glance() ->
  modelA
modelA

incidence %>% 
  lm(IncidenceCount ~ CountryKey + AgeRangeKey + IncidenceYearKey , data=.) %>% 
  glance() ->
  modelB
modelB


#LOGISTIC REGRESSION
incidence %>% 
glm(IncidenceCount ~ CountryKey + AgeRangeKey + IncidenceYearKey , data=., family="quasipoisson")

#PACKAGE FOR ODDS TO PROBABILITIES (and vice versa)
install.packages("optiRum")
library(optiRum)

#running a quick model using optiRum to convert all ratios at once
library(broom)
library(modelr)

incidence %>% 
  mutate(LowIncidenceCount=as_factor(ifelse(IncidenceCount<500,"low","high"))) ->
  incidence

#not running - not producing glmmodel
incidence %>% 
  sample_n(10^5) %>% 
  glm(LowIncidenceCount ~ CountryKey + AgeRangeKey + IncidenceYearKey, data=., family="binomial", model=FALSE, x=FALSE, y=FALSE) ->
  glmmodel

incidence %>% 
  sample_n(10) %>% 
  add_predictions(glmmodel) %>% 
  mutate(prob=logit.prob(pred)) %>% 
  select(pred,prob,everything())

#DECISION TREES
install.packages("FFTrees")
library(FFTrees)

#SAMPLING
incidence %>% 
  resample_partition(c("training"=0.7,"testing"=0.3)) ->
  incidence2

incidence2$training %>% 
  as_data_frame() ->
  train

incidence2$testing %>% 
  as_data_frame() ->
  test

train %>% 
  glm(LowIncidenceCount ~ CountryKey + AgeRangeKey + IncidenceYearKey, data=., family="binomial", model=FALSE, x=FALSE, y=FALSE) ->
  glmmodel

test %>% 
  add_predictions(glmmodel) %>% 
  mutate(prob=logit.prob(pred)) %>% 
  select(pred,prob,everything()) ->
  testresults

giniChart(testresults$pred, testresults$LowIncidenceCount)

#BUILDING CONFUSION MATRICES
install.packages("caret")
library(caret)

confusionMatrix(ifelse(testresults$prob<0.5,"low","high"), 
                testresults$LowIncidenceCount)


#DATA PREPARATION
summary(incidence)

#takes a long time to run:
#featurePlot(test, test$LowIncidenceCount)

install.packages("PASWR")
library(PASWR)

#TITANIC DATA
colnames(titanic3)
summary(titanic3)
 
View(titanic3)
titanic3

#template chart:
titanic3 %>% 
  ggplot(aes(x=age)) +
  geom_histogram() ->
  p

p
p+aes(x=sibsp)
p+aes(x=parch)
p+aes(x=fare)

#creating categorical variables
install.packages("Hmisc")
library(Hmisc)

#age:
titanic3 %>% 
  mutate(age=fct_explicit_na(cut2(age,g=10))) %>% 
  group_by(age) %>% 
  summarise(n(),mean(survived)) %>% 
  View()
  #if want to define own cutpoints then use cuts:
titanic3 %>% 
  mutate(age=fct_explicit_na(cut2(age,cuts=(c(0,15,45,Inf))))) %>% 
  group_by(age) %>% 
  summarise(n(),mean(survived)) %>% 
  View()
  #use smbinning to define cut-points based on weight of evidence

#fare:
titanic3 %>% 
  mutate(age=fct_explicit_na(cut2(age,cuts=(c(0,15,45,Inf))))) %>% 
  filter(fare<500) %>% 
  mutate(fare=fct_explicit_na(cut2(fare,g=10))) %>% 
  group_by(fare) %>% 
  summarise(n(),mean(survived)) %>% 
  View()

#model building:
titanic3 %>% 
  mutate(age=fct_explicit_na(cut2(age,cuts=(c(0,15,45,Inf))))) %>% 
  filter(fare<500) %>% 
  glm(survived ~age + fare + pclass + sibsp + parch, data=., family="binomial")

#saving data for manipulation:
titanic3 %>% 
  mutate(age=fct_explicit_na(cut2(age,cuts=(c(0,15,45,Inf))))) %>% 
  filter(fare<500) %>% 
  mutate(survived=as_factor(ifelse(survived,"S","D")))->
  titanic_raw

set.seed(98986)
titanic_raw %>% 
  resample_partition(c("train"=.7, "test"=.3)) ->
  titanic_samples
#bootstrapping - function is 'bootstrap'

titanic_samples$train %>% 
  as_data_frame() %>% 
  preProcess() ->
  scalingModel
  
titanic_samples$train %>% 
  as_data_frame() %>% 
  predict(scalingModel, .) ->
  training
titanic_samples$test %>% 
  as_data_frame() %>% 
  predict(scalingModel, .) ->
  testing

training %>% 
  glm(survived ~ pclass + sibsp + parch + fare + sex + age, data=., family="binomial") ->
  titanic_model

#give an indication of importance of variables included in the model:
varImp(titanic_model)

#looking at the fit of the model:
titanic_model %>% 
  augment() %>% 
  ggplot(aes(x=.fitted)) +
  geom_density() +
  facet_wrap(~survived)

titanic_model %>% 
  augment() %>% 
  ggplot(aes(x=.fitted, group=survived, fill=survived)) +
  geom_density(alpha=.5)
#overlap is where we have difficulties with the predictions



