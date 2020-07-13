setwd("J:/Fraud Detection/creditcard")
creditcard = read.csv("J:/Fraud Detection/creditcard/creditcard.csv", header = TRUE, sep =",")
creditcard$Class = factor(creditcard$Class)
credit_nobs = nrow(creditcard)
train_no= sample(nrow(creditcard),(2/3)*credit_nobs)
test_no = sample(setdiff(1:credit_nobs,train_no),(1/3)*credit_nobs)
training_data = creditcard[train_no,]
test_data = creditcard[test_no,]

training_data %>%
  select(Class) %>%
  group_by(Class) %>%
  summarise(count = n()) %>%
  glimpse

test_data %>%
  select(Class) %>%
  group_by(Class) %>%
  summarise(count = n()) %>%
  glimpse

#This iS DONE
##################################################
sink("summary.txt")
print(summary(V1,V2,V3,V4))
sink()
closeAllConnections()


#random Forest for every variable......................
Model = randomForest(Class~.,data = training_data)
test_data$predicted = predict(Model, test_data)
test_data$predicted = predict(Model, test_data)
confusionMatrix(test_data$Class, test_data$predicted)
F1_all = F1_Score(test_data$Class, test_data$predicted)
options(repr.plot.width = 5, repr.plot.height = 4)
varImpPlot(Model, sort = T, n.var = 10, main = "Top 10 most important Variables")

ModelV17 = randomForest(Class~V17, data = training_data)
test_data$predictedV17 = predict(ModelV17,test_data)
F1_V17 = F1_Score(test_data$Class, test_data$predictedV17)
F1_V17

Model_V17V12 = randomForest(Class~V17 + V12, data = training_data)
test_data$predictedV17V12 = predict(Model_V17V12,test_data)
F1_V17V12 = F1_Score(test_data$Class, test_data$predictedV17V12)
F1_V17V12

Model_V17V12V14 = randomForest(Class~V17 + V12+V14, data = training_data)
test_data$predictedV17V1214 = predict(Model_V17V1214,test_data)
F1_V17V12V14 = F1_Score(test_data$Class, test_data$predictedV17V12V14)
F1_V17V12V14

Model_4 = randomForest(Class~V17 + V12 + V14 + V10, data = training_data)
test_data$predicted4 = predict(Model_4,test_data)
F1_4 = F1_Score(test_data$Class, test_data$predicted4)
F1_4

Model_5 = randomForest(Class~V17 + V12 + V14 + V10 + V16, data = training_data)
test_data$predicted5 = predict(Model_5,test_data)
F1_5 = F1_Score(test_data$Class, test_data$predicted5)
F1_5

Model_10 = randomForest(Class~V17 + V12 + V14 + V10 + V16 + V11 + V9 + V4 + V18 + V26, data = training_data)
test_data$predicted10 = predict(Model_10,test_data)
F1_10 = F1_Score(test_data$Class, test_data$predicted10)
F1_10

###############################################################################
#dataframe of number of variables and scores generated above

number_of_variables = c(1,2,3,4,5,10,17)
F1_score = c(F1_V17, F1_V17V12, F1_V17V12V14, F1_4, F1_5, F1_10, F1_all)
Performance = data.frame(number_of_variables,F1_score)

###############################################################################
plot(Performance$number_ofvariables, Performance$F1_score)
options(repr.plot.width = 4, repr.plot.height = 3)
ggplot(Performance, aes(number_of_variables, F1_score)) + geom_point() + labs(x = "Number of Variables", y = "F1 Score", title = "F1 Score Performance")

#########################################################################

Model_10_2 = randomForest(Class ~ V17 + V12 + V14 + V10 + V16 + V11 + V9 + V4 + V18 + V26,ntree = 1000,data = train)
options(repr.plot.width=repr.plot.height=4)
plot(Model_10_2)
options(repr.plot.width=6, repr.plot.height=4)
plot(Model_10_2, xlim=c(0,100))


