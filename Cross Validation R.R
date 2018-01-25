###########################################################################################
### Cross Validation to select best model among K nearest Neighbors and SVM with different 
##parameters for credit card data
###########################################################################################

### Read the credit card data 
Data <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE) #Load data
#nrow(Data)

### Sampling the data into Train and Test data
set.seed(5)
Sample<- sample(1:nrow(Data), 460)
data = Data[Sample, ]
test<-Data[-Sample,]


#Randomly shuffle the data
set.seed(7)
data<-data[sample(nrow(data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

l=0
n=10

#Perform 10 fold cross validation for KNN
accuracy<-c()
#average1<-c()
#Iterates through 30 different values for K to find the best value
for(i in 1:n){     #Iterates through each of the 10 folds
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE) 
  ## splitting the data in to k folds for validation 
  testData <- data[testIndexes, ]  #Creates Test Data
  trainData <- data[-testIndexes, ] #Creates Train Data
  model.tnn <- train.kknn(V11~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,trainData,kmax=30,distance=1,kernel="rectangular",scaled=TRUE) #Train the model on the train data
  prediction <- predict(model.tnn, testData[,1:10]) #Takes predictor variables of TestData and predict using the trained model
  prediction[prediction > 0.5] = 1  #Assign Labels to predicted values
  prediction[prediction <= 0.5] = 0
  CM <- table(testData[,11], prediction) # Assign a Confusion Matrix showing prediction accuracy
  accuracy[i] <- (sum(diag(CM)))/sum(CM)
  
}

average_knn<-(sum(accuracy)/length(accuracy))*100

### best accuracy -> 82.39

#model.tnn1 <- train.kknn(V11~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10,trainData,kmax=5,distance=1,kernel="rectangular",scaled=TRUE)
#plot(model.tnn1)


### perform k-fold for cross validation


C_param<-c(0.0000001,0.01,1000000)
average1<-c()

for (j in C_param[1:length(C_param)]){
  print (j)
}

for (j in C_param[1:length(C_param)]){
  for(i in 1:n){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    #print(folds)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    #print(j)
    model.ksvm <- ksvm(as.matrix(trainData[,1:10]),as.factor(trainData[,11]),type="C-svc",kernel="vanilladot",C=j,scaled=TRUE)
    #print(j)
    #print(i)
    prediction <- predict(model.ksvm, testData[,1:10])
    #prediction[prediction > 0.5] = 1
    #prediction[prediction <= 0.5] = 0
    CM <- table(testData[,11], prediction)
    accuracy1 <- (sum(diag(CM)))/sum(CM)
    l[i]<-accuracy1
    #print (accuracy1)
    
    #Use the test and train data partitions however you desire...
  }
  average1[which(C_param==j)]<-(sum(l)/length(l))*100
  print(l)
}

## find best C for ksvm

best_C=C_param[which.max(average1)]


###### we get an accuracy of 85.21


#### Our best model is
##### perform best model after k fold  on Initial test and train

model.ksvm <- ksvm(as.matrix(trainData[,1:10]),as.factor(trainData[,11]),type="C-svc",kernel="vanilladot",C=best_C,scaled=TRUE)
prediction <- predict(model.ksvm, test[,1:10])
#prediction[prediction > 0.5] = 1
#prediction[prediction <= 0.5] = 0
CM <- table(test[,11], prediction)
accuracy <- (sum(diag(CM)))/sum(CM)


### now accuracy is 88.14%

