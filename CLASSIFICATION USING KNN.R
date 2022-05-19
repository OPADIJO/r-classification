#import dataset
knn<- read.csv("heart.csv")
View(knn) 

#check the structure of the dataset
names(knn)
head(knn)
tail(knn)
summary(knn)
str(knn)

#Random forest 
#convert Heart Disease column to factor
knn$HeartDiseaseF<- as.factor(knn$HeartDisease)
#divide the dataset into 2 portions 1 for training and the other for testing 
set.seed(1234)
partition<- sample(2, nrow(knn), replace=T, prob = c(0.8,0.2))
#80% of the dataset is used for training while 20% is used for testing 
train<- knn[partition==1,]
test<- knn[partition==2,]
#install the random forest package and run the library
install.packages("randomForest")
library(randomForest)
#run random forest on the dataset
set.seed(222)
rand<- randomForest(HeartDiseaseF~., data = train)
print(rand)
#prediction$confusion matrix with train data
library(caret)
predict1<- predict(rand, train)
ConfMat<- confusionMatrix(predict1, train$HeartDiseaseF)
ConfMat
#prediction$confusion matrix with test data
predict2<- predict(rand, test)
confMAt2<- confusionMatrix(predict2, test$HeartDiseaseF)
confMAt2

plot(rand)


str(knn)
#KNN
#CONVERT SEX TO AN INTEGER VALUE
knn$Sex[knn$Sex=="M"]<-1
knn$Sex[knn$Sex=="F"]<-0
knn$Sex<- as.integer(knn$Sex)

#CONVERT CHEST PAIN TYPE TO INTEGER VALUE
knn$ChestPainType[knn$ChestPainType =="TA"]<-1
knn$ChestPainType[knn$ChestPainType =="ATA"]<-2
knn$ChestPainType[knn$ChestPainType =="NAP"]<-3
knn$ChestPainType[knn$ChestPainType =="ASY"]<-4
knn$ChestPainType<- as.integer(knn$ChestPainType)

#CONVERT RESTING ECG TO INTEGER VALUE
knn$RestingECG[knn$RestingECG == "Normal"]<- 1
knn$RestingECG[knn$RestingECG == "ST"]<- 2
knn$RestingECG[knn$RestingECG == "LVH"]<- 3
knn$RestingECG<- as.integer(knn$RestingECG)

#CONVERT EXCERCISE ANGINA TO INTEGER
knn$ExerciseAngina[knn$ExerciseAngina== "Y"]<- 1
knn$ExerciseAngina[knn$ExerciseAngina== "N"]<- 0
knn$ExerciseAngina<- as.integer(knn$ExerciseAngina)

#CONVERT ST SLOPE TO INTEGER 
knn$ST_Slope[knn$ST_Slope== "Up"]<- 2
knn$ST_Slope[knn$ST_Slope== "Flat"]<- 1
knn$ST_Slope[knn$ST_Slope=="Down"]<- 0
knn$ST_Slope<- as.integer(knn$ST_Slope)

View(knn)


#check the structure of the new data set 
str(knn)

#generate a random number that s 80% of the whole dataset
ran<- sample(1:nrow(knn), 0.8*nrow(knn))

#normalize the first 11 columns because they are the predictors
nor<- function(x){(x-min(x))/(max(x)-min(x))}

knn_norm<- as.data.frame(lapply(knn[,c(1,2,3,4,5,6,7,8,9,10,11)],nor))
summary(knn_norm)

#EXTRACT THE TRAINING DATASET
train<- knn_norm[ran,]
#EXTRACT THE TESTING DATASET
test<- knn_norm[-ran,]
#EXTRACT THE Heart Disease COLUMN OF TRAIN DATASET  
train_target_category <- knn[ran, 12]

#EXTRACT THE Heart Disease COLUMN IN THE TEST DATASET TO BE USED TO MEASURE THE ACCURACY
test_target_category<- knn[-ran, 12]

#LOAD THE LIBRARY
library(class)

#check the entire dataset to check if there is any missing value
library(pillar)
glimpse(knn_norm)
str(knn_norm)
summary(knn)

#KNN FUNCTION WHERE VALUE OF K IS THE SQUARE ROOT OF THE ENTIRE OBSERVATION
obes<- knn(train,test,cl=train_target_category, k=30)

#DRAW CONFUSION MATRIX
tab<- table(obes,test_target_category)
tab

#accuracy of the model built. 
accuracy<- function(x){sum(diag(x)/(sum(rowSums(x)))) *100}
accuracy(tab)
