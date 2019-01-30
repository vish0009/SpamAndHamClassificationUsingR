# text analysis using decession tree
library(caret)
library(e1071)
library(tm)
#loading the data
data <- read.csv("data/sms_spam.csv", stringsAsFactors = FALSE)
str(data)
data$type=as.factor(data$type)


# only taking the text strings from the data
data_string=data$text
sampling=sample(5557,4500)
data_tainsample=data[sampling,]
data_testsample=data[-sampling,]
prop.table(table(data_tainsample$type))
#converting the data into vector document
data_corpus=VCorpus(VectorSource(data$text))

# cleaning the data
data_corpus=tm_map(data_corpus,stemDocument)
data_corpus=tm_map(data_corpus,content_transformer(tolower))
data_corpus=tm_map(data_corpus,removePunctuation)
data_corpus=tm_map(data_corpus,removeNumbers)
as.character( data_corpus[[1]])

# converting the data into documenttermmatrix and saving it as dataframe
data_dtm=DocumentTermMatrix(data_corpus)
data_matrix=as.matrix(data_dtm)
data_frame=as.data.frame(data_matrix)



#sampeling the data
train_data=data_frame[1:5000,]
train_labels=data[1:5000,]$type

test_data=data_frame[5001:5575,]
test_labels=data[5001:5575,]$type

prop.table(table(train_labels))
prop.table(table(test_labels))
library(C50)
#creating the model
model=C5.0(train_data,train_labels)

#predict from model
predection=predict(model,test_data)
#checking for accuracy
table(predection,test_labels)
confusionMatrix(predection,test_labels)
