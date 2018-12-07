#loading the data

library(gdata)
library(dplyr)
library(ggplot2)


# please change the file path

file_S="/Users/jerry/Documents/GitHub/SPOT_R/Masterdata_GB_20181207.csv"
Offers_S<-read.csv(file=file_S,header=TRUE, sep=",")
# converting to tbl_df give a nice view
Offer_S_df<-tbl_df(Offers_S)

# training and testing dataset with label

dataset=Offer_S_df[,c("Age","Title","Address1","Joinedyears","Modaility","RightMaterialType","LeftMaterialType","NextCheck_index","CusStatus")]
# convert categorical data to numerical data
must_convert<-sapply(dataset,is.factor)  #  if a variable needs to be displayed as numeric
Convert_df<-sapply(dataset[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
Final_data<-cbind(dataset[,!must_convert],Convert_df)        # complete data.frame with all variables put together
# data visualization

# par(mfrow=c(2,4))
# # check outliers
# # for(i in 1:2) {
# #   for(j in 1:4){
# #     boxplot(Final_data[,i*j], main=names(Final_data)[i*j])
# #   }
# # }
# # Can filter the outlier or noise
# k=0;
# for(i in 1:2) {
#   for(j in 1:4){
#     k<-k+1
#     hist(Final_data[,k], main=names(Final_data)[k])
#   }
# }

# choose model as we consider the feature are categorical, 
# so we decide to choose Logistics regression model
# V7 is unuseful 
train<-sample_frac(Final_data, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-Final_data[-sid,]

glm.fit <- glm(CusStatus ~ ., 
               data = train,
               family = binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit, 
                     test, 
                     type = "response")
# because the accept ratio=387397/1151930=0.336 so we set glm.probs > 0.4
glm.pred <- ifelse(glm.probs > 0.4, 1, 0)

#confusionMatrix
table(glm.pred,test$CusStatus)
accuracy<-mean(glm.pred == test$CusStatus)
cat("The accuracy is ",accuracy)

# V1 has lowest p-value , most important variable


