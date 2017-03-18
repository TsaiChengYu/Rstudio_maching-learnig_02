#import library
install.packages("randomForest")
library("randomForest")

#抽樣
smp.size <- floor(0.8*nrow(iris))
set.seed(10)
train.ind <- sample(seq_len(nrow(iris)), smp.size)
train <- iris[train.ind, ]
test <- iris[-train.ind, ]


# build the random Forest Model  
model_err_rate=1 
n=ncol(train)-1
for(i in 1:n) # find the best bundle number 
{
  find_result=randomForest(Species~.,data=train,proximity=TRUE,mtry=i,
                           importance=TRUE,ntree=1000,do.trace=100)
  model_err_rate[i]=mean(find_result$err.rate)
  cat("第",i,"個模型錯誤率:",model_err_rate[i],"\n")
}
mtry_value=which.min(model_err_rate)
cat("mtry參數設定為:",mtry_value)

iris_rf <- randomForest(Species~.,data=train,proximity=TRUE,
                         importance=TRUE,ntree=1000,do.trace=100)

# find the important infact of data
plot(iris_rf)
importance(iris_rf)
print(iris_rf)
varImpPlot(iris_rf)

# test the data
irisPred<-predict(iris_rf,newdata=test)
plot(margin(iris_rf,test$Species))



# 混淆矩陣輸出
result = table(real = test$Species, predict = irisPred)  #檢視運算結果
print(result)
p = sum(diag(result))/sum(result)*100
cat("\n\n預測正確率 = ",p,"% \n")

