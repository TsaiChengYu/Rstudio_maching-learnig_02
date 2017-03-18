data <- read.csv("病人原始資料.csv",header = TRUE,sep = ",")
View(data)

tmp = is.na(data)  #find the NA in the data
ruleout = apply(tmp, 1, any)  # create the function of the col to rule out the NA
data = data[!ruleout, ]  # save the data wothout NA (col)

plot(data[,1]); plot(data[,2]);  plot(data[,3]);  plot(data[,4])
plot(data[,5]); plot(data[,6]); plot(data[,7]);plot(data[,8])
plot(data[,9]); plot(data[,10]);  plot(data[,11]);  plot(data[,12])
plot(data[,13]); plot(data[,14]); plot(data[,15]);plot(data[,16])
plot(data[,17]); plot(data[,18]); plot(data[,19]); plot(data[,20])
# 1:sex 2:age 3:height 4:weight 5:BMI  6:kcal 7:protein 8:intake 9:kcal.kg
# 10: protein.need.kg.cbw 11:UUN 12:urine 13:N.cata 14: protein.need.t. 
# 15: pi 16:pe 17:vt 18:ve 19:RR 20:rsi

# 離群值處理
primary_data = subset(data,data$height>120)
primary_data = subset(primary_data,primary_data$weight>30)
primary_data = subset(primary_data,primary_data$vt>100)
primary_data = subset(primary_data,primary_data$ve>0)
primary_data = subset(primary_data,primary_data$RR>0)
primary_data = subset(primary_data,primary_data$urine<4000)

plot(primary_data[,1]); plot(primary_data[,2]);  plot(primary_data[,3]); plot(primary_data[,4])
plot(primary_data[,5]); plot(primary_data[,6]);  plot(primary_data[,7]); plot(primary_data[,8])
plot(primary_data[,9]); plot(primary_data[,10]); plot(primary_data[,11]);plot(primary_data[,12])
plot(primary_data[,13]); plot(primary_data[,14]); plot(primary_data[,15]);plot(primary_data[,16])
plot(primary_data[,17]); plot(primary_data[,18]); plot(primary_data[,19]); plot(primary_data[,20])

# 1:sex 2:age 3:height 4:weight 5:BMI  6:kcal 7:protein 8:intake 9:kcal.kg
# 10: protein.need.kg.cbw 11:UUN 12:urine 13:N.cata 14: protein.need.t. 
# 15: pi 16:pe 17:vt 18:ve 19:RR 20:rsi

primary_data$result<-""
for (i in 1:nrow(primary_data))
{
  if (primary_data[i,15] <= -30 
      && primary_data[i,16]>=30 
      && primary_data[i,17]>=primary_data[i,4]*5 
      && primary_data[i,18]<15 
      && primary_data[i,19]<30
      && primary_data[i,20]<105) {
     primary_data[i, "result"] <- "True"
  } else {
    primary_data[i, "result"] <- "False"
  }
}

#觀看資料特性
primary_data[,21] = factor(primary_data$result)
summary(primary_data[,21])

# 分群
smp.size <- floor(0.8*nrow(primary_data))
set.seed(12)
train.ind <- sample(seq_len(nrow(primary_data)), smp.size)
train <- primary_data[train.ind, ]
test <- primary_data[-train.ind, ]

# 建立方程式
weaning.pred <- as.formula(result~sex+age+BMI+kcal+protein+intake+kcal.kg
                           +protein.need.kg.CBW+UUN+urine 
                           +N.cata+protein.need.t.)

# install package
pkgs <- c("rpart","randomForest","rpart.plot")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs)

# build the CART tree
library("rpart")
library("rpart.plot") 

# CART模型
cart.model<- rpart(weaning.pred,data=train) 
prp(cart.model,         
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    faclen=0
)

pred <- predict(cart.model, newdata=test, type="class")

table(real=test$result, predict=pred)
result = table(real=test$result, predict = pred)  #檢視運算結果
print(result)
p = sum(diag(result))/sum(result)*100
cat("\n\n預測正確率 = ",p,"% \n")

# RF模型
library("randomForest")
model_err_rate=1 
for(i in 1:9) # find the best bundle number 
{
  result=randomForest(formula=weaning.pred,data=train,proximity=TRUE,mtry=i,
                           importance=TRUE,ntree=1000,do.trace=100)
  model_err_rate[i]=mean(result$err.rate)
  cat("第",i,"錯誤率:",model_err_rate[i],"\n")
}
mtry_value=which.min(model_err_rate)


weaning_rf <- randomForest(formula=weaning.pred,data=train,proximity=TRUE,
                        importance=TRUE,ntree=1000,do.trace=10)
plot(weaning_rf)
importance(weaning_rf)
print(weaning_rf)
varImpPlot(weaning_rf)

weaningPred<-predict(weaning_rf,newdata=test)
plot(margin(weaning_rf,test$result))

result = table(real = test$result, predict = weaningPred)  #檢視運算結果
print(result)
p = sum(diag(result))/sum(result)*100
cat("\n\n預測正確率 = ",p,"% \n")

