# 使用cart tree 進行預測分析
install.packages("rpart.plot")
library("rpart")
library("rpart.plot") 
View(iris)

#抽樣
smp.size <- floor(0.8*nrow(iris))
set.seed(20)
train.ind <- sample(seq_len(nrow(iris)), smp.size)
train <- iris[train.ind, ]
test <- iris[-train.ind, ]

# build the model
cart.model<- rpart(Species~.,data=train)  # CART模型：Survived作Y，其餘作X
prp(cart.model,         
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    faclen=0
)

pred <- predict(cart.model, newdata=test, type="class")

table(real=test$Species, predict=pred)
result = table(real=test$Species, predict = pred)  #檢視運算結果
print(result)
p = sum(diag(result))/sum(result)*100
cat("\n\n預測正確率 = ",p,"% \n")
