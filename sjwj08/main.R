
# 数据挖掘与数据仓库 实验8 段晶晶 2020.5.28

# 设置工作空间
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\sjwj08")
getwd()

data = read.csv("./moment.csv")
View(data)

#数据命名
colnames(data)<-c("class","id","R1","G1","B1","R2","G2","B2","R3","G3","B3")
#数据分割
set.seed(1234)#设置随机种子
#定义序列ind，随机抽取1和2,1的个数占80%，2的个数占20%
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))
trainData <-data[ind==1,]#训练数据
testData <- data[ind==2,]#测试数据
# 数据存储
write.csv(trainData,"./tmp/trainData.csv",row.names=FALSE)
write.csv(testData,"./tmp/testData.csv",row.names=FALSE)
trainData
testData
# 读取数据
trainData=read.csv("./tmp/trainData.csv")
testData=read.csv("./tmp/testData.csv")
#将class列转换为factor类型
trainData<-transform(trainData,class=as.factor(class))
testData<-transform(testData,class=as.factor(class))

##支持向量机分类模型构建
library(e1071)#加载e1071包
#利用svm建立支持向量机分类模型
svm.model<-svm(class~., trainData[,-2])
summary(svm.model)

#建立混淆矩阵
confusion=table(trainData$class,predict(svm.model,trainData,type="class"))
accuracy=sum(diag(confusion))*100/sum(confusion)
confusion
accuracy
#保存输出结果
output_trainData=cbind(trainData,predict(svm.model,trainData,type="class"))
colnames(output_trainData)<-c("class","id","R1","G1","B1","R2","G2","B2","R3","G3","B3","OUTPUT")
write.csv(output_trainData,"./tmp/output_trainData.csv",row.names=FALSE)

#保存支持向量机模型
save(svm.model,file="./tmp/svm.model.RData")

#读取数据
testData=read.csv("./tmp/testData.csv")
#读取模型
load("./tmp/svm.model.RData")

#建立混淆矩阵
confusion=table(testData$class,predict(svm.model,testData,type="class"))
accuracy=sum(diag(confusion))*100/sum(confusion)
confusion
accuracy
#保存输出结果
output_testData=cbind(testData,predict(svm.model,testData,type="class"))
colnames(output_testData)<-c(
  "class","id","R1","G1","B1","R2","G2","B2","R3","G3","B3","OUTPUT")
write.csv(output_testData,"./tmp/output_testData.csv",row.names=FALSE)



