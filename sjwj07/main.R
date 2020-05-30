# 数据仓库与数据挖掘 实验7 段晶晶 2020.5.21

# (1).根据需要设置工作空间
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\sjwj07")
getwd()
# (2).使用read.csv()函数读入数据“Iris.csv”
data = read.csv("./Iris.csv")
# View(data)

# (3).把工作空间的建模数据随机分成两部分，一部分用于训练（trainData占总数据80%）
# 另一部分用于测试（testData占总数据20%）
set.seed(1)
ind = sample(2, nrow(data),replace = TRUE, prob=c(0.8,0.2))
trainData = data[ind == 1,]
testData = data[ind == 2,]

write.csv(trainData,"./tmp/trainData.csv",row.names = FALSE)
write.csv(testData,"./tmp/testData.csv",row.names = FALSE)

str(trainData)
trainData <- transform(trainData, calss = as.factor(class))

library(nnet)

# (4).使用nnet包里的nnet()函数以及训练数据构建神经网络模型，使用predict函数和
# 构建的神经网络模型分别对训练数据和测试数据进行分类。（使用四个属性求和，作为
# 分类的自变量。隐藏层设置为10，权值衰减参数设为0.05）
nnet.model = nnet(
  Species ~ SepalLengthCm + SepalWidthCm + PetalLengthCm + PetalWidthCm,
  trainData, size = 10, decay = 0.05)

nnet.model = nnet(
  Species ~ SepalLengthCm + SepalWidthCm + PetalLengthCm + PetalWidthCm,
  testData, size = 10, decay = 0.05)

str(testData)
trainData <- transform(trainData, Species = as.factor(Species))
testData <- transform(testData, Species = as.factor(Species))

# (5).使用混淆矩阵并计算准确率。
#使用测试数据预测
xxt = predict(nnet.model,testData,type = "class")
xxn = predict(nnet.model,trainData,type = "class")

# 建立混淆矩阵
confusiont = table(testData$Species, xxt)
confusionn = table(trainData$Species, xxn)
confusiont
confusionn

accuracyt = sum(diag(confusiont))*100/sum(confusiont)
accuracyn = sum(diag(confusionn))*100/sum(confusionn)

accuracyt
accuracyn


