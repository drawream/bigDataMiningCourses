# 大数据分析基础 实验6 段晶晶 2020.04.01

# 启动Rstudio，新建R script 文件，加载试验所需安装包。
install.packages("kknn")
setwd("C://Users//Jayce//Documents//RStudioWorkspace//prj06")

# 获取源数据，并根据数据描述，对每一列重命名
wdbc<-read.csv("./wdbc.data",header = F)#原数据的列名为v1,v2,不好理解，因此
#根据数据描述，对每一列重命名
wdbc.name<-c("Radius","Texture","Perimeter","Area","Smoothness","Compactness","Concavity","Conca
ve points","Symmetry","Fractal dimension")
wdbc.name<-c(wdbc.name,paste(wdbc.name,"_mean",sep=""),paste(wdbc.name,"_worst",sep=""))
names(wdbc)<-c("id","diagnosis",wdbc.name)

str(wdbc)



# 用table 查看比例：
table(wdbc$diagnosis)

# 可以得到有357 个为良性，有212 个为恶性因为id 列没有意义，去掉id 列。
wdbc<-wdbc[-1]


# 将目标属性编码因子化B 良性M 恶性：
diagnosis <-factor(wdbc$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
diagnosis

# 计算各自占比round 四舍五入round(x,digits=n) prop.table 得到边缘概率
round(prop.table(table(diagnosis))*100,digits=1)

# 通过summary 详细地观察3 个特征：可以看出不同特征的度量值差别大。
summary(wdbc[c("Radius_mean","Area_mean","Smoothness_mean")])

# 对数据通过归一化来进行无量纲处理，对数据进行转换
normalize<-function(x){ return((x-min(x))/(max(x)-min(x))) }


# 应用函数as.data.frame()把lapply()返回的列表转换成一个数据框

wdbc_n<-as.data.frame(lapply(wdbc[2:31],normalize))

# 查看其中一个变量的汇总统计量
summary(wdbc$Area_mean)

# 切分数据集 
# 方法一：由于没有新病人的数据，所以使用前469 条记录作为训练数据集，剩下的
# 100 条记录用来模拟新的病人
wdbc_train<-wdbc_n[1:469,]
wdbc_test<-wdbc_n[470:569,]
#存储目标变量标签
wdbc_train_labels<-wdbc[1:469,1]
wdbc_test_labels<-wdbc[470:569,1]
mal_rate<-table(wdbc_train_labels)
round(mal_rate[2]/sum(mal_rate),digits=2)

# 第二种方法：如果样本中的恶性肿瘤大部分分布在1，则将469 作为训练集就有很大问
# 题，此时采用随机取样。

set.seed(1234)
ratio<-sample(1:dim(wdbc_n)[1],469,replace=F)
wdbc_train<-wdbc_n[ratio,]
wdbc_test<-wdbc_n[-ratio,]
wdbc_train_labels<-wdbc[ratio,1]
wdbc_test_labels<-wdbc[-ratio,1]
mal_rate<-table(wdbc_train_labels)
round(mal_rate[2]/sum(mal_rate),digits=2)

# 第三种方法：直接利用"caret"包中的crateDataPartition 函数可自动分区。
library(lattice)
install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)
set.seed(1234)
ratio<-createDataPartition(y=diagnosis,p=0.8,list=FALSE)
wdbc_train<-wdbc_n[ratio,]
wdbc_test<-wdbc_n[-ratio,]
wdbc_train_labels<-wdbc[ratio,1]
wdbc_test_labels<-wdbc[-ratio,1]
mal_rate<-table(wdbc_train_labels)
round(mal_rate[2]/sum(mal_rate),digits=2)

# 使用knn()函数对测试数据进行分类
library(class)
wdbc_test_pred<-knn(train=wdbc_train,test=wdbc_test,cl=wdbc_train_labels,k=21)

# 创建一个用来标识两个向量之间一致性的交叉表
install.packages("gmodels")
library(gmodels)
CrossTable(x=wdbc_test_labels,y=wdbc_test_pred,prop.chisq=FALSE)

# 选取两个变量作为横纵坐标进行画图，观察实际类别与预测的分类结果。
plot(wdbc_test$Texture_mean,wdbc_test$Radius_mean,col=wdbc_test_pred,
     pch=as.integer(wdbc_test_labels))


