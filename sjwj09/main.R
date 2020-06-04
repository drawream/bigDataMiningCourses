
# 数据仓库与数据挖掘 实验9 2020.6.4

# 1. 根据需要设置工作空间，读取数据“Iris.csv”。
setwd("C://Users//Jayce//Documents//RStudioWorkspace//sjwj09")
getwd()
irisdata = read.csv("./Iris.csv")
# View(irisdata)


# 2. 将数据分为三类（事实上鸢尾花卉数据就包括三个种类），Iris数据的第六列是种类
# 提示：	type = 3 index = 6
type=3
index=6

# 3. 按照课件步骤，对数据进行聚类离散化，使用disdata变量存储得到的数据，并使用
# View()函数查看。
# 提示：	typelable = c("A","B","C","D") 
#（依次对应花萼长度、宽度，花瓣长度、宽度属性）
# cols = ncol(datafile[,2:5]) 
# （第2列至第5列，依次为花萼长度、宽度，花瓣长度、宽度属性）
# rows = nrow(datafile[,2:5])

typelabel=c("A","B","C","D")
set.seed(1)
cols=ncol(irisdata[,2:5])
rows=nrow(irisdata[,2:5])
View(irisdata)
# 4. 安装、导入必要的R包（Matrix包、arules包）。
# install.packages("Matrix")
# install.packages("arules")
library(Matrix)
library(arules)

# 5.使用K-means算法得到聚类离散化的数据（设置nstart = 20），将数据转换为
# transactions格式。
disdata=matrix(NA,rows,cols+1)
for(i in 1:cols){
  c1=kmeans(irisdata[,i],type,nstart=20);
  disdata[,i]=paste(typelabel[i],c1$cluster);
}
disdata[,cols+1]=irisdata[,index];
disdata[,cols+1]=paste("H",disdata[,cols+1],seq="");
View(disdata) # 函数查看
colnames(disdata)=c("萼片长度","萼片宽度","花瓣长度","花瓣宽度","种类")
View(disdata)
# View(disdata)
write.csv(disdata,file="file.csv",quote=F,row.names=F)
data=read.csv("file.csv",header=T)
# 将聚类离散化得到的数据，转换为transactions格式
trans=as(data,"transactions")
View(trans)
inspect(trans[1:5])
# 6. 使用apriori()函数，生成关联规则（设置支持度0.3，置信度0.9）。
rules=apriori(trans,parameter=list(support=0.3,confidence=0.9))
rules
# 7. 使用inspect()函数观测关联规则。
inspect(rules)



