
#数据仓库与数据挖掘实验四段晶晶2020.04.16

#设置工作空间
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\sjwj04")

getwd()

datafile=read.csv("./中医证型系数.csv")
View(datafile)

summary(datafile)

#参数初始化
#参数离散化分四组，定义全局变量type=4
type=4
#TNM分期处于datafile第八列，定义index=8
index=8

typelabel=c("A","B","C","D","E","F")
set.seed(1)
cols=ncol(datafile[,1:6])
rows=nrow(datafile[,1:6])
cols[1]6
rows[1]930
# 使用disdata变量存储得到的数据
disdata=matrix(NA,rows,cols+1)
for(i in 1:cols){
  c1=kmeans(datafile[,i],type,nstart=20);
  disdata[,i]=paste(typelabel[i],c1$cluster);
}
disdata[,cols+1]=datafile[,index];
disdata[,cols+1]=paste0("H",disdata[,cols+1],seq="");
# View()  函数查看
View(disdata)
colnames(disdata)=c("肝气郁结证型系数","热毒蕴结证型系数","冲任失调证型系数","气血两虚证型系数","脾胃虚弱证型系数","肝肾阴虚证型系数","TNM分期")
View(disdata)
write.csv(disdata,file="processedfile.csv",quote=F,row.names=F)
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\sjwj04")

data=read.csv("processedfile.csv",header=T)
View(data)install.packages("Matrix")
# 安装、导入必要的R包（Matrix包、arules包）
install.packages("Matrix")
install.packages("arules")
library(Matrix)
library(arules)
# 将聚类离散化得到的数据，转换为transactions格式
trans=as(data,"transactions")
inspect(trans[1:5])
# 使用apriori()函数，生成关联规则（设置支持度0.07，置信度0.8）。
rules=apriori(trans,parameter=list(support=0.07,confidence=0.8))
rules
# 使用inspect()函数观测关联规则
inspect(rules)





