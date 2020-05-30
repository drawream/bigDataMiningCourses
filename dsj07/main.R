# 大数据分析实验 段晶晶 2020.04.09

# 设置工作空间
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\dsj07")

countries<-read.csv("countries.csv") #读取数据集

# 用dim()和head()函数查看数据的大体情况
dim(countries)
head(countries)

# 对该数据集进行命名处理
names(countries)<-c("country","birth","death") #设置三个变量名字
var<-countries$country #取变量country 的值赋值给var
var<-as.character(var) #将赋的值变为字符型
head(var)
#将数据集countries 的行名命名为相应国家名
for(i in 1:66) row.names(countries)[i]=var[i]
head(countries)
head(countries)

# 将数据集中的样本点进行可视化
plot(countries$birth,countries$death) #画出所有66 个国家和地区的样本点
c<-which(countries$country=="CHINA") #获取中国在数据集中的位置
f<-which(countries$country=="INDIA")
g<-which(countries$country=="UNITED-STATES")
h<-which(countries$country=="JAPAN")
m<-which.max(countries$birth) #获取出生率最高的国家在数据集中的位置
points(countries[c(c,f,g,h,m),-1],pch=16)#以实心圆点标出如上国家和地区的样本点
legend(countries$birth[c],countries$death[c],"CHINA",bty="n",xjust=0.5,cex=0.8) 
#标出中国样本点的图例，图框默认， xjust ：legend 相对于x 轴的对其方式0：
# 左对齐，0.5：居中对齐，1：右对齐，字符大小为0.8
legend(countries$birth[f],countries$death[f],"INDIA",bty="n",xjust=0.5,cex =0.8)
legend(countries$birth[g],countries$death[g],"UNITED-STATES",bty="n",xjust =0.5,cex=0.8)
legend(countries$birth[h],countries$death[h],"JAPAN",bty="n",xjust=0.5,cex =0.8)
legend(countries$birth[m],countries$death[m],countries$country[m],bty="n", xjust=1,cex=0.8)

# 进行K-means 聚类并输出聚类结果
fit_kml<-kmeans(countries[,-1],center=3) #用kmeans 算法对countries 数据集进行聚类，center：中心点
print(fit_kml)

# 查看各类中心点的坐标
fit_kml$centers

# 对聚类相关信息的查看
#分别输出本次聚类的总平方和，组内平方和的总和，以及组间平方和
fit_kml$totss;fit_kml$tot.withinss;fit_kml$betweenss 

fit_kml$betweenss+fit_kml$tot.withinss #计算组间及组内平和的总和

# 将均值聚类结果进行可视化
plot(countries[,-1],pch=(fit_kml$cluster-1)) #将countries 数据集中聚为3 类的样本点以3 种不同形状表示
points(fit_kml$centers,pch=8) #将3 类别的中心点以星号标示
#对类别1 的中心点添加标注
legend(fit_kml$centers[1,1],fit_kml$centers[1,2],"center_1",bty="n",xjust=1,yjust=0,cex=0.8)
legend(fit_kml$centers[2,1]-2,fit_kml$centers[2,2],"center_2",bty="n",xjust=0,yjust=0,cex=0.8)
legend(fit_kml$centers[3,1],fit_kml$centers[3,2],"center_3",bty="n",xjust=0.5,cex=0.8)

# 聚类优度的分析
# 通过1 到65 的循环找出最优的类别数。
result<-rep(0,65)
for(k in 1:65) #对类别数k 取1 至65 进行循环
{fit_km=kmeans(countries[,-1],center=k) #取类别数k，进行k 均值聚类
result[k]<-fit_km$betweenss/fit_km$totss}#计算类别数为k 时的聚类优度，存入result 中
round(result,2) #输出计算所得result,取小数位后两位的结果
round(result,2)

length(result)
# 对上述的结果进行绘图处理

# 对result 简单制图，type 设置画图的类型为b（b 同时画点和线，但点线不相交）
plot(
  1:65,
  result,
  type="b",
  main="Choosing the Optimal Number of Cluster", 
  xlab="number of cluster:1 to 67",
  ylab="betweenss/totss")

#将类别数为10 的点用实心圆标出
points(10,result[10],pch=16) 

#对类别数为10 的点给出其坐标标注（x,y）,x 为其类别数10，y 为其聚类优度（%）
legend(10,result[10],paste("(10,",sprintf("%.1f%%",result[10]*100),")",sep=""),bty="n",xjust=0.3,cex=0.8)

# 取类别数为10 进行重新聚类

fit_km2=kmeans(countries[,-1],center=10) #取类别数参数center 为10，进行k 均值聚类
cluster_CHINA=fit_km2$cluster[which(countries$country=="CHINA")]
#获取中国大陆所属类别，记入cluster_CHINA 变量
which(fit_km2$cluster==cluster_CHINA)#选择出与中国同类别的国家和地区
which(fit_km2$cluster==cluster_CHINA)#选择出与中国同类别的国家和地区
# 完成


# 习题
dim(countries) # 数据情况查看
head(countries)
names(countries)<-c("country","birth","death") # 设置列名
var<-as.character(countries$country)
head(var) # 查看
for(i in 1:66) row.names(countries)[i]=var[i]
head(countries)
fit_hc=hclust(dist(countries[,-1])) # 层次聚类
print(fit_hc) # 打印聚类结果
plot(fit_hc) #结果可视化
group_k3=cutree(fit_hc,k=3) # 剪枝处理
group_k3
# 利用剪枝函数中的参数h控制输出Height=18时的系谱聚类结果
group_h18=cutree(fit_hc,h=18)
group_h18
sapply(unique(group_k3),function(g)countries$country[group_k3==g])
plot(fit_hc)
rect.hclust(fit_hc,k=4,border="light grey")
rect.hclust(fit_hc,k=3,border="dark grey")
rect.hclust(fit_hc,k=7,which=c(2,6),border="darkgrey")
