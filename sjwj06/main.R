
# 数据仓库与数据挖掘 实验6 段晶晶 2020.04.30


# 1. 根据需要设置工作空间，读取数据“air_data.csv”。
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\sjwj06")
getwd()

# 2. 对原始数据对数据进行数据变换，并使用View()函数查看。
data = read.csv("./air_data.csv")
View(data)

# 3. 按照课件步骤，对数据变换得到的数据进行标准化，并使用head()函数查看标准化得
# 到的数据。
dim(data) # 查看数据结构及概述
names(data) # 查看数据字段
head(data) # 查看数据前6行
col = c(15:18,20:29) # 确定要探索分析的变量
explore_data = data[,col] # 输出数据概览
summary(explore_data)

# 查看缺失值、占总数个数
sum(is.na(explore_data))
sum(is.na(explore_data))/nrow(explore_data)*100

# 丢弃票价为缺失值的记录
delet_na = explore_data[-which(is.na(explore_data$SUM_YR_1) | 
				is.na(explore_data$SUM_YR_2)),]
# 丢弃票价为0，平局折扣率不为0，总飞行数大于0的记录
index = ((delet_na$SUM_YR_1 == 0 & delet_na$SUM_YR_2 == 0)
	*(delet_na$avg_discount != 0)
	*(delet_na$SEG_KM_SUM > 0)
)
deletdata = delet_na[-which(index == 1),]
# 保存清洗后的数据
write.csv(deletdata,'./temp/cleanedfile.csv',row.names=FALSE)

##############################################################################
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\sjwj06")
getwd()

# 2. 对原始数据对数据进行数据变换，并使用View()函数查看。
data = read.csv("./air_data.csv")


delet_na = data[-which(is.na(data$SUM_YR_1) | 
				is.na(data$SUM_YR_2)),]
# 丢弃票价为0，平局折扣率不为0，总飞行数大于0的记录
index = ((delet_na$SUM_YR_1 == 0 & delet_na$SUM_YR_2 == 0)
      	*(delet_na$avg_discount != 0)
      	*(delet_na$SEG_KM_SUM > 0)
)
cleanedfile = delet_na[-which(index == 1),]
# 保存清洗后的数据
write.csv(cleanedfile,'./temp/cleanedfile.csv',row.names=FALSE)

data = read.csv("./air_data.csv", header = T)

names(cleanedfile)

LRFMC <- c('FFP_DATE',
           'LOAD_TIME',
           'FLIGHT_COUNT',
           'SEG_KM_SUM',
           'LAST_FLIGHT_DATE',
           'avg_discount')
reduceddata <- celeanefile[,LRFMC]

#保存数据
write.csv(reduceddata,'./temp/reducedfile.csv',row.name = FALSE)

# 查看数据字段类型
str(reduceddata)

#三个关于时间的字段均为因子型数据，需要将其转换为日期格式，用于计算时间差
reduceddata$FFP_DATE<-as.Date(reduceddata$FFP_DATE)
reduceddata$LOAD_TIME<-as.Date(reduceddata$LOAD_TIME)
reduceddata$LAST_FLIGHT_DATE<-as.Date(reduceddata$LAST_FLIGHT_DATE)
#L:入会至当前时间的间隔；R：最近登机时间间距当前的间隔
reduceddata<-transform(reduceddata,L=difftime(LOAD_TIME,FFP_DATE,units = 'days')/30,
                       R=difftime(LOAD_TIME,LAST_FLIGHT_DATE,units = 'days')/30)

reduceddata$L<-as.numeric(reduceddata$L)
reduceddata$R<-as.numeric(reduceddata$R)
summary(reduceddata)

#na.omit()可以删除所有含有缺失数据的行
reduceddata<-na.omit(reduceddata)
summary(reduceddata)

#摘取有用的数据
transformeddata=reduceddata[,c('L','R','FLIGHT_COUNT','SEG_KM_SUM','avg_discount')]
#为便于查看，对指标进行重命名
colnames(transformeddata)=c('L','R','F','M','C')
#数据写入
write.csv(transformeddata,'./tmp/transformeddata.csv',row.names = FALSE)

View(datafile)


#数据标准化
zscoreddata=scale(transformeddata)
#字段进行重命名。为了区别标准化后的数据，字段名前加一个“Z”
colnames(zscoreddata)=c('ZL','ZR','ZF','ZM','ZC')
#查看标准化数据
head(zscoreddata)

#读取数据
inputfile=read.csv('./tmp/zscoredfile.csv',header = T)
View(inputfile)
#聚类分析
result=kmeans(inputfile,5)
#结果输出
type=result$cluster
table(type)

#安装、导入包
install.packages('fmsb')
library(fmsb)
#绘制雷达图
#设置各个变量的取值范围
centervec<-result$center
#查看中心簇
centervec

max<-apply(centervec,2,max)
min<-apply(centervec, 2, min)
max
min
#取值范围和数据，组合新数据集
df=data.frame(rbind(max,min,centervec))
#绘图
radarchart(df=df,seg=5,plty = 1,vlcex = 0.7)


