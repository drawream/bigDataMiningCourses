# 大数据分析基础 段晶晶 2020.03
#设置目录空间 
setwd("C:/Users/Jayce/Documents/RStudioWorkspace/prj05")

#读取数据
finance_data <- read.csv("./finance.csv",header=T)

#查看数据前六行
head(finance_data) 

str(finance_data) #查看数据类型

summary(finance_data) #查看数据变量分布情况

finance_data$月管理费用<- round(finance_data$总利息/120,1)#添加月管理费用新列
#删除贷款金额、期限、月管理费列，得到新的数据框fdata
fdata <- finance_data[,-c(2,3,6)]
z <- fdata$月管理费用 #选出月管理费用一列，命名为z
fdata$Z月管理费 <- (z-mean(z))/sd(z) #对z 进行标准化,并将值添加到新列Z 月管理费
fdata <- fdata[,-13] #删除月管理费用一列
#建立逻辑回归模型
glm_fdata <- glm(申请人数~.,family = binomial(link = "logit"),data=fdata)
summary(glm_fdata) #查看模型分布情况


glm_AIC <- step(glm_fdata) #找出最优化的模型
summary(glm_AIC) #显示最优化的模型


