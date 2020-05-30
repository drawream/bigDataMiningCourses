# 数据仓库与数据挖掘第三次试验 段晶晶 2020.04.09



# (1). 根据需要设置工作空间。
# 设置工作空间
setwd("C:/Users/Jayce/Documents/RStudioWorkspace/sjwj03")

# (2). 使用read.csv()函数读入数据“Iris.csv”。
iris = read.csv("./Iris.csv")

# (3). 将数据按“Species”（种类）列，分为三类数据，依次为
#  1. Iris Setosa（山鸢尾）
#  2. Iris Versicolour（杂色鸢尾）
#  3. Iris Virginica（维吉尼亚鸢尾），并保存到
# irisSetosa, irisVersicolour, irisVirginica三个变量中。
# （提示：irisSetosa = subset(iris, iris$Species == "Iris-setosa")）

# 山鸢尾
irisSetosa = subset(iris, iris$Species == "Iris-setosa")

# 杂色鸢尾
irisVersicolour = subset(iris, iris$Species == "Iris-versicolor")

# 维吉尼亚鸢尾
irisVirginica = subset(iris, iris$Species == "Iris-virginica")

# (4). 使用View()函数查看“iris”和“irisVirginica”。
View(iris)
View(irisVirginica)

# (5). 绘制山鸢尾 花萼长度的条形图。指出山鸢尾花萼长度在哪个范围的样本最多。
?barplot
barplot(irisSetosa$SepalLengthCm,names.arg =irisSetosa$Id)

?plot
plot(irisSetosa$SepalLengthCm)
# 4.5~5.5

##############################################
# Sepal Length Cm 萼片长度Cm，
# Sepal Width Cm, 萼片宽度Cm，
# Petal Length Cm, 花瓣长度Cm
# Petal Width Cm, 花瓣宽度Cm，
############################################
# (6). 绘制杂色鸢尾、维吉尼亚鸢尾花萼长度的条形图。
# 杂色鸢尾
barplot(irisVersicolour$SepalLengthCm,names.arg =irisSetosa$Id)
# 维吉尼亚鸢尾花萼
barplot(irisVirginica$SepalLengthCm,names.arg =irisSetosa$Id)

# (7). 绘制山鸢尾、杂色鸢尾、维吉尼亚鸢尾花萼宽度的饼图。
pie(irisSetosa$SepalWidthCm)
pie(irisVersicolour$SepalWidthCm)
pie(irisVirginica$SepalWidthCm)

# (8). 绘制山鸢尾、杂色鸢尾、维吉尼亚鸢尾花瓣长度的直方图。
hist(irisSetosa$PetalLengthCm)
hist(irisVersicolour$PetalLengthCm)
hist(irisVirginica$PetalLengthCm)

#(9). 判断样本数据维吉尼亚鸢尾花瓣宽度是否符合正态分布。
qqnorm(irisVirginica$PetalWidthCm)
qqline(irisVirginica$PetalWidthCm)
# 由图可知，全部数据可分布于一条直线的两端。d大致为正态分布。