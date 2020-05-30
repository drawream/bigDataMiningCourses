# 数据仓库与数据挖掘 实验五 段晶晶 2020.4.23

# (1). 根据需要设置工作空间，使用read.csv()函数读入数据"Iris.csv"。

setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\sjwj05")
getwd()
iris = read.csv("./Iris.csv")

# (2). 将数据按“Species”（种类）列，分为三类数据，依次为Iris Setosa（山鸢尾）、
# Iris Versicolour（杂色鸢尾），以及Iris Virginica（维吉尼亚鸢尾），并保存到
# irisSetosa, irisVersicolour, irisVirginica三个变量中。
# （提示：irisSetosa = subset(iris, iris$Species == "Iris-setosa")）

# 山鸢尾
irisSetosa = subset(iris, iris$Species == "Iris-setosa")

# 杂色鸢尾
irisVersicolour = subset(iris, iris$Species == "Iris-versicolor")

# 维吉尼亚鸢尾
irisVirginica = subset(iris, iris$Species == "Iris-virginica")


# (3). 使用View()函数查看“irisVersicolour”。
View(irisVirginica)

# (4). 使用table()函数查看数据中杂色鸢尾的花瓣宽度（PetalWidthCm）分布。可知
# 数据中杂色鸢尾的花瓣宽度，有1，1.1，1.2，1.3，1.4，1.5，1.6，1.7，1.8（厘米）
# 九类，每类有不同条数的数据，例如花瓣宽度为1.4，有7条数据。
# （提示：table(irisVersicolour$PetalWidthCm)）
table(irisVersicolour$PetalWidthCm)

##############################################
# Sepal Length Cm 萼片长度Cm，
# Sepal Width Cm, 萼片宽度Cm，
# Petal Length Cm, 花瓣长度Cm
# Petal Width Cm, 花瓣宽度Cm，
############################################

# (5). 计算当花瓣宽度分别为1、1.1、1.2、1.3、1.5、1.6、1.7、1.8，对应的花瓣长度
# 的平均值。并填写表格（见“四、实验结果”）。
# （提示： ）
# options(digits=2)

w1 =mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.0)$PetalLengthCm)
w11=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.1)$PetalLengthCm)
w12=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.2)$PetalLengthCm)
w13=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.3)$PetalLengthCm)
# w14=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.4)$PetalLengthCm)
w15=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.5)$PetalLengthCm)
w16=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.6)$PetalLengthCm)
w17=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.7)$PetalLengthCm)
w18=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.8)$PetalLengthCm)

p = c(1,1.1,1.2,1.3,1.5,1.6,1.7,1.8)
w = c(w1,w11,w12,w13,w15,w16,w17,w18)

# (6). 使用拉格朗日插值法，结合上步得到的表格，计算当花瓣宽度为1.4时，计算得到的
# 花瓣长度结果。（拉格朗日插值法的操作可参考课件代码、自行查阅网络资源；拉格朗日
# 插值法原理，可参考以下网址的范例部分，进行理解
# https://www.cnblogs.com/ECJTUACM-873284962/p/6833391.html）


#########################
##定义变量x为缺失值y对应的x值，yk为已知的所有y值组成的向量，
# xk为yk向量对应的x值向量
lagrange<-function(x,xk,yk){
  n<-length(xk) #n为列的长度
  lagr<-0
  if(length(xk)!=length(yk))
    stop("the length of input values xk and yk is not equal!")
  if(n<2)
    stop("the length of xk and yk should be bigger than 1")
  for(i in 1:n)
  {
    Li<-1
    for(j in 1:n)
    {
      if(i!=j)
        Li<-Li*(x-xk[j])/(xk[i]-xk[j])
    }
    lagr<-Li*yk[i]+lagr
  }
  return(lagr)
}

#############################

w141=lagrange(1.4,p,w)
w141


# (7). 查看irisVersicolour的原数据，当花瓣宽度为1.4时，花瓣长度如何，并计算花瓣
# 长度的平均值，与插值结果比较。

w14=mean(subset(irisVersicolour, irisVersicolour$PetalWidthCm == 1.4)$PetalLengthCm)
w14



setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\sjwj05")

iris<-read.csv("./Iris.csv")

# View(iris)

irisSetosa = subset(iris, iris$Species == "Iris-setosa")
irisVersicolor = subset(iris, iris$Species == "Iris-versicolor")
irisVirginica = subset(iris, iris$Species == "Iris-virginica")

#View(irisVersicolor)

table(irisVersicolor$PetalWidthCm)

x1 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1)
# View(x1)
y1 = mean(x1$PetalLengthCm)
y1

x2 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1.1)
# View(x2)
y2 = mean(x2$PetalLengthCm)
y2

x3 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1.2)
# View(x3)
y3 = mean(x3$PetalLengthCm)
y3

x4 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1.3)
# View(x4)
y4 = mean(x4$PetalLengthCm)
y4

x5 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1.5)
# View(x5)
y5 = mean(x5$PetalLengthCm)
y5

x6 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1.6)
# View(x6)
y6 = mean(x6$PetalLengthCm)
y6

x7 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1.7)
# View(x7)
y7 = mean(x7$PetalLengthCm)
y7

x8 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1.8)
# View(x8)
y8 = mean(x8$PetalLengthCm)
y8

x9 = subset(irisVersicolor,irisVersicolor$PetalWidthCm == 1.4)
# View(x9)
y9 = mean(x9$PetalLengthCm)
y9

s=c(1,1.1,1.2,1.3,1.5,1.6,1.7,1.8)
b=c(y1,y2,y3,y4,y5,y6,y7,y8)

lagrange<-function(x,xk,yk){
  n<-length(xk) #n为列的长度
  lagr<-0
  if(length(xk)!=length(yk))
    stop("the length of input values xk and yk is not equal!")
  if(n<2)
    stop("the length of xk and yk should be bigger than 1")
  for(i in 1:n)
  {
    Li<-1
    for(j in 1:n)
    {
      if(i!=j)
        Li<-Li*(x-xk[j])/(xk[i]-xk[j])
    }
    lagr<-Li*yk[i]+lagr
  }
  return(lagr)
}

y9=lagrange(1.4,s,b)
y9

mean(y1,y2,y3,y4,y5,y6,y7,y8,y9)
