# 1. 根据需要设置工作空间，读取数据“air_data.csv”。
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\dsj11")
getwd()
births<-read.csv("./nybirths.dat")
##创建时间序列对象，单位时间内观测值的频数为12，从1946 年的1 月份开始。
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot(birthstimeseries)#画出生成的时间序列数据的折线图，得到时间序列曲线
#进行时间序列的波动趋势分解。

birthstimeseriescomponents<-decompose(birthstimeseries) 
birthstimeseriescomponents$seasonal
#对生成时间序列数据的趋势的、季节性和不规则部分可视化。
plot(birthstimeseriescomponents) 

birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
#去除原始时间序列中的季节性
plot(birthstimeseriesseasonallyadjusted) #可视化季节修正后的数据

