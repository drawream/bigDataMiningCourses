
# 大数据分析基础 实验8 段晶晶 2020.04.15

# 设置工作空间
setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\dsj08")

# 1.下载并加载arules 包 
# install.packages("arules") #下载安装arules 包
library(arules) #加载arules 软件包
# 2.查看Groceries 数据集的概要信息
data("Groceries") #获取数据集Groceries
summary(Groceries) #获取数据集Groceries 数据集的概括信息

# 3.查看Groceries 数据集的前十行的详细信息
inspect(Groceries[1:10])

# 4.生成关联规则
#生成关联规则rule0
rules0=apriori(Groceries,parameter=list(support=0.001,confidence=0.5))

# 5.查看rule0 中生成的关连规则
rules0 #显示rule0 中生成关连规则的条数
inspect ( rules0 [ 1:10 ] ) #观测rule0 中前10 条规则

# 6.对生成规则进行强度控制
#将支持度调整为0.005，记为rule1
rulesl=apriori(Groceries,parameter=list(support=0.005,confidence=0.5)) 
rulesl #显示rule1 生成关连规则的条数
# 7.改变输出形式
rules2=apriori(Groceries,parameter=list(support=0.005,confidence=0.60))
#将置信度调整为0.60，记为rule2
rules2 #显示rule2 成关连规则的条数

#将置信度调整为0.64，记为rule3
rules3=apriori(Groceries,parameter=list(support=0.005,confidence=0.64)) 
rules3 #显示rule2 成关连规则的条数

inspect(rules3)

rules.sorted_sup = sort ( rules0, by="support")
#给定置信度阈值为0.5，按支持度排序记为rules.sorted_sup
inspect(rules.sorted_sup [1:5] )
#输出rules.sorted_sup 前5 条强关联规则

rules.sorted_con = sort ( rules0, by="confidence")
#给定支持度阈值为0.001，按置信度排序，记为rules.sorted_con
inspect ( rules.sorted_con [1:5] )
#输出rules.sorted_con 前5 条强关联规则

rules.sorted_lift=sort(rules0, by="lift")
# 给定支持度阈值为0.001 ， 置信度阈值为0.5， 按提升度排序， 记为
rules.sorted_lift
inspect ( rules.sorted_lift [1:5] )
#输出rules.sorted_lift 前5 条强关联规则

itemsets_apr=apriori(Groceries,parameter=list(supp=0.001,
                                              target="frequent itemsets"),
                                              control=list(sort=-1))
#将apriori()中的目标参数取设为频繁项集

itemsets_apr #显示所生成频繁项集的个数
inspect(itemsets_apr[1:5]) #观测前5 个频繁项集

# 8.关联规则的可视化
# install.packages("arulesViz")
library(arulesViz) #加载程序包aruleViz
#生成关联规则rule5
rules5<-apriori(Groceries,parameter
                =list(
                      support=0.002,
                      confidence=0.5
                      )
                )
rules5
plot(rules5)                

# 更改互动参数后的散点图
plot(rules5,interactive = TRUE) #绘制互动散点图

# 绘制Two-key 散点图
plot(rules5,shading = "order",control = list(main="Two-key plot")) 

plot(rules5,method = "grouped") #对rules5 作分组图

# 实验完成




