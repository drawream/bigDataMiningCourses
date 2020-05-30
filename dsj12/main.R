
# 大数据分析基础 实验12 段晶晶 2020.5.27

setwd("C:\\Users\\Jayce\\Documents\\RStudioWorkspace\\dsj12")

# 加载Rmisc 关联包、绘图命令包以及算法包等。

library(plyr) # Rmisc 的关联包,若同时需要加载dplyr 包，必须先加载plyr 包
library(dplyr) # filter()对表格进行过滤
library(ggplot2) # ggplot() 绘制统计图形
library(DT) # datatable() 建立交互式数据表
library(caret) # createDataPartition() 分层抽样函数
library(rpart) # rpart() 运用到简单的分类图像
library(e1071) # naiveBayes() 朴素贝叶斯分类法
library(randomForest) # randomForest() 随机森林模型
library(pROC) # roc()
library(Rmisc) # multiplot() 分割绘图区域
install.packages("klaR")
library(klaR)
# 导入员工离职数据，并进行描述性分析
hr <- read.csv("./HR_comma_sep.csv") # 导入数据
str(hr) # 查看数据的基本数据结构
summary(hr) # 计算数据的主要描述统计量

#将目标变量转换为因子型
hr$left <- factor(hr$left, levels = c('0', '1')) #数据因子化
box_sat <- ggplot(hr, aes(x = left, y = satisfaction_level, fill = left))+
  geom_boxplot(varwidth = T)+
  theme_bw() +
  stat_summary(
    fun = 'mean', geom = 'point', shape = 23, size = 3, fill = 'white'
    ) +
  labs(x = 'left', y = 'satisfaction_level')+
  theme(axis.text.x = element_text(size = rel(1.3)),
        axis.text.y = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3))
        )
  
# varwidth=T 意味着箱子的宽度根据样本数决定
# 一种ggplot 的主题
# 在图中标识均值点，fun.y = 'mean'为求均值，geom = 'point'为绘制点图，
# 后面的是那个参数就是控制点的形状、大小和填充颜色
# axis.text.x 设置横纵坐标标签，
# axis.title.x 设置刻度标签字体大小，rel()控制原始字号的倍数

# 设置坐标轴标签字体大小
hr$left <- factor(hr$left, levels = c('0', '1')) #数据因子化
box_sat <- ggplot(hr, aes(x = left, y = satisfaction_level, fill = left)) +
  geom_boxplot(varwidth = T) +
  theme_bw() +
  stat_summary(fun = 'mean', geom = 'point', shape = 23, size = 3, fill = 'white') +
  labs(x = 'left', y = 'satisfaction_level')+
  theme(axis.text.x = element_text(size = rel(1.3)),
        axis.text.y = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))
# varwidth=T 意味着箱子的宽度根据样本数决定
# 一种ggplot 的主题
# 在图中标识均值点，fun.y = 'mean'为求均值，geom = 'point'为绘制点图，
# 后面的是那个参数就是控制点的形状、大小和填充颜色
# axis.text.x 设置横纵坐标标签，
#axis.title.x 设置刻度标签字体大小，rel()控制原始字号的倍数
# 设置坐标轴标签字体大小

# 绘制绩效评估与是否离职的箱线图
box_eva <- ggplot(hr, aes(x = left, y = last_evaluation, fill = left)) +
  geom_boxplot(varwidth = T) +
  theme_bw() +
  stat_summary(fun = 'mean', geom = 'point', shape = 23, size = 3, fill = 'white') +
  labs(x = 'left', y = 'last_evaluation') +
  theme(axis.text.x = element_text(size = rel(1.3)),
        axis.text.y = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

# 绘制平均月工作时长与是否离职的箱线图
box_mon <- ggplot(hr, aes(x = left, y = average_montly_hours, fill = left)) +
  geom_boxplot(varwidth = T) +
  theme_bw() +
  stat_summary(fun = 'mean', geom = 'point', shape = 23, size = 3, fill = 'white') +
  labs(x = 'left', y = 'average_montly_hours') +
  theme(axis.text.x = element_text(size = rel(1.3)),
        axis.text.y = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))
# 合并这些图形在一个绘图区域，cols = 3 的意思就是排版为一行三列
multiplot(box_sat, box_eva, box_mon, cols = 3)

# 通过各特征之间的关系来探索离职率情况
# 绘制参与项目个数条形图时需要把此变量转换为因子型
hr$number_project <- factor(hr$number_project,
                            levels = c('2', '3', '4', '5', '6', '7'))
bar_pro <- ggplot(hr, aes(x = number_project, fill = left)) +
  geom_bar(position = 'fill') + # position = 'fill' #即绘制百分比堆积条形图
  theme_bw() +
  labs(x = 'left', y = 'number_project') +
  theme(axis.text.x = element_text(size = rel(1.3)),
        axis.text.y = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))
#绘制5 年内是否升职与是否离职的百分比堆积条形图
bar_5years <- ggplot(hr, aes(x = as.factor(promotion_last_5years), fill = left)) +
  geom_bar(position = 'fill') +
  theme_bw() +
  labs(x = 'left', y = 'promotion_last_5years') +
  theme(axis.text.x = element_text(size = rel(1.3)),
        axis.text.y = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))

# 绘制薪资与是否离职的百分比堆积条形图
bar_salary <- ggplot(hr, aes(x = salary, fill = left)) +
  geom_bar(position = 'fill') +
  theme_bw() +
  labs(x = 'left', y = 'salary') +
  theme(axis.text.x = element_text(size = rel(1.3)),
        axis.text.y = element_text(size = rel(1.3)),
        axis.title.x = element_text(size = rel(1.3)),
        axis.title.y = element_text(size = rel(1.3)))
# 合并这些图形在一个绘图区域，cols = 3 的意思就是排版为一+行三列
multiplot(bar_pro, bar_5years, bar_salary, cols = 3)

# filter()用来筛选符合条件的样本
hr_model <- filter(hr, last_evaluation >= 0.70 | time_spend_company >= 4
                   | number_project > 5)
# 设置5 折交叉验证method = ‘cv’是设置交叉验证方法，number = 5 表示5 折交叉验证
train_control <- trainControl(method = 'cv', number = 5)
set.seed(1234) # 设置随机种子，为了使每次抽样结果一致
# 根据数据的因变量进行7:3 的分层抽样，返回行索引向量p = 0.7 就意味着按照7:3 进行抽样，
# list=F 即不返回列表，返回向量
index <- createDataPartition(hr_model$left, p = 0.7, list = F)
traindata <- hr_model[index, ] # 提取数据中的index 所对应行索引的数据作为训练集
testdata <- hr_model[-index, ] # 其余的作为测试集

# 使用caret 包中的trian 函数对训练集使用5 折交叉的方法建立决策树模型
# left ~.的意思是根据因变量与所有自变量建模；trCintrol 是控制使用那种方法进行建模
# methon 就是设置使用哪种类型算法
rpartmodel <- train(left ~ ., data = traindata,
                    trControl = train_control, method = 'rpart')
# 利用rpartmodel 模型对测试集进行预测，（[-7]表示就是剔除测试集的因变量这一列）

pred_rpart <- predict(rpartmodel, testdata[-7])
# 建立混淆矩阵，positive=‘1’设定表示的正例为“1”
con_rpart <- confusionMatrix(testdata$left, pred_rpart, positive = '1')
con_rpart
#构建rfe 函数的控制参数（使用随机森林函数和5 折交叉验证进行特征选择）
rfeControls_rf <- rfeControl(
  functions = rfFuncs, # 设置使用算法进行特征选择
  method = 'cv', # 设置交叉验证的方法
  number = 5) # 确定是5 折交叉验证
#使用rfe 函数（封装法）进行特征选择
set.seed(1234) #set.seed(1234) #设置随机种子，使随机森林的随机性确定，否则会导致每次运行结果不同。
fs_rf <- rfe(x = traindata[, -7], # 剔除训练集的因变量
             y = traindata[, 7], # 提取因变量
             sizes = seq(3, 8, 1), # 选取的最少的特征是3 个，最多是8 个
             rfeControl = rfeControls_rf) # 加入我们构建的控制参数



# wait... ...

fs_rf # 展示特征选择的结果
new_vars <- fs_rf$optVariables[1:6] # 虽然显示选取9 个变量，但是根据实际情况，选择6 个
mydata <- hr_model[c(new_vars, 'left')] # 提取原数据中特征选择后的变量作为新数据
traindata2 <- mydata[index, ] # 建立新的训练集
testdata2 <- mydata[-index, ] # 建立新的测试集
# 使用train 函数对新训练集使用5 折交叉验证的方法建立随机森林模型
rfmodel <- train(left ~., data = traindata2, trControl = train_control, method = 'rf')


pred_rf <- predict(rfmodel, testdata2[-7])
con_rf <- confusionMatrix(pred_rf, testdata2$left, positive = '1')
con_rf

e1071model <- train(left ~ ., data = traindata,
                    trControl = train_control, method = 'nb')
pred_nb <- predict(e1071model, testdata[-7])
con_nb <- confusionMatrix(pred_nb, testdata$left, positive = '1')
con_nb


exit#构建rfe 函数的控制参数(使用朴素贝叶斯函数和5 交叉验证进行后续的特征选择)
rfeControls_nb <- rfeControl(
  functions = nbFuncs,
  method = 'cv',
  number = 5)
#使用rfe 函数（封装法）进行特征选择
fs_nb <- rfe(x = traindata[, -7],
             y = traindata[, 7],
             sizes = seq(3, 8, 1),
             rfeControl = rfeControls_nb)
fs_nb
new_vars <- fs_nb$optVariables
mydata <- hr_model[c(new_vars, 'left')]
traindata3 <- mydata[index, ]
testdata3 <- mydata[-index, ]
nbmodel <- train(left ~., data = traindata3, trControl = train_control, method = 'nb')
pred_nb2 <- predict(nbmodel, testdata3[-7])
con_nb2 <- confusionMatrix(pred_nb2, testdata3$left, positive = '1')
con_nb2

gmlmodel <- train(left ~ ., data = traindata,
                  trControl = train_control, method='LogitBoost')
pred_log <- predict(gmlmodel, testdata[-7])
con_log <- confusionMatrix(pred_log, testdata$left, positive = '1')
con_log

# 使用roc 函数时，预测的值必须是数值型
pred_rpart <- as.numeric(pred_rpart)
pred_rf <- as.numeric(pred_rf)
pred_nb <- as.numeric(pred_nb)
pred_nb2 <- as.numeric(pred_nb2)
pred_log <- as.numeric(pred_log)
roc_rpart <- roc(testdata$left, pred_rpart) # 获取后续画图时使用的信息
Specificity <- roc_rpart$specificities # 为后续的横纵坐标轴奠基
Sensitivity <- roc_rpart$sensitivities # 为后续的横纵坐标轴奠基
# 绘制ROC 曲线
p_rpart <- ggplot(data = NULL, aes(x = 1- Specificity, y = Sensitivity)) +
  geom_line(colour = 'red') + # 绘制ROC 曲线
  geom_abline() + # 绘制对角线
  annotate('text', x = 0.4, y = 0.5, label = paste('AUC=',
                                                   round(roc_rpart$auc, 3))) + theme_bw() + # 在图中（0.4,0.5）处添加AUC 值
  # 在图中右下角坐标（0.95,0.05）添加算法名称
  annotate('text', x = 0.95, y = 0.05, label = 'rpart', size = 8) +
  labs(x = '1 - Specificity', y = 'Sensitivities') # 设置横纵坐标轴标签
p_rpart
roc_rf <- roc(testdata$left, pred_rf)
Specificity <- roc_rf$specificities
Sensitivity <- roc_rf$sensitivities
p_rf <- ggplot(data = NULL, aes(x = 1- Specificity, y = Sensitivity)) +
  geom_line(colour = 'red') + geom_abline() +
  annotate('text', x = 0.4, y = 0.5, label = paste('AUC=',
                                                   round(roc_rf$auc, 3))) + theme_bw() +
  annotate('text', x = 0.95, y = 0.05, label = 'rf', size = 8) +
  labs(x = '1 - Specificity', y = 'Sensitivities')
p_rf
roc_nb <- roc(testdata$left, pred_nb)
Specificity <- roc_nb$specificities
Sensitivity <- roc_nb$sensitivities
p_nb <- ggplot(data = NULL, aes(x = 1- Specificity, y = Sensitivity)) +
  geom_line(colour = 'red') + geom_abline() +
  annotate('text', x = 0.4, y = 0.5, label = paste('AUC=',
                                                   round(roc_nb$auc, 3))) + theme_bw() +
  annotate('text', x = 0.95, y = 0.05, label = 'nb', size = 8) +
  
  labs(x = '1 - Specificity', y = 'Sensitivities')
p_nb
roc_nb2 <- roc(testdata$left, pred_nb2)
Specificity <- roc_nb2$specificities
Sensitivity <- roc_nb2$sensitivities
p_nb2 <- ggplot(data = NULL, aes(x = 1- Specificity, y = Sensitivity)) +
  geom_line(colour = 'red') + geom_abline() +
  annotate('text', x = 0.4, y = 0.5, label = paste('AUC=',
                                                   round(roc_nb2$auc, 2))) + theme_bw() +
  annotate('text', x = 0.95, y = 0.05, label = 'nb2', size = 8) +
  labs(x = '1 - Specificity', y = 'Sensitivities')
p_nb2
roc_log <- roc(testdata$left, pred_log)
Specificity <- roc_log$specificities
Sensitivity <- roc_log$sensitivities
p_log <- ggplot(data = NULL, aes(x = 1- Specificity, y = Sensitivity)) +
  geom_line(colour = 'red') + geom_abline() +
  annotate('text', x = 0.4, y = 0.5, label = paste('AUC=',
                                                   round(roc_log$auc, 2))) + theme_bw() +
  annotate('text', x = 0.9, y = 0.05, label = 'logistic', size = 8) +
  labs(x = '1 - Specificity', y = 'Sensitivities')
p_log
# 合并四个ROC 曲线图，排版为两行两列
multiplot(p_rpart, p_nb, p_nb2, p_log, cols = 2)

# 使用随机森林模型预测分类的概率，type=‘prob’设置预测结果为离职的概率和不离职的概率
pred_end <- predict(rfmodel, testdata2[-7], type = 'prob')

# 合并预测结果和预测概率结果
data_end <- cbind(pred_end, pred_rf)
# 为预测结果表重命名
names(data_end) <- c('pred.0', 'pred.1', 'pred')
# 生成一个交互式数据表
datatable(data_end)





