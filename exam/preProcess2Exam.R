##----utf-8----##
##数据预处理2EXAM##
#(1)写一个R函数从该模型中模拟数据。
DataGenerator <- function (n, sd = 1) #这个函数是R包里的源代码，直接copy过来的(稍作了修改)
{                                     #原始函数是mlbench包里的mlbench.friedman1
  x <- matrix(runif(5 * n), ncol = 5)
  y <- 10 * sin(pi * x[, 1] * x[, 2])
  y <- y + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
  if (sd > 0) {
    y <- y + rnorm(n, sd = sd)
  }
  list(x = x, y = y)
}

#(2)随机模拟一个数据集，样本量是500，绘制图形研究预测变量和被解释变量之间的关系。
#模拟数据集
set.seed(20171118)
data_500 = DataGenerator(500)#生成的是一个列表，保存了生成的x和y
x_data = data_500$x
y_data = data_500$y
x_data
dim(x_data)
y_data
lmData <- data.frame(cbind(y_data,x_data))
names(lmData)=c("Y","x1","x2","x3","x4","x5")
#绘图
op <- par(mfrow=c(2,3))
for(i in names(lmData)){
  x = lmData[,i]
  plot(x,lmData$Y)
  lines(lowess(x,lmData$Y),col="red",lwd=2,lty=2)
}
par(op)

#(3)使用线性回归中的向前法、向后法和逐步回归等变量选择方法，最终模型选择了哪些变量？
library(MASS)
initModel <- lm(Y~.,data = lmData)
summary(initModel)#在summary里可以看到v4的t检验不显著

stepAIC(initModel,direction = "forward")#向前
stepAIC(initModel,direction = "backward")#向后
stepAIC(initModel,direction = "both")#逐步选择
#以上三种方法都没有删除变量，即选择了所有变量

#(4)
library(ggplot2)
library(lattice)
library(caret)
#方法1，使用相关系数进行筛选
#定义计算相关系数的函数
calc_cor <- function(x,y){
  corValue = cor(x,y)
  corValue
}
#定义筛选相关系数的标准
#大于0.2的输出为TRUE
corSelection <- function(corValue,x,y){
  corSelected <- (corValue >0.2 & corValue <0.75 )
  corSelected
}
#lmSBF表示选择的是lm(线性)的SBF
corFunction <- lmSBF
corFunction$score <- calc_cor
corFunction$summary <- defaultSummary
corFunction$filter <- corSelection
corCtrl <- sbfControl(method = "repeatedcv",
                      repeats = 5,
                      verbose = TRUE,
                      functions = corFunction )
corFilter <- sbf(lm_Data[,2:6],
                 lm_Data$y,
                 tol = 1.0e-12,
                 sbfControl = corCtrl)
corFilter #结果显示，删除了变量x3

#方法2，使用MIC统计量

#定义计算MIC的函数
library(minerva)
calc_MIC <- function(x,y){
  MicValue = mine(x,y)
  MicValue$MIC
}

#定义筛选MIC的标准
micSelection <- function(MicValue,x,y){
  micSelected <- (MicValue > 0.2)
  micSelected
}

#lmSBF表示选择的是lm(线性)的SBF
micFunction <- lmSBF
#将上面定义的函数传入到筛选器里面去
micFunction$score <- calc_MIC
micFunction$summary <- defaultSummary
micFunction$filter <- micSelection
micCtrl <- sbfControl(method = "repeatedcv",
                      repeats = 5,
                      verbose = TRUE,
                      functions = micFunction )
micFilter <- sbf(lm_Data[,2:6],
                 lm_Data$y,
                 tol = 1.0e-12,
                 sbfControl = micCtrl)
micFilter #

#x1和x2都被选中了,二者之间倾向性不明显




##########################################
#下面是没有做出来的Relief算法部分
##########################################
#方法3，使用ReliefF算法
#定义计算ReliefF统计量的函数
library(AppliedPredictiveModeling)
# calc_Relief <- function(x,y){
#   ReliefValues = permuteRelief(x,
#                                y,
#                                nperm = 20,
#                                estimator = "InfGain",
#                                ReliefIterations = 50)
#   ReliefValues$permutations
# }
perm <- permuteRelief(x = lmData[,2:6],
                      y = lmData$Y,
                      nperm = 10,
                      estimator = "ReliefFequalK",
                      ReliefIterations = 20)
perm$permutations
# micvalue <- apply(lmData[,2:6],
#                   MARGIN = 2,
#                   FUN = calc_Relief,
#                   lmData$Y)
