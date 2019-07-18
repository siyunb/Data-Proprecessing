##数据预处理考试第一题##
library(AppliedPredictiveModeling)
data(abalone)
head(abalone)
str(abalone)
summary(abalone)
#因变量是Rings，是int类型，属于数值因变量
#其他变量都是解释变量

####(1)对数据作图估计预测变量和被解释变量之间的函数关系。
namesMinusRings = names(abalone)[-9]
op <- par(no.readonly=TRUE)
par(mfrow=c(3,3))
for(i in namesMinusRings){
  x = abalone[,i]
  plot(x,abalone$Rings)
  if(i!="Type"){      #除了Type其他都拟合
  lines(lowess(x,abalone$Rings),col="blue",lwd=2,lty=2)
  }
}
par(op)

####(2)用散点图和相关系数图解释预测变量之间的相关性。

library(car)
library(corrplot)
names(abalone)
#这个图散点图矩阵作图时间非常久
scatterplotMatrix(~Type+LongestShell+Diameter+Height+WholeWeight+
                    ShuckedWeight+VisceraWeight+ShellWeight,data=abalone)
corValue <- cor(abalone[,-c(1,9)])
corrplot(corValue)#变量间相关性很高

####(3)对预测变量估计重要性得分。找到一种筛选方法得到预测变量子集，该集合不含冗余变量。

#首先
#过滤近零方差变量
libarry(caret)
numericCol <- abalone[,2:8]
nearZeroVar(numericCol)#返回inter0表示没有方差很小的变量，没有起到筛选效果

#筛选数值型预测变量
library(ggplot2)
library(lattice)
library(caret)
str(abalone)

#计算数值型变量得分的方法1
#filterVarImp可以建立LOESS模型，并定量分析变量和结果变量之间的关系
loessResults <- filterVarImp(x = abalone[,2:8],#2：8为数值型预测变量
                             y = abalone[,9],#9为结果变量
                             nonpara = TRUE)#nonpara参数表示是非参数回归
loessResults

#方法2
#尝试使用MIC(最大信息数)来筛选预测变量
library(minerva)
micValues <- mine(abalone[,2:8],abalone[,9])#计算结果包含很多个统计量
names(micValues)
micValues$MIC

#方法3，计算各预测变量和结果变量的相关系数
corrValues <- apply(abalone[,2:8],
                    MARGIN = 2,
                    FUN = function(x,y)cor(x,y),
                    y = abalone$Rings)
corrValues

#筛选分类预测变量
#三个水平，使用方差分析
str(abalone$Type)
aovResult <- aov(abalone$Rings~abalone$Type)
summary(aovResult)#结果显示不同Type之间存在显著差异

####(4)对连续型预测变量应用主成分分析，决定多少个不相关的主成分能够代表数据中的信息？

#进行主成分分析，并选择主成分
pcaObject <- prcomp(numericCol,
                    center = TRUE, scale = TRUE)#对变量进行中心化和标准化
percentVariance <- pcaObject$sdev^2/sum(pcaObject$sdev^2)*100#计算方差贡献率
percentVariance #展示方差贡献率,一个主成分即能贡献90%以上的方差
head(pcaObject$x[,1:5])#展示pcaObject的子对象x：主成分得分
