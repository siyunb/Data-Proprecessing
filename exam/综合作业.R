library(ggplot2)
library(AppliedPredictiveModeling)
library(ggplot2)
library(dummies)
library(lars)
library(glmnet)
library(grid)
library(GGally)
library(MASS)


vp <- function(x, y) {
  viewport(layout.pos.row = x, layout.pos.col = y)
}
# 二，数据预处理
# 1，鲍鱼数据
# (1)
mytheme =theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="gold3"),panel.background=element_rect(fill='aliceblue')) 
p1=ggplot(abalone,aes(x=LongestShell,y=Rings))+geom_line()+mytheme
p2=ggplot(abalone,aes(x=Diameter,y=Rings))+geom_line()+mytheme
p3=ggplot(abalone,aes(x=Height,y=Rings))+geom_line()+mytheme
p4=ggplot(abalone,aes(x=WholeWeight,y=Rings))+geom_line()+mytheme
p5=ggplot(abalone,aes(x=ShuckedWeight,y=Rings))+geom_line()+mytheme
p6=ggplot(abalone,aes(x=VisceraWeight,y=Rings))+geom_line()+mytheme
p7=ggplot(abalone,aes(x=ShellWeight,y=Rings))+geom_line()+mytheme
p8=ggplot(abalone,aes(x=factor(Type),y=Rings,fill = I("lightblue")))+geom_boxplot()+xlab("鲍鱼种类")+ylab("数量")+mytheme
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 2)))
print(p1,vp = vp(1,1))
print(p2,vp = vp(1,2))
print(p3,vp = vp(2,1))
print(p4,vp = vp(2,2))
print(p5,vp = vp(3,1))
print(p6,vp = vp(3,2))
print(p7,vp = vp(4,1))
print(p8,vp = vp(4,2))

# (2)变量相关性
ggpairs(abalone[,setdiff(names(abalone),c('Rings','Type'))])
library(corrplot)
cormatrix=cor(abalone[,setdiff(names(abalone),c('Rings','Type'))])
corrplot(cormatrix,method = "shade")


# (3)重要性得分并筛选变量
importance_score=c()
count_num=1
for (i in setdiff(names(abalone),c('Rings','Type'))) {
  importance_score[count_num]=cor(abalone$Rings,abalone[,i])
  count_num=count_num+1
}
onehot_data=dummy.data.frame(abalone,names=c('Type'),sep="_")
x=as.matrix(onehot_data[,setdiff(names(onehot_data),c('Rings'))])
y=as.matrix(onehot_data[,c('Rings')])

##lasso选择变量
model_fit= lars(x,y)
# 10折交叉验证选择模型
model_cv=cv.lars(x,y,K=10)
select_model=model_cv$index[which.min(model_cv$cv)]
my_coef=coef.lars(model_fit,mode='fraction',s=select_model)
# 使用cp选择模型，可以发现需要剔除构造的TYPE_F和LongestShell变量
cp_select=which.min(model_fit$Cp)
my_coef_cp=coef.lars(model_fit,mode='step',s=cp_select)
# glmnet包实现
cv.out=cv.glmnet(x,y,alpha = 1)
plot(cv.out)
bestlam=cv.out$lambda.min
fit_model = glmnet(x, y, alpha = 1, nlambda = 40)
lasso_coefficient=predict(fit_model,type = "coefficients",s = bestlam)

# (4)主成分
pca_fit=princomp(abalone[,setdiff(names(abalone),c('Rings','Type'))],cor=T)
# 第一个主成分解释方差的比例已经达到了90%
summary(pca_fit)
# 根据碎石图发现选取第一个变量就足以代表原始数据了
screeplot(pca_fit,type="lines",main="碎石图",col="blue")

# 2，模拟数据
# (1) 模拟数据
generate_data=function(amount,from,to,norm_mean,norm_std) {
  x1=runif(500,from,to)
  x2=runif(500,from,to)
  x3=runif(500,from,to)
  x4=runif(500,from,to)
  x5=runif(500,from,to)
  # x_matrix=data.frame(x1,x2,x3,x4,x5)
  y=10*sin(pi*x1*x2)+20*(x3-0.5)^2+10*x4+5*x5+rnorm(500,norm_mean,norm_std)
  result=data.frame(x1,x2,x3,x4,x5,y)
  return(result)
}

# (2) 关系探索
result=generate_data(500,0,2,0,1)
p1=ggplot(result,aes(x=x1,y=y))+geom_line()
p2=ggplot(result,aes(x=x2,y=y))+geom_line()
p3=ggplot(result,aes(x=x3,y=y))+geom_line()
p4=ggplot(result,aes(x=x4,y=y))+geom_line()
p5=ggplot(result,aes(x=x5,y=y))+geom_line()
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(p1,vp = vp(1,1))
print(p2,vp = vp(1,2))
print(p3,vp = vp(2,1))
print(p4,vp = vp(2,2))
print(p5,vp = vp(3,1))

ggpairs(result)

cormatrix=cor(result)
corrplot(cormatrix,method = "shade")
# (3) 选择变量
select_lm=lm(y~x1+x2+x3+x4+x5,data=result)
lm_for=stepAIC(select_lm,direction="forward")
# 全部选择
summary(lm_for)
lm_back=stepAIC(select_lm,direction="backward")
# 选择x1,x2,x3,x4
summary(lm_back)
lm_both=stepAIC(select_lm,direction="both")
# 选择x1,x2,x3,x4
summary(lm_both)

# (4) 过滤法
# 删除方差几乎为0的变量,本文各个变量服从相同的分布，所以方差都近似相同
var_var=c()
for (i in 1:5) {
  var_var[i]=var(result[,i])
}

# 去除强相关变量
cormatrix=cor(result[,setdiff(names(result),c('y'))])
# 设置阈值为0.2，没有强相关性
cormatrix>0.2

#变量聚类 
library(Hmisc)
tmp=varclus(as.matrix(result[,setdiff(names(result),c('y'))]))
plot(tmp)

#relieff选择
library(caret)
library(CORElearn)
reliefValues=attrEval(y ~ .,data = lm_Data,estimator = "ReliefFequalK",ReliefIterations = 50)
reliefValues1=attrEval(y ~ .,data = lm_Data,estimator = "ReliefFexpRank",ReliefIterations = 50)
#也可以看出x1,x2有相同的倾向性，由于重要性相近，很难进行选择。
reliefValues
reliefValues1

#非过滤法，loess选择,但可以看出x1,x2基本由相同的倾向性
loessresult=filterVarImp(x=lm_Data[,-6],y=lm_Data[,6],nonpara=TRUE)
loessresult
loessresult=loessresult$Overall[loessresult$Overall>=0.05]
loessresult

#mic选择
library(minerva)
micValues1 = mine(lm_Data[, -6],lm_Data[,6])
data3=as.data.frame(micValues1$MIC)
micValues1$MIC
# 阈值大于2
data3=data3$Y[data3$Y>=0.2]
data3


