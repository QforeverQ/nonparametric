###体温
library(tseries)#游程检验
library(nortest)#正态性检验包
library(fBasics)#两样本ks检验包
library(MASS)#回归

#导入8月数据，序号，上午，下午，月，日，学生
temp8=read.csv("E:/2021秋季学期/非参数统计/体温/每日体温监测-上午下午8月.csv",header=TRUE)
head(temp8)
temp1<-as.data.frame(temp8)
attach(temp8)
summary(temp8)

#导入9月数据，序号，上午，下午，月，日，学生
temp9=read.csv("E:/2021秋季学期/非参数统计/体温/每日体温监测-上午下午9月.csv",header=TRUE)
head(temp9)
temp1<-as.data.frame(temp9)
attach(temp9)
summary(temp9)

#导入10月数据，序号，上午，下午，月，日，学生
temp10=read.csv("E:/2021秋季学期/非参数统计/体温/每日体温监测-上午下午10月.csv",header=TRUE)
head(temp10)
temp1<-as.data.frame(temp10)
attach(temp10)
summary(temp10)

#总数据上午下午
temp_mor=c(temp8$上午,(na.omit(temp9))$上午,(na.omit(temp10))$上午)
temp_aft=c(temp8$下午,(na.omit(temp9))$下午,(na.omit(temp10))$下午)

#原始数据横板
temph=read.csv("E:/2021秋季学期/非参数统计/体温/体温总表横版.csv",header=TRUE)
head(temph)
temph<-as.data.frame(temph)
attach(temph)
summary(temph)
#原始数据竖板
temps=read.csv("E:/2021秋季学期/非参数统计/体温/体温总表竖版.csv",header=TRUE)
head(temps)
temps<-as.data.frame(temps)
attach(temps)
summary(temps)

#上午和下午体温数据相关性
cor.test(temp8$上午,temp8$下午,meth="spearman")
cor.test(temp8$上午,temp8$下午,meth="kendall")
cor.test((na.omit(temp9))$上午,(na.omit(temp9))$下午,meth="spearman")
cor.test((na.omit(temp9))$上午,(na.omit(temp9))$下午,meth="kendall")
cor.test((na.omit(temp10))$上午,(na.omit(temp10))$下午,meth="spearman")
cor.test((na.omit(temp10))$上午,(na.omit(temp10))$下午,meth="kendall")
cor.test(temp_mor,temp_aft,meth="spearman")
cor.test(temp_mor,temp_aft,meth="kendall")

#正态性检验
par(mfrow=c(1,2))
hist(temp_mor,freq=T,xlab="体温",ylab="数量",main="上午体温数据频数分布直方图")
qqnorm(temp_mor);qqline(temp_mor);
ad.test(temp_mor);cvm.test(temp_mor);pearson.test(temp_mor);sf.test(temp_mor);shapiro.test(temp_mor)
hist(temp_aft,freq=T,xlab="体温",ylab="数量",main="下午体温数据频数分布直方图")
qqnorm(temp_aft);qqline(temp_aft);
ad.test(temp_aft);cvm.test(temp_aft);pearson.test(temp_aft);sf.test(temp_aft);shapiro.test(temp_aft)

#回归
x=temp_mor;
y=temp_aft;
summary(lm(y~x))
lqs(y~x,method="lms")
lqs(y~x,method="lts"))
lqs(y~x,method="S")

#ks检验
ks.test(temp_mor,temp_aft)#上午和下午的温度分布有差异

#两样本位置参数检验：上午和下午 下午体温中位数高
#合并
#Brown-Mood
z=cbind(c(temp_mor,temp_aft),c(rep(1,3893),rep(2,3893)))
k=unique(z[,2]);m=median(z[,1]);m1=NULL;m2=NULL
for(i in k){m1=c(m1,sum(z[z[,2]==i,1]>m));m2=c(m2,sum(z[z[,2]==i,1]<=m))}
C=rbind(m1,m2)
fisher.test(C,alt="less")#0.1001 
#Wilcoxon
wilcox.test(temp_mor,temp_aft,alt="less")#0.001352
#正态记分检验
x=temp_mor; y=temp_aft;
m=length(x);n=length(y);
w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
T=sum(w[w[,2]==1,4])
w2=sum(w[,4]^2)
S=sqrt(m*n*w2/(m+n-1)/(m+n))
Z=T/S
p=pnorm(Z)
p#1.28535e-66
#成对数据检验
wilcox.test(temp_mor,temp_aft,paired=T,alt="less")

#区组friedman检验
#横着是不同学生，竖着是上下午
d=matrix(0,2,46)
k=1
for(i in 3:48)
{ n1=0;n2=0
  sum1=0;sum2=0
  for(j in seq(1,174,2))
  { if(is.na(temps[,i][j])==FALSE){sum1=sum1+temps[,i][j];n1=n1+1;}
    if(is.na(temps[,i][j+1])==FALSE){sum2=sum2+temps[,i][j+1];n2=n2+1;}
  }
  d[1,k]=sum1/n1;d[2,k]=sum2/n2;k=k+1;
}
d
write(as.vector(t(d)),file="E:/2021秋季学期/非参数统计/体温/完全区组设计矩阵.csv")
friedman.test(d)#0.0002681 上下午是有差异的
#kendall
R=apply(d,2,sum)
b=nrow(d);k=ncol(d)
S=sum((R-b*(k+1)/2)^2)
W=12*S/b^2/(k^3-k)
pchisq(b*(k-1)*W,k-1,low=F)# 0.0003170306


#两样本尺度参数检验：上午和下午 下午体温波动大
#平方秩
x=temp_mor; y=temp_aft;
m=length(x);n=length(y);
x1=abs(x-mean(x));y1=abs(y-mean(y));
xy1=c(x1,y1);xy0=c(x,y);xyi=c(rep(1,m),rep(2,n));
xy=cbind(xy1,xy0,xyi)
xy2=cbind(xy[order(xy[,1]),],1:(m+n),(1:(m+n))^2)
T1=sum(xy2[xy2[,3]==1,5]);T2=sum(xy2[xy2[,3]==2,5])
R=xy2[,5];meanR=mean(R);
S=sqrt(m*n*(sum(R^2)-(m+n)*meanR^2)/(m+n)/(m+n-1))
Zx=(T1-m*meanR)/S;Zy=(T2-n*meanR)/S
p=min(pnorm(Zx),pnorm(Zy))
p #8.049131e-09 小于

#Siegel-Tukey
z=cbind(c(temp_mor,temp_aft),c(rep(1,3893),rep(2,3893)))
x=temp_mor; y=temp_aft;
x1=x-median(outer(x,y,"-"))
xy=cbind(c(x1,y),c(rep(1,length(x)),rep(2,length(y))))
xy1=xy[order(xy[,1]),]
z=xy[,1];n=length(z);a1=2:3;b=2:3
for(i in seq(1,n,2)){ b=b+4;a1=c(a1,b) }
a2=c(1,a1+2);z=NULL
for(i in 1:n) z=c(z,(i-floor(i/2)))
b=1:2
for(i in seq(1,(n+2-2),2)) 
if(z[i]/2!=floor(z[i]/2)) {z[i:(i+1)]=b;b=b+2}
zz=cbind(c(0,0,z[1:(n-2)]),z[1:n])
if(n==1) R=1;
if(n==2) R=c(1,2);
if(n>2) R=c(a2[1:zz[n,1]],rev(a1[1:zz[n,2]]))
xy2=cbind(xy1,R)
Wx=sum(xy2[xy2[,2]==1,3])
Wy=sum(xy2[xy2[,2]==2,3])
nx=length(x);ny=length(y)
Wxy=Wy-0.5*ny*(ny+1);Wyx=Wx-0.5*nx*(nx+1)
pvalue=pwilcox(Wyx,nx,ny)
pvalue #0.01534668

#Mood
z=cbind(c(temp_mor,temp_aft),c(rep(1,3893),rep(2,3893)))
x=temp_mor; y=temp_aft;
m=length(x);n=length(y)
x1=x-median(outer(x,y,"-"))
xy=cbind(c(x1,y),c(rep(1,length(x)),rep(2,length(y))))
N=nrow(xy);xy1=cbind(xy[order(xy[,1]),],1:N)
R1=xy1[xy1[,2]==1,3];M=sum((R1-(N+1)/2)^2)
E1=m*(N^2-1)/12;s=sqrt(m*n*(N+1)*(N^2-4)/180)
Z=(M-E1)/s
pvalue=pnorm(Z)
pvalue # 0.07396175 小于

#ansari-bradley 不好
x=temp_mor; y=temp_aft;
ansari.test(x,y,alt="greater")
#0.4633

#fligner-killeen
fligner.test(c(temp_mor,temp_aft),c(rep(1,3893),rep(2,3893)))
#0.001748


#游程检验，零假设序列是随机的
test=sign(temps$X4-median(temps$X4))
test1=test;test2=test;test1[test1==0]=1;test2[test2==0]=-1
runs.test(factor(test1))#0.7055
runs.test(factor(test2))#0.1464
test=sign(temps$X11-median(temps$X11))#小于
test1=test;test2=test;test1[test1==0]=1;test2[test2==0]=-1
runs.test(factor(test1))#6.443e-07
runs.test(factor(test2))#不是二分了，只有36.4和36.5
test=sign(temps$X20-median(temps$X20))#8月大多大于等于中位数，10月大多小于等于中位数
test1=test;test2=test;test1[test1==0]=1;test2[test2==0]=-1
runs.test(factor(test1))#5.213e-15
runs.test(factor(test2))#1.662e-08
test=sign(temps$X25-median(temps$X25))
test1=test;test2=test;test1[test1==0]=1;test2[test2==0]=-1
runs.test(factor(test1))#0.01899
runs.test(factor(test2))#5.861e-06
test=sign(temps$X28-median(temps$X28))
test1=test;test2=test;test1[test1==0]=1;test2[test2==0]=-1
runs.test(factor(test1))#0.02073
runs.test(factor(test2))#0.2892
test=sign(temps$X42-median(temps$X42))
test1=test;test2=test;test1[test1==0]=1;test2[test2==0]=-1
runs.test(factor(test1))#0.3087
runs.test(factor(test2))#0.03622
test=sign(temps$X44-median(temps$X44))#8月大多小于等于中位数，10月大多大于等于中位数
test1=test;test2=test;test1[test1==0]=1;test2[test2==0]=-1
runs.test(factor(test1))#0.001965
runs.test(factor(test2))#5.569e-08
test=sign(temps$X46-median(temps$X46))#上午低，下午高
test1=test;test2=test;test1[test1==0]=1;test2[test2==0]=-1
runs.test(factor(test1))#7.926e-10
runs.test(factor(test2))#1.192e-14

#cox-stuart趋势检验
D=temps$X4[1:87]-temps$X4[88:174]
K=min(sum(sign(D)==1),sum(sign(D)==-1))
pbinom(K,sum(sign(D)==1)+sum(sign(D)==-1),0.5)#0.4478416 不拒绝 无趋势
D=temps$X11[1:87]-temps$X11[88:174]
K=min(sum(sign(D)==1),sum(sign(D)==-1))
pbinom(K,sum(sign(D)==1)+sum(sign(D)==-1),0.5)#0.04007166 增加
D=temps$X20[1:87]-temps$X20[88:174]
K=min(sum(sign(D)==1),sum(sign(D)==-1))
pbinom(K,sum(sign(D)==1)+sum(sign(D)==-1),0.5)#1.511817e-09 减少
D=temps$X25[1:87]-temps$X25[88:174]
K=min(sum(sign(D)==1),sum(sign(D)==-1))
pbinom(K,sum(sign(D)==1)+sum(sign(D)==-1),0.5)#0.5504618 不拒绝 无趋势
D=temps$X28[1:87]-temps$X28[88:174]
K=min(sum(sign(D)==1),sum(sign(D)==-1))
pbinom(K,sum(sign(D)==1)+sum(sign(D)==-1),0.5)#5.068224e-08 增加
D=temps$X42[1:87]-temps$X42[88:174]
K=min(sum(sign(D)==1),sum(sign(D)==-1))
pbinom(K,sum(sign(D)==1)+sum(sign(D)==-1),0.5)#0.1409895 不拒绝 无趋势
D=temps$X44[1:87]-temps$X44[88:174]
K=min(sum(sign(D)==1),sum(sign(D)==-1))
pbinom(K,sum(sign(D)==1)+sum(sign(D)==-1),0.5)#0.07401609 有趋势 增加
D=temps$X46[1:87]-temps$X46[88:174]
K=min(sum(sign(D)==1),sum(sign(D)==-1))
pbinom(K,sum(sign(D)==1)+sum(sign(D)==-1),0.5)#0.1840938 不拒绝 无趋势



