###4、马尔代夫
library(nortest)#正态性检验包
library(fBasics)#两样本ks检验包
library(RColorBrewer)#颜色数据集


#导入数据集，初始数据集
ma1=read.csv("E:/2021秋季学期/非参数统计/马尔代夫/马尔代夫旅游.csv",header=TRUE)
head(ma1)
game1<-as.data.frame(ma1)
attach(ma1)
summary(ma1)

#导入数据集，只有三大店铺的数据集
ma2=read.csv("E:/2021秋季学期/非参数统计/马尔代夫/马尔代夫店铺.csv",header=TRUE)
head(ma2)
game1<-as.data.frame(ma2)
attach(ma2)
summary(ma2)
#三大店铺数据
ma_dy=ma2$价格[1:157]
ma_gl=ma2$价格[158:265]
ma_xc=ma2$价格[266:490]


#导入数据集，所有行程数据集
ma3=read.csv("E:/2021秋季学期/非参数统计/马尔代夫/马尔代夫所有行程+价格.csv",header=TRUE)
head(ma3)
game1<-as.data.frame(ma3)
attach(ma3)
summary(ma3)
#各行程数据
ma54=na.omit(ma3$价格[1:40])
ma64=na.omit(ma3$价格[41:258])
ma65=na.omit(ma3$价格[259:261])
ma75=na.omit(ma3$价格[262:777])
ma86=na.omit(ma3$价格[778:782])
ma97=na.omit(ma3$价格[783:785])
ma108=na.omit(ma3$价格[786:790])

#导入数据集，所有月份
ma4=read.csv("E:/2021秋季学期/非参数统计/马尔代夫/马尔代夫月份.csv",header=TRUE)
head(ma4)
game1<-as.data.frame(ma4)
attach(ma4)
summary(ma4)

#正态性检验
par(mfrow=c(1,2))
#总价格，不满足正态假设
hist(na.omit(ma1$价格),freq=T,xlab="马尔代夫旅游路线价格",ylab="数量",main="价格频数分布直方图")
qqnorm(na.omit(ma1$价格));qqline(na.omit(ma1$价格));
ad.test(na.omit(ma1$价格));cvm.test(na.omit(ma1$价格));pearson.test(na.omit(ma1$价格));
sf.test(na.omit(ma1$价格));shapiro.test(na.omit(ma1$价格))

#各行程之间有无显著差异：5天4晚价格低于其他行程，其他行程之间分布相同
#ks检验
h=c(0,40,258,261,777,782,785,790)
K=matrix(0,7,7)
W=matrix(0,7,7)
for (i in 1:6)
{ for (j in (i+1):7)
  { x=na.omit(ma3$价格[(h[i]+1):h[i+1]]);y=na.omit(ma3$价格[(h[j]+1):h[j+1]])
    K[i,j]=ks.test(x,y)$p.value
    #对于KS检验未通过的继续进行秩和检验
    if (K[i,j]<0.05)
    { w1=wilcox.test(x,y,alt="less")$p.value
      w2=wilcox.test(x,y,alt="greater")$p.value
      if (w1<w2){ W[i,j]=w1*(-1)}
      else { W[i,j]=w2}
    }
  }
}
K
W
#除了5天4晚之外，其他的行程价格分布无显著差异


#三大店铺价格有无显著差异
ks.test(ma_dy,ma_gl)
ks.test(ma_dy,ma_xc)#有差异
ks.test(ma_gl,ma_xc)
#Brown-Mood
z=cbind(c(na.omit(ma_dy),na.omit(ma_xc)),
        c(rep(1,length(na.omit(ma_dy))),rep(2,length(na.omit(ma_xc)))))
k=unique(z[,2]);m=median(z[,1]);m1=NULL;m2=NULL
for(i in k){m1=c(m1,sum(z[z[,2]==i,1]>m));m2=c(m2,sum(z[z[,2]==i,1]<=m))}
C=rbind(m1,m2)
fisher.test(C)#0.4555 
#Wilcoxon
wilcox.test(na.omit(ma_dy),na.omit(ma_xc))#0.9741
#正态记分检验
x=na.omit(ma_dy); y=na.omit(ma_xc);
m=length(x);n=length(y);
w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
T=sum(w[w[,2]==1,4])
w2=sum(w[,4]^2)
S=sqrt(m*n*w2/(m+n-1)/(m+n))
Z=T/S
p=pnorm(Z)
p#0.5914834

#尺度参数检验
#平方秩
x=na.omit(ma_dy); y=na.omit(ma_xc);
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
p #5.619099e-06 小于

#Mood
x=na.omit(ma_dy); y=na.omit(ma_xc);
z=cbind(c(x,y),c(rep(1,length(x)),rep(2,length(y))))
m=length(x);n=length(y)
x1=x-median(outer(x,y,"-"))
xy=cbind(c(x1,y),c(rep(1,length(x)),rep(2,length(y))))
N=nrow(xy);xy1=cbind(xy[order(xy[,1]),],1:N)
R1=xy1[xy1[,2]==1,3];M=sum((R1-(N+1)/2)^2)
E1=m*(N^2-1)/12;s=sqrt(m*n*(N+1)*(N^2-4)/180)
Z=(M-E1)/s
pvalue=pnorm(Z)
pvalue # 4.082611e-05 小于

ansari.test(x,y,alt="less")#0.0001215

#fligner-killeen
fligner.test(c(x,y),c(rep(1,length(x)),rep(2,length(y))))#0.00050275.
#第壹和携程的价格分布有差异并且差异不在于位置在于尺度，携程的价格波动大
#箱线图
data=as.data.frame(cbind(na.omit(ma_dy),na.omit(ma_xc)))
boxplot(data,names=c("第壹假期旅行网","携程自由行"),
        col=c("blue","blue3"),main="两家店铺马尔代夫线路价格")

#每月价格有无显著差异
ma_11=ma4$价格[1:54]
ma_12=ma4$价格[55:603]
ma_1=ma4$价格[604:724]
ma_2=ma4$价格[725:738]
ma_3=ma4$价格[739:789]
ma_4=ma4$价格[790:790]
median(na.omit(ma_11));median(na.omit(ma_12));median(na.omit(ma_1))
median(na.omit(ma_2));median(na.omit(ma_3));ma_4
mean(na.omit(ma_11));mean(na.omit(ma_12));mean(na.omit(ma_1))
mean(na.omit(ma_2));mean(na.omit(ma_3));ma_4

#各月价格差异
data_kw1=c(ma_11,ma_12,ma_1,ma_2,ma_3)
mydata_kw1<-data.frame(data_kw1,group1=c(rep(1,54),rep(2,549),rep(3,121),
                      rep(4,14),rep(5,51)))
kruskal.test(data_kw1~group1,data=mydata_kw1)
#任意两月价格差异
#wilcoxon秩和检验
h=c(0,54,603,724,738,789)
wilcox_month=matrix(0,5,5)
for(i in 1:4)
{ for(j in (i+1):5)
  { wilcox_month1=wilcox.test(ma4$价格[(h[i]+1):h[i+1]],ma4$价格[(h[j]+1):h[j+1]],alt="less") 
    wilcox_month2=wilcox.test(ma4$价格[(h[i]+1):h[i+1]],ma4$价格[(h[j]+1):h[j+1]],alt="greater") 
    if(wilcox_month1$p.value<wilcox_month2$p.value) {wilcox_month[i,j]=wilcox_month1$p.value*(-1)}
    else {wilcox_month[i,j]=wilcox_month2$p.value}
  }
}
wilcox_month
wilcox_month_v=as.vector(t(wilcox_month))
write(wilcox_month_v,file="E:/2021秋季学期/非参数统计/马尔代夫/任意两月价格wilcoxon秩和检验.csv")
#正态记分检验
h=c(0,54,603,724,738,789)
l=c(54,549,121,14,51)
normal_month=matrix(0,5,5)
for(i in 1:4)
{ for(j in (i+1):5)
  { x=ma4$价格[(h[i]+1):h[i+1]]; y=ma4$价格[(h[j]+1):h[j+1]];
    m=l[i]; n=l[j];
    w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
    w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
    T=sum(w[w[,2]==1,4])
    w2=sum(w[,4]^2)
    S=sqrt(m*n*w2/(m+n-1)/(m+n))
    Z=T/S
    p=pnorm(Z)
    if(p>0.5){p=1-p}
    else {p=p*(-1)}
    normal_month[i,j]=p
  }
}
normal_month
normal_month_v=as.vector(t(normal_month))
write(normal_month_v,file="E:/2021秋季学期/非参数统计/马尔代夫/任意两月价格正态记分检验.csv")

#有无早餐对价格的影响
zaoyes=na.omit(ma4[ma4$早餐==1,]$价格)
zaono=na.omit(ma4[ma4$早餐==0,]$价格)
ks.test(zaoyes,zaono)#0.2582
#Brown-Mood
z=cbind(c(zaoyes,zaono),c(rep(1,length(zaoyes)),rep(2,length(zaono))))
k=unique(z[,2]);m=median(z[,1]);m1=NULL;m2=NULL
for(i in k){m1=c(m1,sum(z[z[,2]==i,1]>m));m2=c(m2,sum(z[z[,2]==i,1]<=m))}
C=rbind(m1,m2)
fisher.test(C,alt="less")#0.05896 
#Wilcoxon
wilcox.test(zaoyes,zaono,alt="less")#0.1073
#正态记分检验
x=zaoyes; y=zaono;
m=length(x);n=length(y);
w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
T=sum(w[w[,2]==1,4])
w2=sum(w[,4]^2)
S=sqrt(m*n*w2/(m+n-1)/(m+n))
Z=T/S
p=pnorm(Z)
p#0.1157632

#豪华酒店对价格的影响
zhuyes=na.omit(ma4[ma4$豪华住宿==1,]$价格)
zhuno=na.omit(ma4[ma4$豪华住宿==0,]$价格)
ks.test(zhuyes,zhuno)#5.323e-05
#Brown-Mood
z=cbind(c(zhuyes,zhuno),c(rep(1,length(zhuyes)),rep(2,length(zhuno))))
k=unique(z[,2]);m=median(z[,1]);m1=NULL;m2=NULL
for(i in k){m1=c(m1,sum(z[z[,2]==i,1]>m));m2=c(m2,sum(z[z[,2]==i,1]<=m))}
C=rbind(m1,m2)
fisher.test(C,alt="greater")#1.311e-05 
#Wilcoxon
wilcox.test(zhuyes,zhuno,alt="greater")#4.782e-05
#正态记分检验
x=zhuyes; y=zhuno;
m=length(x);n=length(y);
w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
T=sum(w[w[,2]==1,4])
w2=sum(w[,4]^2)
S=sqrt(m*n*w2/(m+n-1)/(m+n))
Z=T/S
p=pnorm(Z)
1-p#0.0002689024



