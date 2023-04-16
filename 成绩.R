###2、成绩
library(nortest)#正态性检验包
library(scales)#show_col的包
library(nortest)#正态性检验包
library(RColorBrewer)#颜色数据集
colors()
brewer.pal.info
display.brewer.all()
library(psych)#碎石图
library(MASS)#回归
library(mblm)#回归

#导入数据，原始数据表
scores=read.csv("E:/2021秋季学期/非参数统计/成绩/考试成绩.csv",header=TRUE)
head(scores)
scores<-as.data.frame(scores)
attach(scores)
summary(scores)

#各题得分率表
rates=read.csv("E:/2021秋季学期/非参数统计/成绩/各题得分率.csv",header=TRUE)
head(rates)
rates<-as.data.frame(rates)
attach(rates)
summary(rates)

#得到各专业数据集
scores_gs<-as.data.frame(scores[6:35,]);show(scores_gs);summary(scores_gs)
scores_sc<-as.data.frame(scores[36:67,]);show(scores_sc);summary(scores_sc)
scores_rl<-as.data.frame(scores[77:99,]);show(scores_rl);summary(scores_rl)
scores_tw<-as.data.frame(scores[100:112,]);show(scores_tw);summary(scores_tw)
scores_kj<-as.data.frame(scores[c(2,3,68:75),]);show(scores_kj);summary(scores_kj)
scores_cw<-as.data.frame(scores[c(4,5,76),]);show(scores_cw);summary(scores_cw)
scores_gl<-as.data.frame(scores[1,]);show(scores_gl);summary(scores_gl)

#正态性检验
par(mfrow=c(1,2))
hist(scores$成绩,freq=T,xlab="总成绩",ylab="人数",main="总成绩频数分布直方图")
qqnorm(scores$成绩);qqline(scores$成绩);
ad.test(scores$成绩);cvm.test(scores$成绩);pearson.test(scores$成绩);sf.test(scores$成绩);shapiro.test(scores$成绩)

#自定义调色板
mypalette<-colorRampPalette(c("steelblue1","blue3"))
mycolors<-mypalette(9)
show_col(mycolors)
#各题得分率的线箱图
boxplot(rates,names=c("第一题","第二题","第三题","第四题","第五题","第六题","第七题","第八题","第九题"),
        col=mycolors,main="各题得分率箱线图")

#PCA
scores_tt=scores[,4:12]
fa.parallel(scores_tt,fa="pc",n.iter=100,show.legend=F)
pca=princomp(scores_tt,cor=T)
summary(pca,loadings=T)
pca_data=predict(pca)
pca_data


#得到各题得分率向量，多样本位置参数检验
t11=rates$X1;t22=rates$X2;t33=rates$X3;t44=rates$X4;t55=rates$X5
t66=rates$X6;t77=rates$X7;t88=rates$X8;t99=rates$X9
data_kw1=c(t11,t22,t33,t44,t55,t66,t77,t88,t99)
mydata_kw1<-data.frame(data_kw1,group1=c(rep("第1题",112),rep("第2题",112),rep("第3题",112),
                      rep("第4题",112),rep("第5题",112),rep("第6题",112),rep("第7题",112),
                      rep("第8题",112),rep("第9题",112)))
kruskal.test(data_kw1~group1,data=mydata_kw1)
#任意道问题得分率差异
#wilcoxon秩和检验
wilcox_scores=matrix(0,9,9)
for(i in 1:8)
{ for(j in (i+1):9)
  { wilcox_scores1=wilcox.test(rates[,i],rates[,j],alt="less") 
    wilcox_scores2=wilcox.test(rates[,i],rates[,j],alt="greater") 
    if(wilcox_scores1$p.value<wilcox_scores2$p.value) {wilcox_scores[i,j]=wilcox_scores1$p.value*(-1)}
    else {wilcox_scores[i,j]=wilcox_scores2$p.value}
  }
}
wilcox_scores
wilcox_scores_v=as.vector(t(wilcox_scores))
write(wilcox_scores_v,file="E:/2021秋季学期/非参数统计/成绩/任意两题得分wilcoxon秩和检验.csv")
#正态记分检验
normal_scores=matrix(0,9,9)
for(i in 1:8)
{ for(j in (i+1):9)
  { x=rates[,i]; y=rates[,j];
    m=112; n=112;
    w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
    w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
    T=sum(w[w[,2]==1,4])
    w2=sum(w[,4]^2)
    S=sqrt(m*n*w2/(m+n-1)/(m+n))
    Z=T/S
    p=pnorm(Z)
    if(p>0.5){p=1-p}
    else {p=p*(-1)}
    normal_scores[i,j]=p
  }
}
normal_scores
normal_scores_v=as.vector(t(normal_scores))
write(normal_scores_v,file="E:/2021秋季学期/非参数统计/成绩/任意两题得分正态记分检验.csv")

#尺度检验
data_fk1=cbind(c(t11,t22,t33,t44,t55,t66,t77,t88,t99),
          c(rep(1,112),rep(2,112),rep(3,112),rep(4,112),rep(5,112),
            rep(6,112),rep(7,112),rep(8,112),rep(9,112)))
fligner.test(data_fk1[,1],data_fk1[,2])
#平方秩检验
#多样本
N=1008;k=9
d2=NULL
d=data_fk1
for(i in 1:k) d2=rbind(d2,cbind(abs(d[d[,2]==i,1]-mean(d[d[,2]==i,1])),d[d[,2]==i,1],i))
d3=cbind(d2[order(d2[,1]),],1:N,(1:N)^2)
Ti=NULL
for(i in 1:k) Ti=c(Ti,sum(d3[d3[,3]==i,5]))
ni=NULL
for(i in 1:k) ni=c(ni,nrow(d3[d3[,3]==i,]))
T=(N-1)*(sum(Ti^2/ni)-sum(Ti)^2/N)/(sum(d3[,5]^2)-sum(Ti)^2/N)
pvalue=pchisq(T,k-1,low=F)
pvalue
#两样本
square_scores=matrix(0,9,9)
for(i in 1:8)
{ for(j in (i+1):9)
  { x=rates[,i];y=rates[,j]
    m=length(x);n=length(y)
    x1=abs(x-mean(x));y1=abs(y-mean(y))
    xy1=c(x1,y1);xy0=c(x,y);xyi=c(rep(1,m),rep(2,n))
    xy=cbind(xy1,xy0,xyi)
    xy2=cbind(xy[order(xy[,1]),],1:(m+n),(1:(m+n))^2)
    T1=sum(xy2[xy2[,3]==1,5]);T2=sum(xy2[xy2[,3]==2,5])
    R=xy2[,5];meanR=mean(R)
    S=sqrt(m*n*(sum(R^2)-(m+n)*meanR^2)/(m+n)/(m+n-1))
    Zx=(T1-m*meanR)/S;Zy=(T2-n*meanR)/S
    if(pnorm(Zx)<pnorm(Zy)) {square_scores[i,j]=pnorm(Zx)*(-1)}
    else {square_scores[i,j]=pnorm(Zy)}
  }
}
square_scores
square_scores_v=as.vector(t(square_scores))
write(square_scores_v,file="E:/2021秋季学期/非参数统计/成绩/任意两道题目得分尺度检验平方秩.csv")


#各专业同学成绩有无显著差异
#总分
data_kw2=c(scores_gs$成绩,scores_sc$成绩,scores_rl$成绩,scores_tw$成绩,scores_kj$成绩,scores_cw$成绩,scores_gl$成绩)
mydata_kw2<-data.frame(data_kw2,group2=c(rep("工商管理",30),rep("市场营销",32),
                       rep("人力资源管理",23),rep("天文学",13),rep("会计学",10),
                       rep("财务管理",3),rep("管理科学",1)))
kruskal.test(data_kw2~group2,data=mydata_kw2)
#任意两道问题得分率差异
#wilcoxon秩和检验
data=c(scores_gs$成绩,scores_sc$成绩,scores_rl$成绩,scores_tw$成绩,scores_kj$成绩,scores_cw$成绩,scores_gl$成绩)
h=c(0,30,62,85,98,108,111,112)
wilcox_dep=matrix(0,7,7)
for(i in 1:6)
{ for(j in (i+1):7)
  { wilcox_dep1=wilcox.test(data[(h[i]+1):h[i+1]],data[(h[j]+1):h[j+1]],alt="less") 
    wilcox_dep2=wilcox.test(data[(h[i]+1):h[i+1]],data[(h[j]+1):h[j+1]],alt="greater") 
    if(wilcox_dep1$p.value<wilcox_dep2$p.value) {wilcox_dep[i,j]=wilcox_dep1$p.value*(-1)}
    else {wilcox_dep[i,j]=wilcox_dep2$p.value}
  }
}
wilcox_dep
wilcox_dep_v=as.vector(t(wilcox_dep))
write(wilcox_dep_v,file="E:/2021秋季学期/非参数统计/成绩/任意两专业成绩wilcoxon秩和检验.csv")
#正态记分检验
normal_dep=matrix(0,7,7)
h=c(0,30,62,85,98,108,111,112)
l=c(30,32,23,13,10,3,1)
for(i in 1:6)
{ for(j in (i+1):7)
  { x=data[(h[i]+1):h[i+1]]; y=data[(h[j]+1):h[j+1]];
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
    normal_dep[i,j]=p
  }
}
normal_dep
normal_dep_v=as.vector(t(normal_dep))
write(normal_dep_v,file="E:/2021秋季学期/非参数统计/成绩/任意两专业成绩正态记分检验.csv")


#尺度检验
data_fk2=list(scores_gs$成绩,scores_sc$成绩,scores_rl$成绩,scores_tw$成绩,scores_kj$成绩,scores_cw$成绩,scores_gl$成绩)
fligner.test(data_fk2)
#平方秩检验
#多样本
N=112;k=7
d2=NULL
d=cbind(data,c(rep(1,30),rep(2,32),rep(3,23),rep(4,13),rep(5,10),rep(6,3),rep(7,1)))
for(i in 1:k) d2=rbind(d2,cbind(abs(d[d[,2]==i,1]-mean(d[d[,2]==i,1])),d[d[,2]==i,1],i))
d3=cbind(d2[order(d2[,1]),],1:N,(1:N)^2)
Ti=NULL
for(i in 1:k) Ti=c(Ti,sum(d3[d3[,3]==i,5]))
ni=NULL
for(i in 1:k) ni=c(ni,nrow(d3[d3[,3]==i,]))
T=(N-1)*(sum(Ti^2/ni)-sum(Ti)^2/N)/(sum(d3[,5]^2)-sum(Ti)^2/N)
pvalue=pchisq(T,k-1,low=F)
pvalue
#两样本
square_dep=matrix(0,7,7)
for(i in 1:6)
{ for(j in (i+1):7)
  { x=data[(h[i]+1):h[i+1]]; y=data[(h[j]+1):h[j+1]];
    m=length(x);n=length(y)
    x1=abs(x-mean(x));y1=abs(y-mean(y))
    xy1=c(x1,y1);xy0=c(x,y);xyi=c(rep(1,m),rep(2,n))
    xy=cbind(xy1,xy0,xyi)
    xy2=cbind(xy[order(xy[,1]),],1:(m+n),(1:(m+n))^2)
    T1=sum(xy2[xy2[,3]==1,5]);T2=sum(xy2[xy2[,3]==2,5])
    R=xy2[,5];meanR=mean(R)
    S=sqrt(m*n*(sum(R^2)-(m+n)*meanR^2)/(m+n)/(m+n-1))
    Zx=(T1-m*meanR)/S;Zy=(T2-n*meanR)/S
    if(pnorm(Zx)<pnorm(Zy)) {square_dep[i,j]=pnorm(Zx)*(-1)}
    else {square_dep[i,j]=pnorm(Zy)}
  }
}
square_dep
square_dep_v=as.vector(t(square_dep))
write(square_dep_v,file="E:/2021秋季学期/非参数统计/成绩/任意两个专业成绩尺度检验平方秩.csv")










#分析总分差异主要出现在哪几道题上
#工商管理和市场营销
#两样本位置参数Brown-Mood检验
n1=dim(scores_gs)[1];n2=dim(scores_sc)[1];N=n1+n2
z=matrix(0,N,2)
B=c(rep(0,9))
for(i in 1:9)
{ z[,1]=c(scores_gs[,i+3],scores_sc[,i+3])
  z[,2]=c(rep(1,n1),rep(2,n2))
  m=median(z[,1]);m1=m2=NULL
  for(j in c(1,2)) {m1=c(m1,sum(z[z[,2]==j,1]>m));m2=c(m2,sum(z[z[,2]==j,1]<=m));}
  C=rbind(m1,m2)
  f1=fisher.test(C,alt="less")$p.value
  f2=fisher.test(C,alt="greater")$p.value
  if(f1<f2){B[i]=f1*(-1)}
  else {B[i]=f2}
}
B
i=2
#两样本位置参数wilcoxon秩和检验
w=c(rep(0,9))
for(i in 1:9)
{ w1=wilcox.test(scores_gs[,i+3],scores_sc[,i+3],alt="less")
  w2=wilcox.test(scores_gs[,i+3],scores_sc[,i+3],alt="greater")
  if(w1$p.value<w2$p.value) {w[i]=w1$p.value*(-1)}
  else {w[i]=w2$p.value}
}
w
#正态记分检验
H=c(rep(0,9))
for(i in 1:9)
{ x=scores_gs[,i+3]; y=scores_sc[,i+3];
  m=length(x);n=length(y);
  w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
  w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
  T=sum(w[w[,2]==1,4])
  w2=sum(w[,4]^2)
  S=sqrt(m*n*w2/(m+n-1)/(m+n))
  Z=T/S
  p=pnorm(Z)
  if(p>0.5){H[i]=1-p}
  else {H[i]=p*(-1)}
}
H

#工商管理和人力资源管理
#两样本位置参数Brown-Mood检验
n1=dim(scores_gs)[1];n2=dim(scores_rl)[1];N=n1+n2
z=matrix(0,N,2)
B=c(rep(0,9))
for(i in 1:9)
{ z[,1]=c(scores_gs[,i+3],scores_rl[,i+3])
  z[,2]=c(rep(1,n1),rep(2,n2))
  m=median(z[,1]);m1=m2=NULL
  for(j in c(1,2)) {m1=c(m1,sum(z[z[,2]==j,1]>m));m2=c(m2,sum(z[z[,2]==j,1]<=m));}
  C=rbind(m1,m2)
  f1=fisher.test(C,alt="less")$p.value
  f2=fisher.test(C,alt="greater")$p.value
  if(f1<f2){B[i]=f1*(-1)}
  else {B[i]=f2}
}
B
#两样本位置参数wilcoxon秩和检验
w=c(rep(0,9))
for(i in 1:9)
{ w1=wilcox.test(scores_gs[,i+3],scores_rl[,i+3],alt="less")
  w2=wilcox.test(scores_gs[,i+3],scores_rl[,i+3],alt="greater")
  if(w1$p.value<w2$p.value) {w[i]=w1$p.value*(-1)}
  else {w[i]=w2$p.value}
}
w
#正态记分检验
H=c(rep(0,9))
for(i in 1:9)
{ x=scores_gs[,i+3]; y=scores_rl[,i+3];
  m=length(x);n=length(y);
  w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
  w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
  T=sum(w[w[,2]==1,4])
  w2=sum(w[,4]^2)
  S=sqrt(m*n*w2/(m+n-1)/(m+n))
  Z=T/S
  p=pnorm(Z)
  if(p>0.5){H[i]=1-p}
  else {H[i]=p*(-1)}
}
H

#天文学和人力资源管理
#两样本位置参数Brown-Mood检验
n1=dim(scores_tw)[1];n2=dim(scores_rl)[1];N=n1+n2
z=matrix(0,N,2)
B=c(rep(0,9))
for(i in 1:9)
{ z[,1]=c(scores_tw[,i+3],scores_rl[,i+3])
  z[,2]=c(rep(1,n1),rep(2,n2))
  m=median(z[,1]);m1=m2=NULL
  for(j in c(1,2)) {m1=c(m1,sum(z[z[,2]==j,1]>m));m2=c(m2,sum(z[z[,2]==j,1]<=m));}
  C=rbind(m1,m2)
  f1=fisher.test(C,alt="less")$p.value
  f2=fisher.test(C,alt="greater")$p.value
  if(f1<f2){B[i]=f1*(-1)}
  else {B[i]=f2}
}
B
#两样本位置参数wilcoxon秩和检验
w=c(rep(0,9))
for(i in 1:9)
{ w1=wilcox.test(scores_tw[,i+3],scores_rl[,i+3],alt="less")
  w2=wilcox.test(scores_tw[,i+3],scores_rl[,i+3],alt="greater")
  if(w1$p.value<w2$p.value) {w[i]=w1$p.value*(-1)}
  else {w[i]=w2$p.value}
}
w
#正态记分检验
H=c(rep(0,9))
for(i in 1:9)
{ x=scores_tw[,i+3]; y=scores_rl[,i+3];
  m=length(x);n=length(y);
  w=cbind(c(x,y),c(rep(1,m),rep(2,n))); w=w[order(w[,1]),]
  w=cbind(w,(1:(m+n)),qnorm((1:(m+n))/(m+n+1)))
  T=sum(w[w[,2]==1,4])
  w2=sum(w[,4]^2)
  S=sqrt(m*n*w2/(m+n-1)/(m+n))
  Z=T/S
  p=pnorm(Z)
  if(p>0.5){H[i]=1-p}
  else {H[i]=p*(-1)}
}
H

#列联表，不同专业及格率是否相同
l1=matrix(c(12,18,24,8,20,3,10,3,5,5),nrow=5,ncol=2,byrow=T)
chisq.test(l1)
F=cbind(c(12,18,24,8),c(12,18,20,3),c(12,18,10,3),c(12,18,5,5),c(24,8,20,3),
        c(24,8,10,3),c(24,8,5,5),c(20,3,10,3),c(20,3,5,5),c(10,3,5,5))
L=matrix(0,10,10)
for(i in 1:10)
{ ff=matrix(F[,i],nrow=2,ncol=2,byrow=T)
  L[i,1]=fisher.test(ff)$p.value#拒绝0.009465
  if(L[i,1]>=0.05){L[i,2:10]=NA}
  else
  { #比例差估计
    p1_hat=ff[1,1]/(ff[1,1]+ff[1,2])
    p2_hat=ff[2,1]/(ff[2,1]+ff[2,2])
    p_hat=p1_hat-p2_hat
    SE1=sqrt(p1_hat*(1-p1_hat)/(ff[1,1]+ff[1,2])+p2_hat*(1-p2_hat)/(ff[2,1]+ff[2,2]))
    CI_low1=p_hat-1.96*SE1
    CI_up1=p_hat+1.96*SE1
    L[i,2]=p_hat;L[i,3]=CI_low1;L[i,4]=CI_up1
    #相对风险RR
    RR=p1_hat/p2_hat
    SE2=sqrt((1-p1_hat)/ff[1,1]+(1-p2_hat)/ff[2,1])
    CI_low2=RR*exp(-1.96*SE2)
    CI_up2=RR*exp(1.96*SE2)
    L[i,5]=RR;L[i,6]=CI_low2;L[i,7]=CI_up2
    #胜算比OR
    OR=ff[1,1]*ff[2,2]/(ff[1,2]*ff[2,1])
    SE3=sqrt(1/ff[1,1]+1/ff[1,2]+1/ff[2,1]+1/ff[2,2])
    CI_low3=OR*exp(-1.96*SE3)
    CI_up3=OR*exp(1.96*SE3)
    L[i,8]=OR;L[i,9]=CI_low3;L[i,10]=CI_up3
  }
}
L
L_v=as.vector(t(L))
write(L_v,file="E:/2021秋季学期/非参数统计/成绩/列联表.csv")







#回归

x=c(127,155,127,131,153,180,144,189,172,160,170,176,179,163,173,183,184,169,153,159)
y=c(13,33,38,10,57,89,42,70,78,23,68,77,58,63,89,84,72,49,47,63)
length(x)
length(y)
median(x)
rank(x)
median(c(176,179))
median(c(13,38,10,42,57,47,33,63,23,63))
median(c(49,68,78,89,77,58,89,84,72,70))
median(y-1.4082*x)
b=matrix(0,1,20)
k=1
for (i in 1:20)
{
  for (j in 1:20)
  {
    if (j>i)
    {
      b[k]=(y[j]-y[i])/(x[j]-x[i])
      k=k+1
    }
  }
}
b
length(b)
median(b)
median(y-1.0384*x)

lqs(y~x,method="lms")
lqs(y~x,method="lts")
lqs(y~x,method="S")
A=matrix(0,1,20)
for (t in 1:20)
{
  B=matrix(0,1,19)
  k=1
  for (j in 1:20)
  {
    if (j!=t)
    {
      B[k]=(y[j]-y[t])/(x[j]-x[t])
      k=k+1
    }
  }
  A[t]=median(B)
}
A
median(A)
median(y-1.0769*x)

TT=function(x,y,alpha)
{
  n=length(x)
  s=NULL
  for (i in 1:(n-1))
  {
    for (j in (i+1):n)
    {
      s=c(s,(y[j]-y[i])/(x[j]-x[i]))
    }
  }
  b=median(s)
  a=median(y-b*x)
  e=y-a-b*x
  m=length(s)
  s=sort(s)
  z=NULL
  for (i in 1:m)
  {
    z=c(z,cor.test(x,y-s[i]*x,method="kendall")$p.value)
  }
  for (i in 1:floor(m/2))
  {
    if (z[i]>alpha/2)
    {
      bound=c(i-1,m-i+2,s[i-1],s[m-i+2],s[i-1])
      break
    }
  }
  list(nrow(d),coefficient=c(a,b),residual=e,ci=bound[1:4],confid=1-2*bound[5])
}
list(TT(x,y,0.05))
