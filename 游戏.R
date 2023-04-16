###3、游戏
library(nortest)#正态性检验包

#导入数据集，初始数据集已按游戏类型分类排序，第二顺序为热度
game1=read.csv("E:/2021秋季学期/非参数统计/游戏/安卓手机游戏-类型.csv",header=TRUE)
head(game1)
game1<-as.data.frame(game1)
attach(game1)
summary(game1)

#删掉缺失评分信息以及异常评分的数据，按评分排序
game2=read.csv("E:/2021秋季学期/非参数统计/游戏/安卓手机游戏-评分.csv",header=TRUE)
head(game2)
game2<-as.data.frame(game2)
attach(game2)
summary(game2)

#初始给定数据集，未进行任何处理
game3=read.csv("E:/2021秋季学期/非参数统计/游戏/安卓手机游戏-热度.csv",header=TRUE)
head(game3)
game3<-as.data.frame(game3)
attach(game3)
summary(game3)

#删掉缺失评分数据后，按游戏类型分类
game4=read.csv("E:/2021秋季学期/非参数统计/游戏/安卓手机游戏-评分-类型排序.csv",header=TRUE)
head(game4)
game4<-as.data.frame(game4)
attach(game4)
summary(game4)

#正态性检验
par(mfrow=c(1,2))
heat=game1$热度
hist(heat,freq=T,xlab="热度",ylab="游戏数量",main="热度频数分布直方图")
qqnorm(heat);qqline(heat);
ad.test(heat);cvm.test(heat);pearson.test(heat);sf.test(heat);shapiro.test(heat)
rank=game2$评分
hist(rank,freq=T,xlab="评分",ylab="游戏数量",main="评分频数分布直方图")
qqnorm(rank);qqline(rank);
ad.test(rank);cvm.test(rank);pearson.test(rank);sf.test(rank);shapiro.test(rank)



#各类型游戏热度差异
data_kw1=c(heat[1:90],heat[91:226],heat[227:263],heat[264:291],heat[292:405],
           heat[406:490],heat[491:576],heat[577:629],heat[630:647],heat[648:757],
           heat[758:803],heat[804:815],heat[816:1107],heat[1108:1127],heat[1128:1140])
mydata_kw1<-data.frame(data_kw1,group1=c(rep("策略游戏",90),rep("动作游戏",136),rep("飞行游戏",37),
                      rep("格斗游戏",28),rep("角色扮演",114),rep("竞速游戏",85),rep("冒险解谜",86),
                      rep("模拟经营",53),rep("棋牌游戏",18),rep("射击游戏",110),rep("体育运动",46),
                      rep("养成游戏",12),rep("益智休闲",292),rep("音乐游戏",20),rep("游戏工具",13)))
kruskal.test(data_kw1~group1,data=mydata_kw1)
#任意两类游戏热度差异
#wilcoxon秩和检验
h=c(0,90,226,263,291,405,490,576,629,647,757,803,815,1107,1127,1140)
wilcox_heat=matrix(0,15,15)
for(i in 1:14)
{ for(j in (i+1):15)
  { wilcox_heat1=wilcox.test(heat[(h[i]+1):h[i+1]],heat[(h[j]+1):h[j+1]],alt="less") 
    wilcox_heat2=wilcox.test(heat[(h[i]+1):h[i+1]],heat[(h[j]+1):h[j+1]],alt="greater") 
    if(wilcox_heat1$p.value<wilcox_heat2$p.value) {wilcox_heat[i,j]=wilcox_heat1$p.value*(-1)}
    else {wilcox_heat[i,j]=wilcox_heat2$p.value}
  }
}
wilcox_heat
wilcox_heat_v=as.vector(t(wilcox_heat))
write(wilcox_heat_v,file="E:/2021秋季学期/非参数统计/游戏/任意两类游戏热度wilcoxon秩和检验.csv")
#正态记分检验
h=c(0,90,226,263,291,405,490,576,629,647,757,803,815,1107,1127,1140)
l=c(90,136,37,28,114,85,86,53,18,110,46,12,292,20,13)
normal_heat=matrix(0,15,15)
for(i in 1:14)
{ for(j in (i+1):15)
  { x=heat[(h[i]+1):h[i+1]]; y=heat[(h[j]+1):h[j+1]];
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
    normal_heat[i,j]=p
  }
}
normal_heat
normal_heat_v=as.vector(t(normal_heat))
write(normal_heat_v,file="E:/2021秋季学期/非参数统计/游戏/任意两类游戏热度正态记分检验.csv")


#平方秩检验
#多样本
N=1140;k=15
d2=NULL
d=cbind(heat,c(rep(1,90),rep(2,136),rep(3,37),rep(4,28),rep(5,114),rep(6,85),
               rep(7,86),rep(8,53),rep(9,18),rep(10,110),rep(11,46),rep(12,12),
               rep(13,292),rep(14,20),rep(15,13)))
for(i in 1:k) d2=rbind(d2,cbind(abs(d[d[,2]==i,1]-mean(d[d[,2]==i,1])),d[d[,2]==i,1],i))
d3=cbind(d2[order(d2[,1]),],1:N,(1:N)^2)
Ti=NULL
for(i in 1:k) Ti=c(Ti,sum(d3[d3[,3]==i,5]))
ni=NULL
for(i in 1:k) ni=c(ni,nrow(d3[d3[,3]==i,]))
T=(N-1)*(sum(Ti^2/ni)-sum(Ti)^2/N)/(sum(d3[,5]^2)-sum(Ti)^2/N)
pvalue=pchisq(T,k-1,low=F)
#两样本
h=c(0,90,226,263,291,405,490,576,629,647,757,803,815,1107,1127,1140)
square_heat=matrix(0,15,15)
for(i in 1:14)
{ for(j in (i+1):15)
  { x=heat[(h[i]+1):h[i+1]];y=heat[(h[j]+1):h[j+1]]
    m=length(x);n=length(y);
    x1=abs(x-mean(x));y1=abs(y-mean(y));
    xy1=c(x1,y1);xy0=c(x,y);xyi=c(rep(1,m),rep(2,n));
    xy=cbind(xy1,xy0,xyi)
    xy2=cbind(xy[order(xy[,1]),],1:(m+n),(1:(m+n))^2)
    T1=sum(xy2[xy2[,3]==1,5]);T2=sum(xy2[xy2[,3]==2,5])
    R=xy2[,5];meanR=mean(R);
    S=sqrt(m*n*(sum(R^2)-(m+n)*meanR^2)/(m+n)/(m+n-1))
    Zx=(T1-m*meanR)/S;Zy=(T2-n*meanR)/S
    if(pnorm(Zx)<pnorm(Zy)) {square_heat[i,j]=pnorm(Zx)*(-1)}
    else{square_heat[i,j]=pnorm(Zy)}
  }
}
square_heat
square_heat_v=as.vector(t(square_heat))
write(square_heat_v,file="E:/2021秋季学期/非参数统计/游戏/任意两类游戏热度尺度检验平方秩.csv")


#各类型游戏评分差异
#wilcoxon秩和
rankp=game4$评分
data_kw2=c(rankp[1:71],rankp[72:188],rankp[189:213],rankp[214:238],rankp[239:341],
           rankp[342:410],rankp[411:494],rankp[495:543],rankp[544:557],rankp[558:653],
           rankp[654:692],rankp[693:703],rankp[704:913],rankp[914:928],rankp[929:936])
mydata_kw2<-data.frame(data_kw2,group2=c(rep("策略游戏",71),rep("动作游戏",117),rep("飞行游戏",25),
                      rep("格斗游戏",25),rep("角色扮演",103),rep("竞速游戏",69),rep("冒险解谜",84),
                      rep("模拟经营",49),rep("棋牌游戏",14),rep("射击游戏",96),rep("体育运动",39),
                      rep("养成游戏",11),rep("益智休闲",210),rep("音乐游戏",15),rep("游戏工具",8)))
kruskal.test(data_kw2~group2,data=mydata_kw2)
#任意两类游戏评分差异
#wilcoxon秩和检验
h=c(0,71,188,213,238,341,410,494,543,557,653,692,703,913,928,936)
wilcox_rank=matrix(0,15,15)
for(i in 1:14)
{ for(j in (i+1):15)
  { wilcox_rank1=wilcox.test(rankp[(h[i]+1):h[i+1]],rankp[(h[j]+1):h[j+1]],alt="less") 
    wilcox_rank2=wilcox.test(rankp[(h[i]+1):h[i+1]],rankp[(h[j]+1):h[j+1]],alt="greater") 
    if(wilcox_rank1$p.value<wilcox_rank2$p.value) {wilcox_rank[i,j]=wilcox_rank1$p.value*(-1)}
    else {wilcox_rank[i,j]=wilcox_rank2$p.value}
  }
}
wilcox_rank
wilcox_rank_v=as.vector(t(wilcox_rank))
write(wilcox_rank_v,file="E:/2021秋季学期/非参数统计/游戏/任意两类游戏评分wilcoxon秩和检验.csv")
#正态记分检验
h=c(0,71,188,213,238,341,410,494,543,557,653,692,703,913,928,936)
l=c(71,117,25,25,103,69,84,49,14,96,39,11,210,15,8)
normal_rank=matrix(0,15,15)
for(i in 1:14)
{ for(j in (i+1):15)
  { x=rankp[(h[i]+1):h[i+1]]; y=rankp[(h[j]+1):h[j+1]];
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
    normal_rank[i,j]=p
  }
}
normal_rank
normal_rank_v=as.vector(t(normal_rank))
write(normal_rank_v,file="E:/2021秋季学期/非参数统计/游戏/任意两类游戏评分正态记分检验.csv")

#平方秩检验
#多样本
N=936;k=15
d2=NULL
d=cbind(rankp,c(rep(1,71),rep(2,117),rep(3,25),rep(4,25),rep(5,103),
               rep(6,69),rep(7,84),rep(8,49),rep(9,14),rep(10,96),
               rep(11,39),rep(12,11),rep(13,210),rep(14,15),rep(15,8)))
for(i in 1:k) d2=rbind(d2,cbind(abs(d[d[,2]==i,1]-mean(d[d[,2]==i,1])),d[d[,2]==i,1],i))
d3=cbind(d2[order(d2[,1]),],1:N,(1:N)^2)
Ti=NULL
for(i in 1:k) Ti=c(Ti,sum(d3[d3[,3]==i,5]))
ni=NULL
for(i in 1:k) ni=c(ni,nrow(d3[d3[,3]==i,]))
T=(N-1)*(sum(Ti^2/ni)-sum(Ti)^2/N)/(sum(d3[,5]^2)-sum(Ti)^2/N)
pvalue=pchisq(T,k-1,low=F)
#两样本
h=c(0,71,188,213,238,341,410,494,543,557,653,692,703,913,928,936)
square_rank=matrix(0,15,15)
for(i in 1:14)
{ for(j in (i+1):15)
  { x=heat[(h[i]+1):h[i+1]];y=heat[(h[j]+1):h[j+1]]
    m=length(x);n=length(y)
    x1=abs(x-mean(x));y1=abs(y-mean(y))
    xy1=c(x1,y1);xy0=c(x,y);xyi=c(rep(1,m),rep(2,n))
    xy=cbind(xy1,xy0,xyi)
    xy2=cbind(xy[order(xy[,1]),],1:(m+n),(1:(m+n))^2)
    T1=sum(xy2[xy2[,3]==1,5]);T2=sum(xy2[xy2[,3]==2,5])
    R=xy2[,5];meanR=mean(R)
    S=sqrt(m*n*(sum(R^2)-(m+n)*meanR^2)/(m+n)/(m+n-1))
    Zx=(T1-m*meanR)/S;Zy=(T2-n*meanR)/S
    if(pnorm(Zx)<pnorm(Zy)) {square_rank[i,j]=pnorm(Zx)*(-1)}
    else {square_rank[i,j]=pnorm(Zy)}
  }
}
square_rank
square_rank_v=as.vector(t(square_rank))
write(square_rank_v,file="E:/2021秋季学期/非参数统计/游戏/任意两类游戏评分尺度检验平方秩.csv")



#各类型游戏评论数差异
data_kw2=c(comment[1:90],comment[91:226],comment[227:263],comment[264:291],comment[292:405],
           comment[406:490],comment[491:576],comment[577:629],comment[630:647],comment[648:757],
           comment[758:803],comment[804:815],comment[816:1107],comment[1108:1127],comment[1128:1140])
mydata_kw2<-data.frame(data_kw2,group2=c(rep("策略游戏",90),rep("动作游戏",136),rep("飞行游戏",37),
                      rep("格斗游戏",28),rep("角色扮演",114),rep("竞速游戏",85),rep("冒险解谜",86),
                      rep("模拟经营",53),rep("棋牌游戏",18),rep("射击游戏",110),rep("体育运动",46),
                      rep("养成游戏",12),rep("益智休闲",292),rep("音乐游戏",20),rep("游戏工具",13)))
kruskal.test(data_kw2~group2,data=mydata_kw2)
with(data=mydata_kw2,pairwise.wilcox.test(x=data_kw2,g=group2,p.adjust.method="bonferroni"))
#尺度检验
data_fk2=list(comment[1:90],comment[91:226],comment[227:263],comment[264:291],comment[292:405],
              comment[406:490],comment[491:576],comment[577:629],comment[630:647],comment[648:757],
              comment[758:803],comment[804:815],comment[816:1107],comment[1108:1127],comment[1128:1140])
fligner.test(data_fk2)
#单边方差检验
h=c(0,90,226,263,291,405,490,576,629,647,757,803,815,1107,1127,1140)
a=matrix(0,15,15)
for(i in 1:14)
{ for(j in (i+1):15)
  { a1=ansari.test(game1$评论数[(h[i]+1):h[i+1]],game1$评论数[(h[j]+1):h[j+1]],alt="less") 
    a2=ansari.test(game1$评论数[(h[i]+1):h[i+1]],game1$评论数[(h[j]+1):h[j+1]],alt="greater") 
    if(a1$p<a2$p) {a[i,j]=a1$p*(-1)}
    else {a[i,j]=a2$p}
  }
}


#相关性分析
#spearman
cor.test(game1$热度,game1$评论数,meth="spearman")
cor.test(game1$热度,game1$评论数,meth="kendall")
cor.test(game1$热度,game1$喜欢数,meth="spearman")
cor.test(game1$热度,game1$喜欢数,meth="kendall")
cor.test(game1$喜欢数,game1$评论数,meth="spearman")
cor.test(game1$喜欢数,game1$评论数,meth="kendall")
cor.test(game4$评分,game4$热度,meth="spearman")
cor.test(game4$评分,game4$热度,meth="kendall")
cor.test(game4$评分,game4$评论数,meth="spearman")
cor.test(game4$评分,game4$评论数,meth="kendall")
cor.test(game4$评分,game4$喜欢数,meth="spearman")
cor.test(game4$评分,game4$喜欢数,meth="kendall")



