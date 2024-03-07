setwd("C:\\Users\\ADMIN\\Desktop\\acads")
ap=read.csv("prev2.csv")

ap
library(ggplot2)
library(dplyr)
library(plotly)
ap=ap[,1:5]
ap=ap[1:16,]
ap=ap[1:15,]
ap
str(ap)
kp=data.frame(ap$tasks,ap$x,ap$y)
kp$ap.tasks=as.factor(kp$ap.tasks)
kp=kp[,-1]
k2 <- kmeans(kp, centers = 2, nstart = 100)
typeof(k2)
typeof(k2$centers)
k2$centers=data.frame(x=c(33.778,71.33),y=c(23.4,62.0))+ggtitle("2 clu")+
geom_point() +
  geom_point(data = dfq, col = 'blue')
k2$centers
str(k2)
k2$cluster=c(3,1,0,1,2,1,0,1,1,2,2,3,0,2,3)		
kcluster=c(0,0,0,0,1,0,1,0,0,1,1,0,1,1,0)
kp[15,3]=24.5
kp

100-6-1-9-4-8-3-12-2-15-100
kp=kp[-16,]
kp
fviz_cluster(k2, data=kp)+ggtitle("2 clusters")
xa <- c(48.6,27.2,70.6)
ya <- c(52.8,53,35.6)

coords = paste(xa,ya,sep=",")
kp[16,]=c(33.778,54.889)
df = data.frame(Sepal.Width = 5.6, Sepal.Length = 3.9) 

ggplot(df,aes(x,y))+geom_point(col="blue")+
  geom_label(aes(x+.5,y+0.5,label=coords))
33.778	54.889
71.333	35.500
dfq=data.frame(ap.x=xa,ap.y=ya)
dfq=data.frame(x=c(0),y=c(0))

kp=data.frame(ap$tasks,ap$x,ap$y)
kcluster=as.character(kcluster)
kp$clus=kcluster
kp
dfq=rbind(c(0,0),dfq)
tyu=ggplot() +
  geom_point(data = ap, 
             mapping = aes(x = x, 
                           y = y, 
                           color=ap$X2.cluster),size=1.5)+
  geom_label(data=ap,mapping = aes(x=x,y=y,label=taskid)
             ,nudge_x = 3, nudge_y = 3,size=2)
tyu
tyu=tyu +annotate(geom="text"
      , x=47, y=51, label="centroid(cluster 0)",size=3,color="black")+
  annotate(geom="text"
           , x=65, y=40, label="centroid(cluster 1)",size=3,color="black")+
  annotate(geom="text"
           , x=65, y=40, label="centroid(cluster 1)",size=3,color="black")

tyu

tau=txu+labs(labels=c("1","2","3"),color="Legend",x="X coordinate",y="Y coordinate,",title = " 2 cluster scatter plot")
tau
tyu=tyu + scale_fill_discrete(name = "New Legend Title")
tyu
tyr=ggplot(data=kp,aes(x=ap.x,y=ap.y))+geom_line(data=kp,mapping=aes(x=ap.x,y=ap.y))+geom_point(mapping = aes_string(x = dfq$ap.x, y = dfq$ap.y),size=5,color=c("black"))
tyr
tyk=tyu+scale_fill_discrete(labels=c("Cluster 0","Cluster 1","Cluster 2","Cluster 3","Robot_1_path","Robot_2_path"))
tbu=tau+scale_color_manual(labels = c("Cluster 0", "Cluster 1","Robot 1 path","Robot 2 path"),values=c("chartreuse1","darkgoldenrod1","dodgerblue1","darkmagenta"))
100-6-1-9-4-8-3-12-2-15-100
tbu
tau+scale_fill_discrete(guide=guide_legend(reverse = TRUE))
tbu+scale_fill_manual(breaks=c("Cluster 0","Cluster 1","Cluster 2","Robot 1 path","Robot 2 path"))
kp[1,]
View(kp)
kp$robot=c(1,1,1,1,2,1,2,1,1,2,2,1,2,2,1)
kp1=ap%>%filter(ap$X2.cluster=="Cluster_1")
kp1
kp1[10,]=c(0,0,0,1)
kp1=kp1[,-3]

ggporbind(c(0,0,1),kp1)
kp1
library(dplyr)
ref=c(6,1,9,4,8,3,12,2,15,10)
kp1[match(ref, kp1$taskseq),]

kp1$taskseq=c(1:10)
kp1
kp2=read.csv("102.csv")
kp2

kp2[10,3]=24.5
kp3=read.csv("101.csv")
kp3
dfq=data.frame(x=c(0),y=c(0))


home 11    1 12  3  7 13 14  5 10  15  home 

ap
l=c(6,9,4,8,2,16)
m=c(11,1,12,  3,  7, 13, 14,  5, 10,  15,16) 

kp3=data.frame(x=c(0),y=c(0))

kp3
for(i in l)
{
  kp3=rbind(kp3,c(ap$x[i],ap$y[i]))
}
r1=kp3$x[1:6]
r2=kp3$x[2:7]
r3=kp3$y[1:6]
r4=kp3$y[2:7]
kp2=data.frame(x=c(0),y=c(0))
for(i in m)
{
  kp2=rbind(kp2,c(ap$x[i],ap$y[i]))
}
kp2
kp3

dry=data.frame(r1=r1,r2=r2,r3=r3,r4=r4)
kp2
dru=data.frame(r1=kp2$x[1:11],r2=kp2$x[2:12],r3=kp2$y[1:11],r4=kp2$y[2:12])
dru
tyu=ggplot() + geom_point(data = ap[1:15,],mapping = aes(x = x,y = y,color = ap[1:15,]$X4.cluster),size=2)+geom_label(data=ap[1:15,],mapping = aes(x=x,y=y,label=taskid)
                              ,nudge_x = 3, nudge_y = 3,size=2)

tyu
home=data.frame("x"=c(0),"y"=c(0))

tzu=tyu+geom_segment(data=dry,mapping=aes(x=r1,y=r3,xend=r2,yend=r4,color=c("Robot 1 path")),
          lineend ="butt",arrow.fill="black", 
          arrow=arrow(length=unit(0.1,"inches")),show.legend = FALSE)

txu=tzu+geom_segment(data=dru,aes(x=r1,y=r3,xend=r2,yend=r4,color=c("Robot 2 path")),arrow = arrow(length =unit(0.1,"inches")),show.legend =FALSE)+
  annotate(geom="text", x=4, y=4, label="home",size=4,color="black")

tyu
txu=txu+geom_point(home,mapping=aes(x=x,y=y),size=5)
txu

tau=txu+labs(labels=c("1","2","3"),color="Legend",x="X coordinate",y="Y coordinate,",
             title = " 4 cluster scatter plot with Energy")

tau
tbu=tau+scale_color_manual(labels = c("Cluster 0", "Cluster 1","Cluster 2","Cluster 3","Robot 1 path","Robot 2 path"),
  values=c("chartreuse1","darkgoldenrod1","dodgerblue1","darkmagenta","brown","orange"))
tbu=tau + scale_fill_discrete(labels=c("Robot 1 path","Robot 2 path","Cluster 0", "Cluster 1"))
tbu

ap
d1=dist(ap[1:16,2:3])
head(d1)
d1=as.matrix(d1)
d1=d1/9.212
colnames(d1)[16]="home"
row.names(d1)[16]="home"
write.csv(d1,"dist.csv")
d1=as.matrix(d1)
d=as.data.frame()
fig=plot_ly(z=d1[1:16,1:16],type="heatmap",colors=colorRamp(c("red","blue","green","yellow")))%>% 
  layout(xaxis = list(tickvals = c(0,seq(1:15)), ticktext = row.names(d1)))%>%
  layout(yaxis = list(tickvals = c(0,seq(1:15)), ticktext = row.names(d1)),title="Heatmap of seconds")

row.names(d1)
fig
seq(1,15,2)
ap=ap[1:16,2:3]
mat=matrix(0,16,16)
mat
atan(1)
for(i in 1:nrow(ap))
{
  for(j in 1:nrow(ap))
  {
    
    tn=((ap$y[j]-ap$y[i])/(ap$x[j]-ap$x[i]))
    mat[i,j]=atan(tn)
  }
}

mat=as.data.frame(mat)   
for(x in 1:nrow(ap))
  for(y in 1:nrow(ap))
  mat[x,y]=0
mat

