setwd("C:\\Users\\ADMIN\\Desktop\\acads")

vrp1
library(ClusterR)
ktemp=KMeans_rcpp(vrp1[,-c(1,8,9)],3)
ktemp$clusters
library(optrees)

v=read.csv("Vrp_data.csv")
v=v[,c(2:3)]
v[16,]=c(0,0)
head(v)
tail(v)
d=dist(v)
head(d)
d=as.matrix(d)
d

dtom=function(dy,dis)
{ dtm=data.frame()
 for(i in 1:(nrow(dy)-1) )
 {
   for(j in i:nrow(dy))
   {
     dtm=rbind(dtm,c(i,j,dis[i,j]))
   }
 }
return(dtm)
}

dtom()

read.csv("dynamic24.csv")

nodes=setdiff(c(1:16),nodes)
nodes=append(nodes,16)
nodes=c(11,15,10,5,14,13,12,3,7,16)

nodes
prog=data.frame()
for(i in 1:10)
{
p=paste("dynamic2",i,".csv",sep="")
dy=read.csv(p)
dy=as.data.frame(dy)
dy=dy[,c(3:4)]
dy=rbind(dy,c(0,0))
dis=dist(dy)
dis=as.matrix(dis)

dtemp=dtom(dy,dis)
dtemp=data.frame(dtemp)
dtemp=data.matrix(dtemp)
dtemp
h=getMinimumSpanningTree(nodes,dtemp,start.node=16,algorithm = "Prim")
prog=rbind(prog,h$tree.arcs)
}
prog=cbind(prog,)

lj=list()

for(i in 1:10)
{
  lj=append(lj,c(rep(i,(length(nodes)-1))))
}

prog=prog[,c(1:3)]
prog$iter=lj  
prog
View(prog)
37.33+27.018+23.194+15.083+d[2,8]+d[2,16]
(rep(i,(length(nodes)-1)))
#69482
#1 3 12 15 13
# 11 7 10 5 14
dy
prog$weight[1]+prog$weight[2]+prog$weight[13]+prog$weight[19]+prog$weight[20]+d[7,16]
prog$weight[1]+prog$weight[2]+prog$weight[23]+prog$weight[34]+d[4,2]+d[2,8]+d[8,12]+d[12,13]+d[13,15]+d[15,16]
  
d[16,11]+d[11,7]+d[7,14]+prog$weight[19]+prog$weight[23]+d[10,16]
sum(prog$weight[1:4])+prog$weight[45]+prog$weight[46]+d[14,13]+d[13,7]+d[7,3]+d[3,12]+d[12,15]+d[15,16]

d[15,16]+d[15,13]+d[12,13]+d[12,3]+d[3,1]+d[1,16]




prog$weight[1]+prog$weight[12]+prog$weight[23]+prog$weight[33]+prog$weight[45]+prog$weight[46]+prog$weight[47]+prog$weight[48]+d[7,2]
+prog$weight[48]+d[10,14]+prog$weight[100]+d[5,16]


str(dtemp)

dtemp$X1.1=as.factor(dtemp$X1.1)

getMinimumSpanningTree(nodes,dtemp,start.node=1, algorithm = "Prim")

prog$weight[1]+prog$weight[7]+prog$weight[13]+prog$weight[14]+dis[2,8]+d[16,6]+d[2,16]
prog$weight[1]+prog$weight[2]+prog$weight[3]+prog$weight[31]+prog$weight[42]+prog$weight[7]+prog$weight[8]+prog$weight[9]+d[7,16]
prog$weight[1]+prog$weight[7]+prog$weight[13]+d[2,8]+d[4,8]+d[4,16]
