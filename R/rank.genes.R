rank.genes<-function(T,p)
{
gr1<-p[T<0]
gr2<-p[T==0]
gr3<-1-p[T>0]
ran<-c(rank(gr1, ties="average"), rank(gr2, ties="average")+length(gr1), 
  rank(gr3, ties="average")+length(gr1)+length(gr2))
ran<-as.data.frame(ran)
T<-as.data.frame(T)
ran=ran[rownames(T),]
ran=as.data.frame(ran)
rownames(ran)=rownames(T)
return(ran)
}