mergedata<-function(...,class.col)
{
args<-list(...)
N<-length(args)
merge<-list()
for (i in c(1:N))
{
if (i==1) {
merge$dat=exprs(args[[i]])
merge$cl=as.numeric(pData(args[[i]])[,class.col[i]])
merge$origin=c(rep(i,length(as.numeric(pData(args[[i]])[,class.col[i]]))))
} else {
merge$dat=cbind(merge$dat, exprs(args[[i]]))
merge$cl=c(merge$cl,as.numeric(pData(args[[i]])[,class.col[i]]))
merge$origin=c(merge$origin,rep(i,length(as.numeric(pData(args[[i]])[,class.col[i]]))))
}
}
return(merge)
}