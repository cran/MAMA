meta.test<-function(..., class.col, data.names)
{
args<-list(...)
N<-length(args)
tespval<-list()
for (i in 1:N)
{
if (i==1) {
test<-mt.teststat(exprs(args[[i]]), as.numeric(pData(args[[i]])[,class.col[i]])-1)
pval<-2*pt(-abs(test),df=dim(exprs(args[[i]]))[1]-1)
tespval$test=test
tespval$p=pval
} else {
test<-mt.teststat(exprs(args[[i]]), as.numeric(pData(args[[i]])[,class.col[i]])-1)
pval<-2*pt(-abs(test),df=dim(exprs(args[[i]]))[1]-1)
tespval$test=cbind(tespval$test,test)
tespval$p=cbind(tespval$p,pval)
}
}
rownames(tespval$test)<-rownames(exprs(args[[1]]))
rownames(tespval$p)<-rownames(exprs(args[[1]]))
colnames(tespval$test)<-data.names
colnames(tespval$p)<-data.names
return(tespval)
}