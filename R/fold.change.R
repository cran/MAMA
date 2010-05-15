fold.change<-function(data, group)
{
 mean1j<-apply(exprs(data)[,as.numeric(pData(
 data)[,group])==1],1,mean)
 mean2j<-apply(exprs(data)[,as.numeric(pData(
 data)[,group])==2],1,mean)
 fc<-mean1j-mean2j
return(fc)
}
