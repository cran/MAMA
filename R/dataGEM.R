dataGEM<-function(data, group )
{
ph<-pData(data)[,group]
levels(ph)<-c(0,1)
dum<-list(expr=exprs(data), class=ph, keys=featureNames(data))
return(dum)
}