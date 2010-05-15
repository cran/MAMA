dataZ<-function(data, group, nsamp, varname)
{
dum<-data
exprs(dum)<-exprs(data)[, c(sample(which(as.numeric(pData(data)[,group])==1),nsamp),
 sample(which(as.numeric(pData(data)[,group])==2),nsamp))]
vars = list("var1", "var2")
tmp = data.frame(cl=c(rep(0,nsamp),rep(1,nsamp)), names=colnames(exprs(dum)))
names(tmp)<-c(varname,"names")
names(vars) = names(tmp)
pdata1 = new("AnnotatedDataFrame")
pData(dum) = tmp
return(dum)
}
