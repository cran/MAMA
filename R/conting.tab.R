conting.tab<-function(lists)
{
n.method<-length(lists)
names.method<-names(lists)
tab<-matrix(0,nrow=n.method,ncol=n.method)
colnames(tab)<-names.method
rownames(tab)<-names.method
for (i in 1:n.method)
for (j in 1:n.method)
if (i!=j) tab[i,j]<-length(which(lists[[i]] %in% lists[[j]])) else tab[i,j]<-NA 
return(tab)
}

