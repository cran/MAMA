metalist.to.matrix<-function(list,genenames)
{
xx<-matrix(0,nrow=length(list$TestStatistic), ncol=length(list))
for (i in 1:(length(list)-1))
{xx[list[[i]],i]<-1}
xx[,length(list)]<-list[[length(list)]]
colnames(xx)<-names(list)
rownames(xx)<-genenames
return(xx)
}

