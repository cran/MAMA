gene.select.FC<-function(data, cutoff)
{
lists<-list()
for (i in 1:ncol(data)) lists[[i]]<-rownames(data)[abs(data[,i])>=cutoff]
names(lists)<-colnames(data)
return(lists)
}