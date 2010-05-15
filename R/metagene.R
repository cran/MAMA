metagene<-function(x, results)
{
metagene<-list()
for (i in 1:length(results))
if (x %in% rownames(results[[i]])) metagene[[i]]<-results[[i]][x,]
names(metagene)<-names(results)
return(metagene)
}