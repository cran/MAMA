metagene<-function(x, results)
{
metagene<-list()
for (i in 1:length(results))
{
if (x %in% rownames(results[[i]])) metagene[[i]]<-results[[i]][x,]
if ("METRADISC.res" %in% class(results[[i]])) {
  metagene[[i]] <- c(results[[i]]$RQ[x,], results[[i]]$MCtest[x,]) 
}
if ("ES.GeneMeta.res" %in% class(results[[i]])  )
  metagene[[i]] <- c(results[[i]]$theScores[x,], results[[i]]$ScoresFDR$two.sided[x,])
}
names(metagene)<-names(results)
return(metagene)
}