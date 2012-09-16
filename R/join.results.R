join.results<-function(...,type = NULL, genenames)
{
args<-list(...)
N<-length(args)
if (!(is.null(type)) & N!=length(type)) stop ("Vector type has not correct length")
results<-list()

if (is.null(type)) {
for (i in 1:N) {
if ("metaMA.res" %in% class(args[[i]]) ) results[[i]]<-metalist.to.matrix(args[[i]],genenames)
if ("ES.GeneMeta.res" %in% class(args[[i]]) ) results[[i]]<- args[[i]]
if ("RankProduct.res" %in% class(args[[i]]) ) results[[i]]<-rbind(args[[i]]$Table1, args[[i]]$Table2)
if ("TSP.res" %in% class(args[[i]]) ) results[[i]]<-args[[i]]$tsp
if ("SOGLresult" %in% class(args[[i]]) ) {dum<-as.data.frame(genenames %in% args[[i]]$genes)
 rownames(dum)<-genenames
 results[[i]]<-dum}
if ("posterior.mean"  %in% class(args[[i]]) ) results[[i]]<-args[[i]]
if ("MAP.Matches.res" %in% class(args[[i]]) ) results[[i]]<-probs.to.matrix(args[[i]]$genes, genenames)
if ("METRADISC.res"   %in% class(args[[i]]) ) results[[i]]<-args[[i]]
}
} else {
for (i in 1:N)
{
if (type[i]==1) {results[[i]]<-metalist.to.matrix(args[[i]],genenames)}
if (type[i]==2) {dum<-as.data.frame(genenames %in% args[[i]]$genes)
 rownames(dum)<-genenames
 results[[i]]<-dum
names(results[[i]])<-"SOGL"}
if (type[i]==3) {results[[i]]<-rbind(args[[i]]$Table1, args[[i]]$Table2)}
if (type[i]==4) {results[[i]]<-probs.to.matrix(args[[i]], genenames)}
if (type[i]==5) {results[[i]]<-args[[i]]}
}
}
return(results)
}

