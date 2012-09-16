join.DEG<-function(...,genenames, type = NULL, cutoff)
{ 
args<-list(...)
N<-length(args)
if (!(is.null(type)) & N!=length(type)) stop ("Vector type has not correct lenght")
genelist<-list()

if (is.null(type))  {
for (i in 1:N) {
if ("metaMA.res" %in% class(args[[i]])  ) genelist[[i]]<-genenames[args[[i]]$Meta]
if ("ES.GeneMeta.res" %in% class(args[[i]]) ) genelist[[i]]<-rownames(args[[i]]$ScoresFDR$two.sided)[args[[i]]$ScoresFDR$two.sided[, 8]<cutoff]
if ("RankProduct.res" %in% class(args[[i]]) ) genelist[[i]]<-unique(c(rownames(args[[i]]$Table1),rownames(
 args[[i]]$Table2)))
if ("tsp.res" %in% class(args[[i]]) ) genelist[[i]]<-genenames[args[[i]]$tsp$index[1,]]
if ( "SOGLresult" %in% class(args[[i]]) ) genelist[[i]]<- args[[i]]$genes
if ("posterior.mean" %in% class(args[[i]]) ) genelist[[i]]<-rownames(args[[i]])[args[[i]]$Pvalue<cutoff & !(is.nan(args[[i]]$Pvalue))]
if ("MAP.Matches.res" %in% class(args[[i]]) ) genelist[[i]]<-unique(unlist(args[[i]]$genes))
}
} else {
for (i in 1:N)
{
if (type[i]==1 ) {genelist[[i]]<-genenames[args[[i]]$Meta]}
if (type[i]==2) {}
if (type[i]==3) {genelist[[i]]<-rownames(args[[i]]$two.sided)[args[[i]]$two.sided[,
 8]<cutoff]}
if (type[i]==4 ) {genelist[[i]]<-args[[i]]$genes}
if (type[i]==5 ) {genelist[[i]]<-unique(c(rownames(args[[i]]$Table1),rownames(
 args[[i]]$Table2)))}
if (type[i]==6) {genelist[[i]]<-rownames(args[[i]])[args[[i]]$Pvalue<cutoff]}
if (type[i]==7) {genelist[[i]]<-genenames[args[[i]]$index[1,]]}
if (type[i]==8) {genelist[[i]]<-unique(unlist(args[[i]]))}
}
}
return(genelist)
}