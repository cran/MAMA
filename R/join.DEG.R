join.DEG<-function(...,genenames, type, cutoff)
{ 
args<-list(...)
N<-length(args)
if (N!=length(type)) stop ("Vector type has not correct lenght")
genelist<-list()

for (i in 1:N)
{
if (type[i]==1) {genelist[[i]]<-genenames[args[[i]]$Meta]}
if (type[i]==2) {}
if (type[i]==3) {genelist[[i]]<-rownames(args[[i]]$two.sided)[args[[i]]$two.sided[,
 8]<cutoff]}
if (type[i]==4) {genelist[[i]]<-args[[i]]$intersect}
if (type[i]==5) {genelist[[i]]<-unique(c(rownames(args[[i]]$Table1),rownames(
 args[[i]]$Table2)))}
if (type[i]==6) {genelist[[i]]<-rownames(args[[i]])[args[[i]]$Pvalue<cutoff]}
if (type[i]==7) {genelist[[i]]<-genenames[args[[i]]$index]}
if (type[i]==8) {genelist[[i]]<-unique(unlist(args[[i]]))}
}
return(genelist)
}