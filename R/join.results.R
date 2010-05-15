join.results<-function(...,type , genenames)
{
args<-list(...)
N<-length(args)
if (N!=length(type)) stop ("Vector type has not correct length")
results<-list()

for (i in 1:N)
{
if (type[i]==1) {results[[i]]<-metalist.to.matrix(args[[i]],genenames)}
if (type[i]==2) {dum<-as.data.frame(genenames %in% args[[i]]$intersect)
 rownames(dum)<-genenames
 results[[i]]<-dum
names(results[[i]])<-"SOGL"}
if (type[i]==3) {results[[i]]<-rbind(args[[i]]$Table1, args[[i]]$Table2)}
if (type[i]==4) {results[[i]]<-probs.to.matrix(args[[i]], genenames)}
if (type[i]==5) {results[[i]]<-args[[i]]}
}
return(results)
}