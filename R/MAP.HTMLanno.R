MAP.HTMLanno<-function(resx, probs, package)
{
 require(annaffy)
 genes.loclink <- list()
 for ( i in 1:nrow(resx))
   genes.loclink[[i]] <- unique(getText(aafLocusLink(probs[[i]], 
 package)))
 for( i in 1:nrow(resx))
 {
 affynote <- aafTableAnn(probs[[i]],package)
 filename <- paste("Pattern",i,".html",sep="")
 title <- paste("common induced probs for pattern",rownames(resx)[i])
 saveHTML(affynote,filename=filename,title=title)
 }
}