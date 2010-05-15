#Vyznamnost vzorov
plotpattern<-function(intx, method)
{
if (method==1) {
 matplot(t(intx)[5:8,],type="l",ylab="pvalue",xlab="pattern ID", 
 lwd=3, lty=c(1,2,3,5))
 legend(x="topleft",colnames(intx)[5:8],lty=c(1,2,3,5),
 col=1:4,lwd=2)
 abline(h=0.05,col="red")}
if (method==2) 
{
#lay=matrix(c(1:5), ncol=1)
#layout(lay)
#layout.show(3)
#par(layout(matrix(c(1:5), ncol=1)),mar=c(0,4,0,0), oma=c(0,0,0,0))
par(mfrow=c(5,1),mar=c(0,4,0,0), oma=c(0,0,0,0))
for (i in 5:7) {
plot(1,type="n", axes=F, xlab="", ylab=names(intx[i]),xlim=c(0,max(intx[,5:8])))
for (j in 1:nrow(intx)) {
    points(x=intx[j,i],y=1, pch = 19, cex=2, col = j )
    abline(h=1,col="black")
    }
}
  plot(1,type="n", yaxt="n", frame.plot=FALSE,xlab="", ylab=names(intx[i]),xlim=c(0,max(intx[,5:8])))
  for (j in 1:nrow(intx)) { points(x=intx[j,8],y=1, pch = 19, cex=2, col = j )}
  abline(h=1,col="black")
#legend

plot(1, type="n", axes=F, xlab="", ylab="")
legend(x="center", legend=c(as.character(intx[,1])), col=1:nrow(intx), pch=19, horiz=TRUE, bty="n",title="Pattern")
}
}
########

