plotgene<-function(gene, datalabels=c("denmark", "australia", "japan", "combined"))
{
gen=gene
laymat<-matrix(c(1,2,2,3,3),ncol=1)
lay<-layout(laymat)
#layout.show(lay)

dum1<-c(gen$SOGL.z, gen$SOGL.fc, gen$SOGL.t, gen$MAP)
dum.nam<-c("SOGL.z", "SOGL.fc", "SOGL.t", paste("MAP",names(gen$MAP)))
par(mar=c(2,6,4,10))
pos<-1:(length(names(dum1)))
image(pos,1,as.matrix(dum1),
col=c("grey75", "grey25"), xlab="", ylab="", 
axes=F, xlim = 0.5 + c(0, length(names(dum1))))
axis(3, at=pos, labels = dum.nam, las = 2, line = -0.5, tick = 0, 
        cex.axis = 0.7)

##
#gen$output.Fisher
#x<-unlist(c(log(gen$output.Fisher[2]), log(gen$output.Fisher[4]), log(gen$z.stat[2]), log(gen$RankProd.down[4])))

x<-unlist(c(gen$output.Fisher[2], gen$output.Fisher[4], gen$z.stat[2], gen$RankProd.down[4], gen$METRADISC))

par(mar=c(5,6,2,10))
plot(x=x,y=1:length(x),pch=19,xlab="p-value", ylab="",ylim=0.5+c(0,length(x)), 
 xaxt="s", yaxt="n",frame.plot=TRUE)
axis(2, at=c(1:length(x)), labels = c("F.stat.up","F.stat.down","z.stat","RankProd.Down", names(gen$METRADISC)), las = 2, line = -0.5, tick = 0, 
        cex.axis = 0.8)
sta<-character(length(x))
sta[1]<-paste("F.stat.up:",round(gen$output.Fisher[1],2))
sta[2]<-paste("F.stat.down:",round(gen$output.Fisher[3],2))
sta[3]<-paste("z.stat:",round(gen$z.stat[1],2))
sta[4]<-paste("RankProd.down:",round(gen$RankProd.down[2],2))
sta[c(5,6)]<-paste("Average Rank:",round(gen$RQ[1],2))
sta[c(7,8)]<-paste("Heterogenity:",round(gen$RQ[2],2))
axis(4, at=c(1:length(x)), 
 labels = sta, las = 2, line = -0.5, tick = 0, cex.axis = 0.8)

axis(1, at=1, labels = , las = 2, line = -0.5, tick = 0, 
        cex.axis = 0.7)
abline(v=0.05,lty=3)

#gen$theScores
par(mar=c(5,6,2,10))
me<-gen$theScores[c(11:13,5)]
va<-gen$theScores[c(14:16,6)]
plot(x=me,y=1:4,pch=19,xlab="Effect Size", ylab="",ylim=0.5+c(0,4),
 xlim=c(min(me-va), max(me+va)), xaxt="s", yaxt="n",frame.plot=TRUE)
for (i in 1:4) lines(x=c(me[i]-va[i],me[i]+va[i]),y=c(i,i))
axis(2, at=c(1:4), 
 labels = datalabels, las = 2, line = -0.5, tick = 0, cex.axis = 0.8)

fd<-paste("FDR.twosided:",round(gen$ScoresFDR.twosided[c(2,4,6,8)],3))
axis(4, at=c(1:4), labels=fd, las = 2, line = -0.5, tick = 0, cex.axis = 0.8)
}