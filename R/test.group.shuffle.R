test.group.shuffle<-function(data,dataname,var=1,minSampleNum = 3, method = "t",B=1000)
{
statX<-NULL
 for ( i in 1:B)
 {
 cat(i)
stat <-  entitybuild2(expr.mat=exprs(data),ALLtype=
 pData(data)[,var], type=levels(pData(data)[,var]),
 minSampleNum = minSampleNum, method = method, random=TRUE)
 statX <- cbind(statX,stat)
}
 entity <- entitybuild2(expr.mat=exprs(data),ALLtype=
 pData(data)[,var], type=levels(pData(data)[,var]),
 minSampleNum = minSampleNum, method = method, random=FALSE)
 statX <- statX*(-1)
 entity$stat <- entity$stat*(-1)
 temp <- entity$entity[1]
 entity$entity[1] <- entity$entity[2]; entity$entity[2]<-temp
save(entity, statX,file=paste("random_stat_",dataname,".Rdata", sep=""),compress=T)
return(statX)
}
