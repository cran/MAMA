dataSOGL<-function(data, group, groupname, annotation)
{
 pData<-data.frame(groupname=as.numeric(pData(data)[,group]))
 rownames(pData)<-sampleNames(data)
 metadata<-data.frame(labelDescription=
 groupname, row.names=groupname)
 phenoData<-new("AnnotatedDataFrame", data=pData, 
 varMetadata=metadata)
 dum<-new("ExpressionSet", exprs=exprs(data),
 phenoData=phenoData, experimentData=experimentData(data),
 annotation=annotation)
return(dum)
}
