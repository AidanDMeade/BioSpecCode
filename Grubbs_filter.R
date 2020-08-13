Grubbs_filter<-function(Data,Numcomps,pvalue){
  library(outliers)
  grubbs.flag <- function(x) {
    outliers <- NULL

    test <- x
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
    while(pv < pvalue) {
      outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
      test <- x[!x %in% outliers]
      grubbs.result <- grubbs.test(test)
      pv <- grubbs.result$p.value
    }
    return(data.frame(X=x,Outlier=(x %in% outliers)))
  }
  
  data.pca <- prcomp(t(Data), retx=TRUE ,center = TRUE,scale = FALSE)
  ol<-NULL
  for(i in 1:Numcomps){
    temp=round(data.pca$x[,i],digits=5)
    grubbs.result=grubbs.flag(temp)
    ol=c(ol,which(grubbs.result$Outlier==TRUE))
  }
  return(ol)
}