meta.test <- function (x, varname, stat = "t") 
{
    N <- length(GEDM(x))
    #probes <- 
    tespval <- list()
    for (i in 1:N) {
      classlab <- as.numeric(clinical(x)[[i]][,grep(varname,names(clinical(x)[[i]]))]) - 1
      test <- mt.teststat(GEDM(x)[[i]], classlab, stat)
      pval <- 2 * pt(-abs(test), df = dim(GEDM(x)[[i]])[1] - 1)
      if (i == 1) {
        tespval$test = test
        tespval$p = pval
        } else {
          tespval$test = cbind(tespval$test, test)
          tespval$p = cbind(tespval$p, pval)
        }
    }
    rownames(tespval$test) <- rownames(GEDM(x)[[1]])
    rownames(tespval$p) <- rownames(GEDM(x)[[1]])
    colnames(tespval$test) <- datanames(x)
    colnames(tespval$p) <- datanames(x)
    if (stat=="t") return(tespval) else return(tespval$test)
}
