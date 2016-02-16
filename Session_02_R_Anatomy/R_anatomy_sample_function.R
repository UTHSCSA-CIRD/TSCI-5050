simdat <- function(xx=25,...){
  datecol<-sample(10000:20000,xx,rep=T)
  outcmcol<-sample(0:1,xx,rep=T)
  numvcol <- rnorm(xx)
  charvcol <- sample(LETTERS,xx,rep=T)
  out<-data.frame(date=datecol,outcome=outcmcol,numv=numvcol,charv=charvcol)
  browser()
}
