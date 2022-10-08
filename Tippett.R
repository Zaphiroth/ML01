Tippett <- function(LR.H1.exp, LR.H2.exp) {
  log.LR.H1.exp = log10(LR.H1.exp)
  log.LR.H2.exp = log10(LR.H2.exp)
  log.LR.H1.exp[which(log.LR.H1.exp < -20)] = -20
  log.LR.H2.exp[which(log.LR.H2.exp < -20)] = -20
  log.LR.H1.exp[which(log.LR.H1.exp > 20)] = 20
  log.LR.H2.exp[which(log.LR.H2.exp > 20)] = 20
  min = min(log.LR.H1.exp,log.LR.H2.exp)
  max = max(log.LR.H1.exp,log.LR.H2.exp)
  x.range.data = c(min-1,log.LR.H1.exp,log.LR.H2.exp, max+1)
  x.range = x.range.data[order(x.range.data)]
  Tippett.2 = matrix(0, nrow = length(x.range), ncol = 1)
  Tippett.1 = matrix(0, nrow = length(x.range), ncol = 1)
  for (i in 1:length(x.range)) {
    Tippett.1[i] = length(which(log.LR.H1.exp > x.range[i]))/length(log.LR.H1.exp)*100
    Tippett.2[i] = length(which(log.LR.H2.exp > x.range[i]))/length(log.LR.H2.exp)*100
  }
  
  false.positives = round(length(which(log.LR.H2.exp > 0))/length(log.LR.H2.exp)*100,2)
  false.negatives = round(length(which(log.LR.H1.exp < 0))/length(log.LR.H1.exp)*100,2)
  plot(x.range, Tippett.2, type="s", xlab=expression(paste(log[10],"LR greater than")), 
       ylab="Proportion of cases [%]", xlim=c(-3,3), ylim=c(0,100), lty=3)
  par(new=TRUE)
  plot(x.range, Tippett.1, type="s", xlab=expression(paste(log[10],"LR greater than")), 
       ylab="Proportion of cases [%]", xlim=c(-3,3), ylim=c(0,100))
  legend("topright", c(expression(paste("true-",H[2]," LR values")),
                       expression(paste("true-",H[1]," LR values"))), 
         lty=c(3,1), bty="n", cex = 0.75)
  abline(v=0, col="gray", lty=4)
}