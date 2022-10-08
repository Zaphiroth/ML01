DET <- function(LR.H1.exp, LR.H2.exp) {
  log.LR.H1.exp = log10(LR.H1.exp)
  log.LR.H2.exp = log10(LR.H2.exp)
  log.LR.H1.exp[which(log.LR.H1.exp < -20)] = -20
  log.LR.H2.exp[which(log.LR.H2.exp < -20)] = -20
  log.LR.H1.exp[which(log.LR.H1.exp > 20)] = 20
  log.LR.H2.exp[which(log.LR.H2.exp > 20)] = 20
  min = min(log.LR.H1.exp,log.LR.H2.exp)
  max = max(log.LR.H1.exp,log.LR.H2.exp)
  treshold.range = c(min-1,log.LR.H1.exp,log.LR.H2.exp, max+1)
  treshold = treshold.range[order(treshold.range)]
  false.positives = matrix(0, nrow = length(treshold), ncol = 1)
  false.negatives = matrix(0, nrow = length(treshold), ncol = 1)
  for (i in 1:length(treshold)) {
    tmp.treshold = treshold[i]
    false.negatives[i] = length(which(log.LR.H1.exp <= tmp.treshold))/length(log.LR.H1.exp)*100
    false.positives[i] = length(which(log.LR.H2.exp > tmp.treshold))/length(log.LR.H2.exp)*100
  }
  x = qnorm(false.positives/100)
  y = qnorm(false.negatives/100)
  x[which(x == -Inf)] = qnorm(0.000001)
  y[which(y == -Inf)] = qnorm(0.000001)
  x[which(x == Inf)] = qnorm(0.999999)
  y[which(y == Inf)] = qnorm(0.999999)
  
  plot(x, y, type="S", xlab="false positives [%]", ylab="false negatives [%]", 
       xaxt="n", yaxt="n", xlim=c(qnorm(0.01),qnorm(0.9)), ylim=c(qnorm(0.01),qnorm(0.9)))
  axis.range = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  axis.gauss = qnorm(axis.range/100)
  abline(h=axis.gauss, lty=3, col="gray")
  abline(v=axis.gauss, lty=3, col="gray")
  axis(side=1,at=axis.gauss,labels=axis.range)
  axis(side=2,at=axis.gauss,labels=axis.range)
}