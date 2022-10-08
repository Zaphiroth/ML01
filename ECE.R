ECE = function(LR.H1.exp, LR.H2.exp) {
  P.H1 = seq(from=0.01, to=0.99, by=0.01)
  P.H2 = 1 - P.H1
  a.priori.odds = P.H1/P.H2
  N.H1 = length(LR.H1.exp)
  N.H2 = length(LR.H2.exp)
  set = c("null","exp","cal")
  for (x in 1:length(set)) {
    if(x == 1) {
      LR.H1 = rep(1, times=N.H1)
      LR.H2 = rep(1, times=N.H2)
    }
    if(x == 2) {
      LR.H1 = as.matrix(LR.H1.exp)
      LR.H2 = as.matrix(LR.H2.exp)
      LR.H1[which(LR.H1 < 10^-20)] = 10^-20
      LR.H2[which(LR.H2 < 10^-20)] = 10^-20
      LR.H1[which(LR.H1 > 10^20)] = 10^20
      LR.H2[which(LR.H2 > 10^20)] = 10^20
    }
    if(x == 3) {
      LR.H1.H2.exp = c(LR.H1,LR.H2)
      indices = order(LR.H1.H2.exp)
      LR.H1.H2.exp.sorted = sort(LR.H1.H2.exp)
      posterior.prob = c(rep(1, times=N.H1), rep(0, times=N.H2))
      posterior.prob.sorted = posterior.prob[indices]
      posterior.H1.H2.cal = data.frame(gpava(LR.H1.H2.exp.sorted, posterior.prob.sorted)[1])
      LR.H1.H2.cal = posterior.H1.H2.cal/(1-posterior.H1.H2.cal)/(N.H1/N.H2)
      LR.H1 = LR.H1.H2.cal[which(indices %in% c(1:N.H1)),]
      LR.H2 = LR.H1.H2.cal[which(indices %in% (N.H1+1):(N.H1+N.H2)),]
      LR.H1[which(LR.H1 < 10^-20)] = 10^-20
      LR.H2[which(LR.H2 < 10^-20)] = 10^-20
      LR.H1[which(LR.H1 > 10^20)] = 10^20
      LR.H2[which(LR.H2 > 10^20)] = 10^20
    }
    penalty.H1 = 0
    penalty.H2 = 0
    for (i in 1:N.H1) {
      a = -log2(LR.H1[i]*a.priori.odds/(1+LR.H1[i]*a.priori.odds))
      penalty.H1 = penalty.H1 + a
    }
    for (j in 1:N.H2) {
      b = log2(1 + LR.H2[j]*a.priori.odds)
      penalty.H2 = penalty.H2 + b
    }
    ECE= P.H1/N.H1*penalty.H1 + P.H2/N.H2*penalty.H2
    col = c("black","red","blue")
    lty = c(3,1,2)
    if (x %in% c(2,3)) {par(new=TRUE)}
    plot(log10(a.priori.odds), ECE, xlim=c(-2,2), ylim=c(0,1), type="l", col
         =col[x], lty=lty[x], xlab = expression(paste("prior log"[10],"(Odds)")))
    abline(v=0, lty=2, col="gray")
    legend("topleft", c("null method with LR=1", "experimental LR", "calibrated LR after PAV"), 
           col=col, lty=lty, bty="n", cex=0.5)
  }
}