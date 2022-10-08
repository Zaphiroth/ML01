histograms = function(LR.H1.exp, LR.H2.exp) {
  log.LR.H1.exp = log10(LR.H1.exp)
  log.LR.H2.exp = log10(LR.H2.exp)
  log.LR.H1.exp[which(log.LR.H1.exp < -20)] = -20
  log.LR.H2.exp[which(log.LR.H2.exp < -20)] = -20
  log.LR.H1.exp[which(log.LR.H1.exp > 20)] = 20
  log.LR.H2.exp[which(log.LR.H2.exp > 20)] = 20
  min = min(log.LR.H1.exp,log.LR.H2.exp)
  max = max(log.LR.H1.exp,log.LR.H2.exp)
  breaks = 10
  stepbins = (max-min)/breaks
  xbars = seq(min, max, by=stepbins)
  log.LR.H1.exp = log.LR.H1.exp[(log.LR.H1.exp < (xbars[length(xbars)]-
                                                    stepbins/2)) & log.LR.H1.exp > (xbars[1]-stepbins/2)]
  log.LR.H2.exp = log.LR.H2.exp[(log.LR.H2.exp < (xbars[length(xbars)]-
                                                    stepbins/2)) & log.LR.H2.exp > (xbars[1]-stepbins/2)]
  set = par(mfrow=c(2,1), mar=c(4,4,1,2))
  hist(log.LR.H1.exp, breaks=xbars-stepbins/2, col="blue", main="", xlab="logLR")
  legend("topleft", expression(paste(H[1]," true")), fill="blue")
  hist(log.LR.H2.exp, breaks=xbars-stepbins/2, col="darkorange", main="", xlab="logLR")
  legend("topleft", expression(paste(H[2]," true")), fill="darkorange")
  par(set)
}