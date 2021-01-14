# Aruncam in mod repetat o moneda pt care P(H) = p. Fie X - nr de succese realizate inainte de al 2-lea esec. Vrem sa determinam P(x=k)
fct_masa = function(){
  
}

# Pp ca avem un experiment aleator. ne interesam la realizarea unui eveniment A. Csd ca sansa de realizare a lui A este p
# 1 aruncam o moneda
FctMasa = function(p){
  plot(c(1,0),c(p,1-p), lwd=5)
  segments(c(1,0), 0, c(1,0), c(p,1-p))
}
FctMasa(2/3)

FctRep = function(p){
  #plot(c(-0.0002,1,0),c(0,p,1-p), lwd=5)
  plot(c(1,0),c(p,1-p), lwd=5, xlim = c(-0.5,1.5))
  segments(c(1,0), 0, c(1,0), c(p,1-p), lty="dashed")
  segments(0, c(1-p,p), c(1,0), c(1-p,p), col = "red")
  segments(-2, 0, 0, 0, col = "red")
  segments(0, c(1-p,p), c(1,0), c(1-p,p), col = "red")
  # segments(0, 0, 0, max(p,1-p))
}
FctRep(2/3)

