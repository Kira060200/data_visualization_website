# Aruncam in mod repetat o moneda pt care P(H) = p. Fie X - nr de succese realizate inainte de al 2-lea esec. Vrem sa determinam P(x=k)
fct_masa = function(){
  
}

# Pp ca avem un experiment aleator. ne interesam la realizarea unui eveniment A. Csd ca sansa de realizare a lui A este p
# 1 aruncam o moneda
FctMasa = function(p){
  plot(c(1,0),c(p,1-p), lwd=5)
  segments(c(1,0), 0, c(1,0), c(p,1-p), col="red")
}
FctMasa(2/3)

FctRep = function(p){
  #plot(c(-0.0002,1,0),c(0,p,1-p), lwd=5)
  plot(c(0,1),c(1-p,1), lwd=5, xlim = c(-0.5,1.5), ylim = c(0,1))
  segments(c(0,1), 0, c(0,1), c(1-p,1), lty="dashed")
  segments(c(-0.6,0,1), c(0, 1-p, 1), c(0, 1, 2), c(0, 1-p, 1), col = "red")
  # segments(0, 1-p, 1, 1-p, col = "red")
  # segments(1, 1, 2, 1, col = "red")
}
FctRep(2/3)

F = function(x,p){
  if (x<0){
    y=0
  }else if (x<1){
    y = 1-p
  }else{
    y=1
  }
  return(y)
}

F = Vectorize(F, vectorize.args = "x")

t = seq(-1,2,length.out = 1000)
y = F(t,2/3)

plot(t, y, type= "l", col="red")

P = function(p, a, b=NULL, param=NULL)
{
  if(is.null(b))
  {
    if(is.null(param))
    {
      F(a, p)
    }
    else
    {
      1 - F(a, p)
    }
  }
  else
  {
    F(b, p) - F(a, p)
  }
}
P(2/3,0,1)
# P = Vectorize(P(2/3,0,1), vectorize.args = "x")

F = Vectorize(F, vectorize.args = "x")
t = seq(-1,2,length.out = 1000)
y = F(t,1/3)
plot(t, y, type= "l", col="red")
x = seq(0,1,length.out = 1000)
y = F(x,1/3)
polygon(c(0,x,1),c(0,y,0), col="red")

# Define the Mean and Stdev
mean=1152
sd=84

# Create x and y to be plotted
# x is a sequence of numbers shifted to the mean with the width of sd.  
# The sequence x includes enough values to show +/-3.5 standard deviations in the data set.
# y is a normal distribution for x
x <- seq(-3.5,3.5,length=100)*sd + mean
y <- dnorm(x,mean,sd)

plot(x, y, type="l")
polygon(c(x[x>=1250], max(x), 1250), c(y[x>=1250], 0, 0), col="red")
    fd2 = function(x){
        return (exp(x)/(1 + exp(x))^2)
    }
    F2 = function(x){
        return (integrate(fd2,lower = -Inf, upper = x)$value)
    }
    F2 = Vectorize(F2, vectorize.args = "x")
        x = seq(-10, 10, length.out = 10)
        y = F2(x) 
        plot(x, y, type= "l", col="red")
<<<<<<< Updated upstream
=======

    lambda = 5
    fd3 = function(x){
        return (lambda*exp(-lambda*x))
    }
    F3 = function(x){
        return (integrate(fd3, lower = 0, upper = x)$value)
    }
    
    F3 = Vectorize(F3)
        x = seq(-10, 10, length.out = 1000)
        y = F3(x) 
            
        
        
        fd2 = function(x){
          return (exp(x)/(1 + exp(x))^2)
        }
        F2 = function(x){
          return (integrate(fd2,lower = -Inf, upper = x)$value)
        }
        F2 = Vectorize(F2, vectorize.args = "x")
        
        
        x = seq(-10, 10, length.out = 1000)
        y = F2(x) 
        plot(x, y, type= "l", col="red")
        if(input$SelectProb2=="P(x<=a)"){
          x = seq(-10,input$a2,length.out = 1000)
          y = F2(x)
          polygon(c(-10,x,input$a2), c(-10,y,0), col="light blue")
        }else if(input$SelectProb2=="P(x>=b)"){
          x = seq(input$b2,10,length.out = 1000)
          y = F2(x)
          polygon(c(input$b2,x,10), c(0,y,0), col="light blue")
        }else{
          x = seq(input$a2,input$b2,length.out = 1000)
          y = F2(x)
          polygon(c(input$a2,x,input$b2), c(0,y,0), col="light blue")
        }
        
>>>>>>> Stashed changes
