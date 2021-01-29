
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)



# Define UI for application that draws a histogram
ui <- fluidPage(sidebarLayout( sidebarPanel(
    
    selectInput("SelectProb", "Select probability formula", choices = c("P(x<=a)", "P(x>=b)", "P(a<=x<=b)")),
    sliderInput("a",
                "a:",
                step = 1,
                min = 0,
                max = 1,
                value = 0),
    sliderInput("b",
                "b:",
                step = 1,
                min = 0,
                max = 1,
                value = 1)
),
mainPanel(tabsetPanel(id = "tabs",
                      tabPanel("1",
                               sliderInput("prob",
                                           "Probabilitate:",
                                           min = 0.1,
                                           max = 1,
                                           value = 0.33)
                      ),
                      tabPanel("2"),
                      tabPanel("3",
                               sliderInput("lmb3",
                                           "Lambda:",
                                           min = 0.1,
                                           max = 1,
                                           value = 1)
                      ),
                      tabPanel("4",
                               sliderInput("prob",
                                           "Probability:",
                                           min = 0.1,
                                           max = 1,
                                           value = 0.33),
                               numericInput("NrAruncari",
                                            "Numar aruncari",
                                            min = 1,
                                            max = 1000,
                                            value = 300)
                      ),
                      tabPanel("5",
                               sliderInput("prob_infectare",
                                           "Probability:",
                                           min = 0.001,
                                           max = 1,
                                           value = 0.001),
                               numericInput("NrInfectati",
                                            "Numar infectati",
                                            min = 10,
                                            max = 5000,
                                            value = 1000)
                      ),
                      tabPanel("6",
                               sliderInput("ProbBit",
                                           "Probabilitate:",
                                           min = 0.1,
                                           max = 1,
                                           value = 0.1),
                               numericInput("NrIncercari",
                                            "Numar incercari",
                                            min = 1,
                                            max = 1000,
                                            value = 15),
                               numericInput("NrBiti",
                                            "Numar biti",
                                            min = 1,
                                            value = 10)
                      ),
                      tabPanel("7",
                               sliderInput("Mean",
                                           "Mean:",
                                           min = 90,
                                           max = 110,
                                           value = 100),
                               sliderInput("StDev",
                                           "Standard Deviation",
                                           min = 10,
                                           max = 20,
                                           value = 15)
                      ),
                      tabPanel("8",
                               sliderInput("exp_sales",
                                           "Expected Sales:",
                                           min =1,
                                           max = 10,
                                           value = 3),
                               numericInput("events",
                                            "NO sales",
                                            min = 1,
                                            max = 100,
                                            value = 10)
                      ),
                      tabPanel("9",
                               sliderInput("Time",
                                           "Average time spent per visit:",
                                           min = 0,
                                           max = 20,
                                           value = 5)
                      )
),
plotOutput("fctMasa"),
plotOutput("fctRep"),
plotOutput("fctProb"),
textOutput("valueProb")
)
)

#navbarPage("Navbar!",
#),

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$tabs,{
        
        observeEvent(input$a,
                     {
                         updateSliderInput(session = session, "a", max = input$b)
                         updateSliderInput(session = session, "b", min = input$a)
                     })
        observeEvent(input$b,
                     {
                         updateSliderInput(session = session, "a", max = input$b)
                         updateSliderInput(session = session, "b", min = input$a)
                     })
        
        if(input$tabs==1){
            FctMasa = function(p){
                plot(c(1,0),c(p,1-p), lwd=5)
                segments(c(1,0), 0, c(1,0), c(p,1-p), col="red")
            }
            
            # Pp ca avem un experiment aleator. ne interesam la realizarea unui eveniment A. Csd ca sansa de realizare a lui A este p
            # 1 aruncam o moneda
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
            
            P = function(p, a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F(a, p))
                    }
                    else
                    {
                        return (1 - F(a, p))
                    }
                }
                else
                {
                    return (F(b, p) - F(a, p))
                }
            }
            
            output$fctMasa <- renderPlot({
                FctMasa(input$prob)
            })
            output$fctRep <- renderPlot({
                # generate bins based on input$bins from ui.R
                # x    <- faithful[, 2]
                # bins <- seq(min(x), max(x), length.out = input$bins + 1)
                
                # draw the histogram with the specified number of bins
                # hist(x, breaks = bins, col = 'darkgray', border = 'white')
                F = Vectorize(F, vectorize.args = "x")
                t = seq(-1,2,length.out = 1000)
                y = F(t,input$prob)
                plot(t, y, type= "l", col="red")
            })
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P(input$prob, input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P(input$prob, input$b, param = 1)+F(input$b, input$prob))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P(input$prob, input$a, input$b)+F(input$a, input$prob))
                    })
                }
            })
            output$fctProb <- renderPlot({
                #F = Vectorize(F, vectorize.args = "x")
                #t = seq(-1,2,length.out = 1000)
                #y = F(t,input$prob)
                #sunspotyear <- data.frame(
                #    Year     = as.numeric(t),
                #    Sunspots = as.numeric(y)
                #)
                
                #ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_ribbon(inherit.aes = F,colour="black", fill="blue", alpha=.2, aes(xmin=input$a, xmax=input$b))
                F = Vectorize(F, vectorize.args = "x")
                t = seq(-1,2,length.out = 1000)
                y = F(t,input$prob)
                plot(t, y, type= "l", col="red")
                
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(0,input$a)
                    y=F(x, input$prob)
                    #lines(x, y, col="blue")
                    
                    for(i in 1:length(x)){
                        tmp = 0
                        if(i==1)
                            tmp = 0
                        else
                            tmp = y[i-1]
                        lines(c(x[i],x[i]), c(tmp,y[i]), col="blue")
                    }
                    #polygon(c(0,x,input$a), c(0,y,0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b,2)
                    y = F(x,input$prob)
                    for(i in 1:length(x)){
                        tmp = 0
                        if(i==1)
                            tmp = 0
                        else
                            tmp = y[i-1]
                        lines(c(x[i],x[i]), c(tmp,y[i]), col="blue")
                    }
                }else{
                    x = seq(input$a,input$b)
                    y = F(x,input$prob)
                    for(i in 1:length(x)){
                        tmp = 0
                        if(i==1)
                            tmp = 0
                        else
                            tmp = y[i-1]
                        lines(c(x[i],x[i]), c(tmp,y[i]), col="blue")
                    }
                }
                
            })
        }else if(input$tabs==2){
            # 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            # V.a. continua
            fd2 = function(x){
                return (exp(x)/(1 + exp(x))^2)
            }
            F2 = function(x){
                return (integrate(fd2,lower = -Inf, upper = x)$value)
            }
            F2 = Vectorize(F2, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = fd2(x)
                plot(x, y, type= "l", col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = F2(x) 
                plot(x, y, type= "l", col="red")
            })
            output$fctProb <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = F2(x) 
                plot(x, y, type= "l", col="red")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(-10,input$a,length.out = 1000)
                    y = F2(x)
                    polygon(c(-10,x,input$a), c(-10,y,0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b,10,length.out = 1000)
                    y = F2(x)
                    polygon(c(input$b,x,10), c(0,y,0), col="light blue")
                }else{
                    x = seq(input$a,input$b,length.out = 1000)
                    y = F2(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
            })
            P2 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F2(a))
                    }
                    else
                    {
                        return (1 - F2(a))
                    }
                }
                else
                {
                    return (F2(b) - F2(a))
                }
            }
            
            #observeEvent(input$a,  {
            #    updateSliderInput(session = session, "b2", min = input$a2)
            #})
            #
            #observeEvent(input$b,  {
            #    updateSliderInput(session = session, "a2", max = input$b2)
            #})
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P2(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P2(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P2( input$a, input$b))
                    })
                }
            })
            
        }
        else if (input$tabs==3){
            # 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # x>0
            fd3 = function(x){
                return (input$lmb3*exp(-input$lmb3*x))
            }
            F3 = function(x){
                return (integrate(fd3,lower = 0, upper = x)$value)
            }
            F3 = Vectorize(F3, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = fd3(x)
                plot(x, y, type= "l", col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = F3(x) 
                plot(x, y, type= "l", col="red")
            })
            output$fctProb <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = F3(x)
                mini = min(y)
                plot(x, y, type= "l", col="red")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(-10,input$a,length.out = 1000)
                    y = F3(x)
                    polygon(c(x, input$a), c(y, mini), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b,10,length.out = 1000)
                    y = F3(x)
                    polygon(c(input$b,x, 10), c(mini,y, mini), col="light blue")
                }else{
                    x = seq(input$a,input$b,length.out = 1000)
                    y = F3(x)
                    polygon(c(input$a,x,input$b), c(mini,y,mini), col="light blue")
                }
            })
            P3 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F3(a))
                    }
                    else
                    {
                        return (1 - F3 (a))
                    }
                }
                else
                {
                    return (F3(b) - F3(a))
                }
            }
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P3(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P3(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P3(input$a, input$b))
                    })
                }
            })
            
        }else if(input$tabs==4){
            # 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            #  Suppose a baseball player has a p= .300 batting average. What is the probability of X<=150 hits in n=500 at bats? X=150? X>150?
            
            
            updateSliderInput(session = session, "a", max = 300)
            updateSliderInput(session = session, "b", max = 300)
            
            fd4 = function(x)
            {
                return(dpois(x = hits, input$prob*input$NrAruncari , log = FALSE))
            }
            
            F4 = function(xx)
            {
                return (ppois(q = xx, lambda = input$NrAruncari*input$prob, lower.tail = TRUE))
            }
            #    F4 = Vectorize(F4,vectorize.args = "x")
            
            output$fctMasa <- renderPlot({
                hits <- 0:input$NrAruncari 
                density <- dpois(x = hits, lambda = input$prob * input$NrAruncari)
                plot (x = hits,y=density,type="l")
            })
            
            output$fctRep <- renderPlot({
                hits <- 0:input$NrAruncari 
                prob <- ppois(q = hits, lambda = input$prob * input$NrAruncari, lower.tail = TRUE)
                plot (x = hits,y=prob,type="l")
            })
            
            
            output$fctProb <- renderPlot({
                x = 0:input$NrAruncari
                y = ppois(q = x, lambda = input$prob * input$NrAruncari, lower.tail = TRUE)
                
                plot(x, y, type= "l", col="red")
                
                if(input$SelectProb=="P(x<=a)"){
                    polygon(c(input$a,x[x<=input$a]), c(0,y[x<=input$a]), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    polygon(c(input$b,x[input$b<=x],input$NrAruncari),c(0,y[input$b<=x],0), col="light blue")
                }else{
                    x = seq(input$a,input$b)
                    y = F4(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
            })
            
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
        }else if(input$tabs==5)
        {
            
            # Suppose the probability that a drug produces a certain side effect is p = = 0.1% and n = 1,000 patients in a clinical trial receive the drug.
            # What is the probability 0 people experience the side effect?
            
            observeEvent(input$NrInfectati,
                         {
                             updateSliderInput(session = session, "a", max = 10)
                             updateSliderInput(session = session, "b", max = 10)
                         })
            
            fd5 = function(x)
            {
                return(dpois(x = x, lambda = input$NrInfectati * input$prob_infectare))
            }
            
            F5 = function(xx)
            {
                return (ppois(q = xx, lambda = input$NrInfectati * input$prob_infectare, lower.tail = TRUE))
            }
            
            output$fctMasa <- renderPlot({
                x <- 0:10
                density <- dpois(x = x, lambda = input$NrInfectati * input$prob_infectare)
                plot (x = x,y=density,type="l")
            })
            
            output$fctRep <- renderPlot({
                x <- 0:10
                prob <- ppois(q = x, lambda = input$NrInfectati * input$prob_infectare, lower.tail = TRUE)
                plot (x = x,y=prob,type="l")
            })
            
            output$fctProb <- renderPlot({
                x = 0:10
                y = ppois(q = x, lambda = input$prob_infectare * input$NrInfectati, lower.tail = TRUE)
                
                plot(x, y, type= "l", col="red")
                
                if(input$SelectProb=="P(x<=a)"){
                    polygon(c(input$a,x[x<=input$a]), c(0,y[x<=input$a]), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    polygon(c(input$b,x[input$b<=x],input$NrInfectati),c(0,y[input$b<=x],0), col="light blue")
                }else{
                    x = seq(input$a,input$b)
                    y = F5(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                    
                }
            })
            P5 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F5(a))
                    }
                    else
                    {
                        return (1 - F5 (a))
                    }
                }
                else
                {
                    return (F5(b) - F5(a))
                }
            }
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P5(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P5(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P5(input$a, input$b))
                    })
                }
            })
        }else if (input$tabs==6){
            # 6 G~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Fie un bit care este transmis oe un canal bruiat si are prob p sa fie transmis incorect.
            # Pt a imbunatati fiabilitatea comunicarii, este transmis de n ori, unde n impar
            # Pp ca avem un decodor care decide care bit e corect dupa nr majoritar de biti transmisi.
            # n si p dati ca parametru, x-nr biti transmisi cu eroare, X~B(n, p)
            
            updateSliderInput(session = session, "a", min=0, max = input$NrBiti)
            updateSliderInput(session = session, "b", min=0, max = input$NrBiti)
            fm = function(x){
                return (dbinom(x, size = input$NrIncercari, prob = input$ProbBit))
            }
            F = function(x){
                return (pbinom(x, size = input$NrIncercari, prob = input$ProbBit))
            }
            F = Vectorize(F, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(0, input$NrBiti, by=1)
                y = fm(x)
                plot(x, y, lwd=5)
                segments(x, 0, x, y, col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(0, input$NrBiti, length.out = 1000)
                y = F(x) 
                plot(x, y, type= "l", col="red")
            })
            output$fctProb <- renderPlot({
                x = seq(0, input$NrBiti, length.out = 1000)
                y = F(x)
                plot(x, y, type= "l", col="red")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(0, input$a)
                    y = F(x)
                    for(i in 1:length(x)){
                        tmp = 0
                        if(i==1)
                            tmp = 0
                        else
                            tmp = y[i-1]
                        lines(c(x[i],x[i]), c(tmp,y[i]), col="blue")
                    }
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b, input$NrBiti)
                    y = F(x)
                    for(i in 1:length(x)){
                        tmp = 0
                        if(i==1)
                            tmp = 0
                        else
                            tmp = y[i-1]
                        lines(c(x[i],x[i]), c(tmp,y[i]), col="blue")
                    }
                }else{
                    x = seq(input$a, input$b)
                    y = F(x)
                    for(i in 1:length(x)){
                        tmp = 0
                        if(i==1)
                            tmp = 0
                        else
                            tmp = y[i-1]
                        lines(c(x[i],x[i]), c(tmp,y[i]), col="blue")
                    }
                }
            })
            P = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F(a))
                    }
                    else
                    {
                        return (1 - F(a))
                    }
                }
                else
                {
                    return (F(b) - F(a))
                }
            }
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P(input$b, param = 1)+fm(input$b))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P(input$a, input$b)+fm(input$a))
                    })
                }
            })
            
        }else if(input$tabs == 7){
            updateSliderInput(session = session, "a", min = 50, max = 140, value = 80)
            updateSliderInput(session = session, "b", min = 50, max = 140, value = 120)
            fd7 = function(x){
                return (dnorm(x, input$Mean, input$StDev))
            }
            F7 = function(x){
                return (integrate(fd7,lower = -Inf, upper = x)$value)
            }
            F7 = Vectorize(F7, vectorize.args = "x")
            x <- seq(-4, 4, length.out=100)*input$StDev + input$Mean
            output$fctMasa <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = fd7(x)
                plot(x, y, type= "l", col="red")
            }) 
            output$fctRep <- renderPlot({
                hx <- dnorm(x, input$Mean, input$StDev)
                plot(x, hx, type = "n", xlab = "IQ Values", ylab = "", main = "Normal Distribution")
                lines(x, hx)
            })
            output$fctProb <- renderPlot({
                hx <- dnorm(x, input$Mean, input$StDev)
                plot(x, hx, type = "n", xlab = "IQ Values", ylab = "", main = "Normal Distribution")
                if(input$SelectProb=="P(x<=a)"){
                    i <- x <= input$a
                    lines(x, hx)
                    polygon(c(input$a,x[i]), c(0,hx[i]), col="red")
                }else if(input$SelectProb=="P(x>=b)"){
                    i <- x >= input$b
                    lines(x, hx)
                    polygon(c(input$b,x[i]), c(0,hx[i]), col="red")
                }else{
                    i <- x >= input$a & x <= input$b
                    lines(x, hx)
                    polygon(c(input$a,x[i],input$b), c(0,hx[i],0), col="red")
                    
                }
            })
            
        }
        else if (input$tabs == 8)
        {
            
            # What is the probability of making 2 to 4 sales in a week if the average sales rate is 3 per week?
            
            observeEvent(input$events,
                         {
                             updateSliderInput(session = session, "a", max = input$events)
                             updateSliderInput(session = session, "b", max = input$events)
                             
                         })
            
            
            fd8 = function(x)
            {
                return(dpois(x = x, lambda = input$exp_sales))
            }
            
            F8 = function(xx)
            {
                return (ppois(q = xx, lambda = input$exp_sales, lower.tail = TRUE))
            }
            
            output$fctMasa <- renderPlot({
                events = 0:input$events
                density <- density <- dpois(x = events, lambda = input$exp_sales)
                plot (x = events,y=density,type="l")
            })
            
            output$fctRep <- renderPlot({
                events = 0:input$events
                prob <- ppois(q = events, lambda = input$exp_sales, lower.tail = TRUE)
                plot (x = events,y=prob,type="l")
            })
            
            output$fctProb <- renderPlot({
                x = 0:input$events
                y = ppois(q = x, lambda = input$exp_sales, lower.tail = TRUE)
                
                plot(x, y, type= "l", col="red")
                
                if(input$SelectProb=="P(x<=a)"){
                    polygon(c(input$a,x[x<=input$a]), c(0,y[x<=input$a]), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    polygon(c(input$b,x[input$b<=x],input$NrInfectati),c(0,y[input$b<=x],0), col="light blue")
                }else{
                    x = seq(input$a,input$b)
                    y = F8(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
            })
            
            P8 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F8(a))
                    }
                    else
                    {
                        return (1 - F8 (a))
                    }
                }
                else
                {
                    return (F8(b) - F8(a))
                }
            }
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P8(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P8(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P8(input$a, input$b))
                    })
                }
            })
            
            
        }
        else if(input$tabs == 9){
            #Calculating the probabilty of a visitor spending up to X minutes on a site
            updateSliderInput(session = session, "a", min = 0, max = 15, value = 2)
            updateSliderInput(session = session, "b", min = 0, max = 20, value = 5)
            #la = 1/input$Time
            x <- seq(0, 12/(1/input$Time), 0.01) 
            output$fctMasa <- renderPlot({
                plot(x, pexp(x, 1/input$Time), type = "l",
                     ylab = "F(x)", lwd = 2, col = "red")
            }) 
            output$fctRep <- renderPlot({
                y <- dexp(x, rate = 1/input$Time)
                plot(x, y, type = "n", ylab = "")
                lines(x,y)
            })
            output$fctProb <-renderPlot({
                y <- dexp(x, rate = 1/input$Time)
                plot(x, y, type = "n", ylab = "")
                if(input$SelectProb=="P(x<=a)"){
                    i <- x <= input$a
                    lines(x, y)
                    polygon(c(0,x[i],input$a), c(0,y[i],0), col="red")
                }else if(input$SelectProb=="P(x>=b)"){
                    i <- x >= input$b
                    lines(x, y)
                    polygon(c(input$b,x[i]), c(0,y[i]), col="red")
                    
                    #text(8, 0.12, format(pexp(10, rate = 0.2, lower.tail = FALSE),nsmall = 2), cex = 1.2)
                }else{
                    i <- x >= input$a & x <= input$b
                    lines(x, y)
                    polygon(c(input$a,x[i],input$b), c(0,y[i],0), col="red")
                    
                }
                
            })
        }
        # when water change, update air
        #observeEvent(input$a,  {
        #    updateSliderInput(session = session, "b", min = input$a)
        #})
        #
        ## when air change, update water
        #observeEvent(input$b,  {
        #    updateSliderInput(session = session, "a", max = input$b)
        #})
        
        
        
        # output$valueProb <- renderText({
        #     c("Probability: ", P(input$prob, input$a, input$b))
        # })
    })    
}

# Run the application 
shinyApp(ui = ui, server = server)