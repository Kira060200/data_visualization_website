
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(ggplot2)
library(dplyr)



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
                      tabPanel("Home",
                               h1(textOutput("titlu")),
                               h2(textOutput("rep_discrete")),
                               h2(textOutput("rep_continue"))),
                      tabPanel("1",
                               sliderInput("prob",
                                           "Probabilitate:",
                                           min = 0.1,
                                           max = 1,
                                           value = 0.33)
                      ),
                      tabPanel("2"),
                      tabPanel("3",
                               numericInput("lmb3",
                                            "Lambda:",
                                            min = 0,
                                            value = 1)
                      ),
                      tabPanel("4",
                               sliderInput("prob2",
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
                                           min = 0.01,
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
                      ),tabPanel("7",
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
                      ),
                      tabPanel("10",
                               sliderInput("nr_test",
                                           "Number of tests",
                                           min = 1,
                                           max = 100,
                                           value = 10),
                               sliderInput("lim_inf",
                                           "Limita inferioara:",
                                           min = 1,
                                           max = 100,
                                           value = 1),
                               sliderInput("lim_sup",
                                           "Limita superioara:",
                                           min = 1,
                                           max = 100,
                                           value = 3)
                      ),
                      tabPanel("11"),
                      tabPanel("12",
                               sliderInput("interv",
                                           "Lungime interval:",
                                           min = 0,
                                           max = 100,
                                           value = 20),
                               sliderInput("aa",
                                           "Limita inferioara:",
                                           min = 0,
                                           max = 100,
                                           value = 20),
                               sliderInput("bb",
                                           "Limita superioara:",
                                           min = 0,
                                           max = 100,
                                           value = 20)
                      ),
                      tabPanel("13",
                               sliderInput("ex13_x",
                                           "X:",
                                           min = 1,
                                           max = 100,
                                           value = 14),
                               sliderInput("ex13_m",
                                           "M:",
                                           min = 1,
                                           max = 100,
                                           value = 70),
                               sliderInput("ex13_n",
                                           "N:",
                                           min = 1,
                                           max = 100,
                                           value = 30),
                               sliderInput("ex13_k",
                                           "K:",
                                           min = 1,
                                           max = 100,
                                           value = 20)
                      ),
                      tabPanel("14"),
                      tabPanel("15",
                               sliderInput("pr",
                                           "Probabilitate:",
                                           min = 0,
                                           max = 1,
                                           value = 0.2),
                               numericInput("k",
                                            "k:",
                                            min = 0,
                                            value = 10)
                      ),
                      tabPanel("16",
                               numericInput("lmb",
                                            "Lambda:",
                                            min = 0,
                                            value = 10),
                               numericInput("k2",
                                            "k:",
                                            min = 0,
                                            value = 25)
                      ),
                      tabPanel("17",
                               numericInput("pr2",
                                            "Probabilitate:",
                                            min = 0,
                                            max = 1,
                                            value = 0.2),
                               numericInput("k3",
                                            "k:",
                                            min = 1,
                                            value = 10),
                               numericInput("r",
                                            "r:",
                                            min = 1,
                                            value = 5)
                      ),
                      tabPanel("18",
                               numericInput("meanlog",
                                            "Mean on the log scale:",
                                            min = 0,
                                            max = 1,
                                            value = 1),
                               numericInput("sdlog",
                                            "Standard Deviation on the log scale:",
                                            min = 0,
                                            max = 1,
                                            value = 0.25)
                      ),
                      tabPanel("19",
                               numericInput("n",
                                            "n:",
                                            min = 0,
                                            value = 10),
                               numericInput("pr3",
                                            "probabilitate:",
                                            min = 0,
                                            value = 0.33)
                      ),
                      tabPanel("20",
                               sliderInput("alpha",
                                           "Number of money orders",
                                           min = 1,
                                           max = 10,
                                           value = 10),
                               sliderInput("theta",
                                           "On average, someone sends a money order once per y minutes:",
                                           min = 10,
                                           max = 60,
                                           value = 15)
                      ),
                      tabPanel("21",
                               numericInput("ex21_n",
                                            "N:",
                                            min = 0,
                                            max = 20,
                                            value = 13),
                               numericInput("ex21_p",
                                            "P:",
                                            min = 0,
                                            max = 1,
                                            value = 0.7)
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
        if(input$tabs=="Home"){
            output$fctMasa <- ({})
            output$fctRep <- ({})
            output$fctProb <- ({})
            output$valueProb = ({})
            output$rep_discrete <- renderText({"Repartitii discrete : 1, 6, 12, 15, 16, 17, 19"})
            output$rep_continue <- renderText({"Repartitii continue : 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 14, 18, 20, 21"})
            output$titlu <- renderText({"Pagina WEB pentru ilustrarea si calcularea de repartitii discrete si continue"})
        }else if(input$tabs==1){
            
            FctMasa = function(p){
                plot(c(1,0),c(p,1-p), lwd=5, main="Functie de masa")
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
                plot(t, y, type= "l", col="red", main = "Functie de repartitie")
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
                plot(t, y, type= "l", col="red", main = "Ilustrarea probabilitatii")
                
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
            updateSliderInput(session = session, "a", min = -10, max = 10)
            updateSliderInput(session = session, "b", min = -10, max = 10)            
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
                plot(x, y, type= "l", col="red", main = "Functie de densitate")
            })
            output$fctRep <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = F2(x) 
                plot(x, y, type= "l", col="red", main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = fd2(x) 
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(-10,input$a,length.out = 1000)
                    y = fd2(x)
                    polygon(c(-10,x,input$a), c(-10,y,0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b,10,length.out = 1000)
                    y = fd2(x)
                    polygon(c(input$b,x,10), c(0,y,0), col="light blue")
                }else{
                    x = seq(input$a,input$b,length.out = 1000)
                    y = fd2(x)
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
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
            updateSliderInput(session = session, "a", min = 0.01, max = 50)
            updateSliderInput(session = session, "b", min = 0.01, max = 50)
            fd3 = function(x){
                return (input$lmb3*exp(-input$lmb3*x))
            }
            F3 = function(x){
                return (integrate(fd3,lower = 0, upper = x)$value)
            }
            F3 = Vectorize(F3, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(0.01, 50, length.out = 1000)
                y = fd3(x)
                plot(x, y, type= "l", col="red", main = "Functie de densitate")
            })
            output$fctRep <- renderPlot({
                x = seq(0.01, 50, length.out = 1000)
                y = F3(x) 
                plot(x, y, type= "l", col="red", main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                x = seq(0.01, 50, length.out = 1000)
                y = fd3(x)
                #mini = min(y)
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(0.01,input$a,length.out = 1000)
                    y = fd3(x)
                    polygon(c(0, x, input$a), c(0,y,0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b,50,length.out = 1000)
                    y = fd3(x)
                    polygon(c(input$b, x), c(0, y), col="light blue")
                }else{
                    x = seq(input$a,input$b,length.out = 1000)
                    y = fd3(x)
                    polygon(c(input$a, x, input$b), c(0,y,0), col="light blue")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
        }else if(input$tabs==4){
            # 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            #  Suppose a baseball player has a p= .300 batting average. What is the probability of X<=150 hits in n=500 at bats? X=150? X>150?
            
            
            updateSliderInput(session = session, "a", max = 300)
            updateSliderInput(session = session, "b", max = 300)
            
            fd4 = function(x)
            {
                return(dpois(x, input$prob2*input$NrAruncari , log = FALSE))
            }
            
            F4 = function(xx)
            {
                return (ppois(q = xx, lambda = input$NrAruncari*input$prob2, lower.tail = TRUE))
            }
            #    F4 = Vectorize(F4,vectorize.args = "x")
            
            output$fctMasa <- renderPlot({
                hits <- 0:input$NrAruncari 
                density <- fd4(hits)
                plot (x = hits,y=density,type="l", main = "Functie de densitate")
            })
            
            output$fctRep <- renderPlot({
                hits <- 0:input$NrAruncari 
                prob <- F4(hits)
                plot (x = hits,y=prob,type="l", main = "Functie de repartitie")
            })
            
            
            output$fctProb <- renderPlot({
                x = 0:input$NrAruncari
                y = fd4(x)
                
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii")
                
                if(input$SelectProb=="P(x<=a)"){
                    polygon(c(0,x[x<=input$a],input$a), c(0,y[x<=input$a],0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    polygon(c(input$b,x[input$b<=x],input$NrAruncari),c(0,y[input$b<=x],0), col="light blue")
                }else{
                    x = seq(input$a,input$b)
                    y = fd4(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
            })
            
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            P4 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F4(a))
                    }
                    else
                    {
                        return (1 - F4 (a))
                    }
                }
                else
                {
                    return (F4(b) - F4(a))
                }
            }
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P4(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P4(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P4(input$a, input$b))
                    })
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
                density <- fd5(x)
                plot (x = x,y=density,type="l", main = "Functie de densitate")
            })
            
            output$fctRep <- renderPlot({
                x <- 0:10
                prob <- F5(x)
                plot (x = x,y=prob,type="l", main = "Functie de repartitie")
            })
            
            output$fctProb <- renderPlot({
                x = 0:10
                y = fd5(x)
                
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii")
                
                if(input$SelectProb=="P(x<=a)"){
                    polygon(c(0,x[x<=input$a],input$a), c(0,y[x<=input$a],0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    polygon(c(input$b,x[input$b<=x],input$NrInfectati),c(0,y[input$b<=x],0), col="light blue")
                }else{
                    x = seq(input$a,input$b)
                    y = fd5(x)
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
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
                plot(x, y, lwd=5, main="Functie de masa")
                segments(x, 0, x, y, col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(-1, input$NrBiti, length.out = 1000)
                y = F(x) 
                plot(x, y, type= "l", col="red", main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                x = seq(0, input$NrBiti, length.out = 1000)
                y = F(x)
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
        }else if(input$tabs == 7){
            updateSliderInput(session = session, "a", min = 50, max = 140, value = 80)
            updateSliderInput(session = session, "b", min = 50, max = 140, value = 120)
            fd7 = function(x){
                return (dnorm(x, input$Mean, input$StDev))
            }
            F7 = function(x){
                # return (integrate(fd7,lower = -Inf, upper = x)$value)
                return (pnorm(x, input$Mean, input$StDev))
            }
            F7 = Vectorize(F7, vectorize.args = "x")
            x <- seq(-4, 4, length.out=100)*input$StDev + input$Mean
            output$fctMasa <- renderPlot({
                hx <- dnorm(x, input$Mean, input$StDev)
                plot(x, hx, type = "n", xlab = "IQ Values", ylab = "", main = "Functie de densitate")
                lines(x, hx)
            }) 
            output$fctRep <- renderPlot({
                x = seq(-10, 10, length.out = 1000)
                y = F7(x)
                plot(x, y, type= "l", col="red", main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                hx <- dnorm(x, input$Mean, input$StDev)
                plot(x, hx, type = "n", xlab = "IQ Values", ylab = "", main = "Ilustrarea probabilitatii")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", pnorm(input$a, input$Mean, input$StDev))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", 1- pnorm(input$b, input$Mean, input$StDev))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", pnorm(input$b, input$Mean, input$StDev) - pnorm(input$a, input$Mean, input$StDev))
                    })
                }
            })
        }else if (input$tabs == 8){
            
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
                density <- fd8(events)
                plot (x = events,y=density,type="l", main = "Functie de densitate")
            })
            
            output$fctRep <- renderPlot({
                events = 0:input$events
                prob <- ppois(q = events, lambda = input$exp_sales, lower.tail = TRUE)
                plot (x = events,y=prob,type="l", main = "Functie de repartitie")
            })
            
            output$fctProb <- renderPlot({
                x = 0:input$events
                y = fd8(x)
                
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii")
                
                if(input$SelectProb=="P(x<=a)"){
                    polygon(c(0,x[x<=input$a],input$a), c(0,y[x<=input$a],0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    polygon(c(input$b,x[input$b<=x],input$NrInfectati),c(0,y[input$b<=x],0), col="light blue")
                }else{
                    x = seq(input$a,input$b)
                    y = fd8(x)
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            
        }else if(input$tabs == 9){
            #Calculating the probabilty of a visitor spending up to X minutes on a site
            updateSliderInput(session = session, "a", min = 0, max = 15, value = 2)
            updateSliderInput(session = session, "b", min = 0, max = 20, value = 5)
            #la = 1/input$Time
            x <- seq(0, 12/(1/input$Time), 0.01) 
            output$fctMasa <- renderPlot({
                y <- dexp(x, rate = 1/input$Time)
                plot(x, y, type = "n", ylab = "f(x)", main = "Functie de densitate")
                lines(x,y)
            }) 
            output$fctRep <- renderPlot({
                plot(x, pexp(x, 1/input$Time), type = "l",
                     ylab = "F(x)", lwd = 2, col = "red", main = "Functie de repartitie")
            })
            output$fctProb <-renderPlot({
                y <- dexp(x, rate = 1/input$Time)
                plot(x, y, type = "n", ylab = "", main = "Ilustrarea probabilitatii")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", pexp(input$a, 1/input$Time))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", 1 - pexp(input$b, 1/input$Time))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", pexp(input$b, 1/input$Time) - pexp(input$a, 1/input$Time))
                    })
                }
            })
        }else if (input$tabs == 10)
        {
            # continua uniforma
            # select N numbers between X and Y
            
            
            
            observeEvent(input$lim_sup,
                         {
                             updateSliderInput(session = session, "a", max = input$lim_sup)
                             updateSliderInput(session = session, "b", max = input$lim_sup)
                         })
            
            
            fd10 = function(x)
            {
                return(dunif(x = x, min=input$lim_inf,max=input$lim_sup))
            }
            
            F10 = function(xx)
            {
                return (punif(q = xx, min=input$lim_inf,max=input$lim_sup, lower.tail = TRUE))
            }
            
            
            output$fctMasa <- renderPlot({
                x = runif(input$nr_test, min = input$lim_inf, max = input$lim_sup)
                density <- fd10(x)
                plot (x = x,y=density,type="l", main = "Functie de densitate")
            })
            
            output$fctRep <- renderPlot({
                x = runif(input$nr_test, min = input$lim_inf, max = input$lim_sup)
                prob <- F10(x)
                plot (x = x,y=prob,type="l", main = "Functie de repartitie")
            })
            
            
            
            output$fctProb <-renderPlot({
                x = seq(input$lim_inf , input$lim_sup)
                y = fd10(x)
                mini = min(y)
                plot(x, y, type= "l", col="red", ylim = c(0,1), main = "Ilustrarea probabilitatii")
                if(input$SelectProb=="P(x<=a)"){
                    i <- x <= input$a
                    polygon(c(input$lim_inf,x[i],input$a), c(0,y[i],0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    i <- x >= input$b
                    polygon(c(input$b,x[i],input$lim_sup), c(0,y[i],0), col="light blue")
                }else{
                    
                    x = seq(input$a , input$b)
                    y = fd10(x)
                    
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
            })
            
            
            P10 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F10(a))
                    }
                    else
                    {
                        return (1 - F10 (a))
                    }
                }
                else
                {
                    return (F10(b) - F10(a))
                }
            }
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P10(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P10(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P10(input$a, input$b))
                    })
                }
            })
            
            
        }else if(input$tabs==11){
            set.seed(5)
            #Weight density curve for women
            updateSliderInput(session = session, "a", min = 40, max = 80, value = 50)
            updateSliderInput(session = session, "b", min = 50, max = 90, value = 60)
            
            df <- data.frame(
                sex=factor(rep(c("F", "M"), each=400)),
                weight=round(c(rnorm(400, mean=55, sd=5))
                ))
            
            f11 = function(x){
                return(dnorm(x=x, mean = 55, sd = 5))
            }
            
            F11 = function(xx){
                return(pnorm(q=xx, mean = 55, sd = 5))
            }
            
            P11 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F11(a))
                    }
                    else
                    {
                        return (1 - F11(a))
                    }
                }
                else
                {
                    return (F11(b) - F11(a))
                }
            }
            
            output$fctMasa <- renderPlot({
                ggplot(df, aes(x=weight)) + geom_density() + ggtitle("Functie de densitate")
            })
            output$fctRep <- renderPlot({
                ggplot(df, aes(x=weight)) + stat_ecdf(geom = "line") + ggtitle("Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                dat <- with(density(df$weight), data.frame(x, y))
                if(input$SelectProb=="P(x<=a)"){
                    ggplot(data = dat, mapping = aes(x = x, y = y)) +
                        geom_line()+
                        geom_area(mapping = aes(x = ifelse(x<=input$a, x, 0)), fill = "red") +
                        xlim(30, 80) + ggtitle ("Ilustrarea probabilitatii")
                }else if(input$SelectProb=="P(x>=b)"){
                    ggplot(data = dat, mapping = aes(x = x, y = y)) +
                        geom_line()+
                        geom_area(mapping = aes(x = ifelse(x>=input$b, x, 0)), fill = "red") +
                        xlim(30, 80) + ggtitle ("Ilustrarea probabilitatii")
                }else{
                    ggplot(data = dat, mapping = aes(x = x, y = y)) +
                        geom_line()+
                        geom_area(mapping = aes(x = ifelse(x>=input$a & x<=input$b, x, 0)), fill = "red") +
                        xlim(30, 80) + ggtitle ("Ilustrarea probabilitatii")
                }
            })
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P11(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P11(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P11(input$a, input$b))
                    })
                }
            })  
            
        }else if (input$tabs == 12){
            # 12 G~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # 
            
            updateSliderInput(session = session, "aa", min=0, max = input$interv)
            updateSliderInput(session = session, "bb", min=0, max = input$interv)
            fm = function(x){
                if(x>=input$aa && x<=input$bb)
                    return(1/(input$bb-input$aa+1))
                else 
                    return (0)
            }
            F = function(x){
                if(x>=input$aa && x<input$bb)
                    return((1-input$aa+floor(x))/(input$bb-input$aa+1))
                else if(x==input$bb)
                    return (1)
                else 
                    return (0)
            }
            F = Vectorize(F, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(0, input$interv, by=1)
                y=fm(0)
                for(i in 1:input$interv){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, ylim = c(0,1), main="Functie de masa")
                segments(x, 0, x, y, col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(-1, input$interv, length.out = 1000)
                y = F(x)
                
                plot(x, y, type= "l", col="red", ylim = c(0,1), main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                x = seq(0, input$interv, by=1)
                y=fm(0)
                for(i in 1:input$interv){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, ylim = c(0,1), main = "Ilustrarea probabilitatii")
                #segments(x, 0, x, y, col="red")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(0, input$a, by=1)
                    y=fm(0)
                    for(i in 1:input$interv){
                        y = c(y,fm(i))
                    }
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    x = seq(input$aa, input$a)
                    segments(x, 0, x, y, col="blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b, input$interv, by=1)
                    y=fm(0)
                    for(i in 1:input$interv){
                        y = c(y,fm(i))
                    }
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    x = seq(input$b, input$bb)
                    segments(x, 0, x, y, col="blue")
                }else{
                    x = seq(input$a, input$b, by=1)
                    y=fm(0)
                    for(i in 1:input$interv){
                        y = c(y,fm(i))
                    }
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    x = seq(input$a, input$b)
                    segments(x, 0, x, y, col="blue")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            observeEvent(input$aa,  {
                updateSliderInput(session = session, "bb", min = input$aa)
                updateSliderInput(session = session, "a", min = input$aa)
            })
            
            
            observeEvent(input$bb,  {
                updateSliderInput(session = session, "aa", max = input$bb)
                updateSliderInput(session = session, "b", max = input$bb)
                updateSliderInput(session = session, "a", max = min(input$bb, input$b))
            })
            
        }else if (input$tabs == 13)
        {
            # What is the probability of selecting x = 14 red marbles from a sample of k = 20 taken from an urn containing m = 70 red marbles
            # and n = 30 green marbles?
            
            
            updateSliderInput(session = session, "a", max = 100)
            updateSliderInput(session = session, "b", max = 100)
            
            
            observeEvent(input$ex13_x,
                         {
                             updateSliderInput(session = session, "ex13_n", min = input$ex13_x + input$ex13_m)  
                         })
            observeEvent(input$ex13_m,
                         {
                             updateSliderInput(session = session, "ex13_n", min = input$ex13_x + input$ex13_m)  
                         })
            
            
            
            fd13 = function(x)
            {
                return(dhyper(x = x, m = input$ex13_m, n = input$ex13_n, k = input$ex13_k))
            }
            
            F13 = function(xx)
            {
                return (phyper(q = xx, m = input$ex13_m, n = input$ex13_n, k = input$ex13_k, lower.tail = TRUE))
            }
            
            output$fctMasa <- renderPlot({
                x = seq(1:input$ex13_k)
                density <- fd13(x)
                plot (x = x,y=density,type="l", main = "Functie de densitate")
            })
            
            output$fctRep <- renderPlot({
                x = seq(1:input$ex13_k)
                prob <- F13(x)
                plot (x = x,y=prob,type="l", main = "Functie de repartitie")
            })
            
            output$fctProb <-renderPlot({
                x = seq(1:input$ex13_k)
                y = fd13(x)
                mini = min(y)
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii") # , ylim = c(0,1))
                if(input$SelectProb=="P(x<=a)"){
                    i <- x <= input$a
                    polygon(c(input$lim_inf,x[i],input$a), c(0,y[i],0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    i <- x >= input$b
                    polygon(c(input$b,x[i],input$lim_sup), c(0,y[i],0), col="light blue")
                }else{
                    x = seq(input$a , input$b)
                    y = fd13(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
            })
            
            P13 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F13(a))
                    }
                    else
                    {
                        return (1 - F13 (a))
                    }
                }
                else
                {
                    return (F13(b) - F13(a))
                }
            }
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P13(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P13(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P13(input$a, input$b))
                    })
                }
            })
            
        }else if(input$tabs==14){
            updateSliderInput(session = session, "a", min = 40, max = 300, value = 50)
            updateSliderInput(session = session, "b", min = 50, max = 400, value = 200)
            
            data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)
            
            dat <- with(density(data$price), data.frame(x, y))
            
            f14 = function(datt){
                return(datt)
            }
            
            output$fctMasa <- renderPlot({
                data %>%
                    filter( price<300) %>%
                    ggplot( aes(x=price)) +
                    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
                    ggtitle ("Functie de densitate")
            })
            output$fctRep <- renderPlot({
                ggplot(dat, aes(x=x)) + stat_ecdf(geom = "line") +
                    ggtitle("Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                dat <- with(density(data$price), data.frame(x, y))
                if(input$SelectProb=="P(x<=a)"){
                    data %>%
                        filter( price<=input$a) %>%
                        ggplot( aes(x=price)) +
                        geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
                        ggtitle("Ilustrarea probabilitatii")
                }else if(input$SelectProb=="P(x>=b)"){
                    data %>%
                        filter( price>=input$b & price <600) %>%
                        ggplot( aes(x=price)) +
                        geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
                        ggtitle("Ilustrarea probabilitatii")
                }else{
                    data %>%
                        filter( price>=input$a & price <= input$b) %>%
                        ggplot( aes(x=price)) +
                        geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
                        ggtitle("Ilustrarea probabilitatii")
                    
                }
            })
            F14 = function(xx){
                return(pnorm(q=xx, mean = 90, sd = 50))
            }
            P14 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F14(a))
                    }
                    else
                    {
                        return (1 - F14 (a))
                    }
                }
                else
                {
                    return (F14(b) - F14(a))
                }
            }
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P14(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P14(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P14(input$a, input$b))
                    })
                }
            })
            
        }else if (input$tabs == 15){
            # 15 G~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # 
            updateSliderInput(session = session, "a", min=0, max = input$k)
            updateSliderInput(session = session, "b", min=0, max = input$k)
            observeEvent(input$k,  {
                updateSliderInput(session = session, "a", min=0, max = input$k)
                updateSliderInput(session = session, "b", min=0, max = input$k)
            })
            fm = function(x){
                return(dgeom(x, prob = input$pr))
            }
            F = function(x){
                return(pgeom(x, prob = input$pr))
            }
            F = Vectorize(F, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(0, input$k, by=1)
                y=fm(0)
                for(i in 1:input$k){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, main="Functie de masa")
                segments(x, 0, x, y, col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(-1, input$k, length.out = 1000)
                y = F(x)
                #y=F(0)
                #for(i in 1:50){
                #    y = c(y,F(i))
                #}
                #print(y)
                plot(x, y, type= "l", col="red", main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                x = seq(0, input$k, by=1)
                y=fm(0)
                for(i in 1:input$k){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, main = "Ilustrarea probabilitatii")
                #segments(x, 0, x, y, col="red")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(0, input$a, by=1)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b, input$k, by=1)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
                }else{
                    x = seq(input$a, input$b)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            
        }else if (input$tabs == 16){
            # 16 G~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # 
            updateSliderInput(session = session, "a", min=0, max = input$k2)
            updateSliderInput(session = session, "b", min=0, max = input$k2)
            observeEvent(input$k2,  {
                updateSliderInput(session = session, "a", min=0, max = input$k2)
                updateSliderInput(session = session, "b", min=0, max = input$k2)
            })
            fm = function(x){
                return(dpois(x, lambda = input$lmb))
            }
            F = function(x){
                return(ppois(x, lambda = input$lmb))
            }
            F = Vectorize(F, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(0, input$k2, by=1)
                y=fm(0)
                for(i in 1:input$k2){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, main="Functie de masa")
                segments(x, 0, x, y, col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(-1, input$k2, length.out = 1000)
                y = F(x)
                #y=F(0)
                #for(i in 1:50){
                #    y = c(y,F(i))
                #}
                #print(y)
                plot(x, y, type= "l", col="red", main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                x = seq(0, input$k2, by=1)
                y=fm(0)
                for(i in 1:input$k2){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, main = "Ilustrarea probabilitatii")
                #segments(x, 0, x, y, col="red")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(0, input$a, by=1)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b, input$k2, by=1)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
                }else{
                    x = seq(input$a, input$b)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            
        }else if (input$tabs == 17){
            # 17 G~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # 
            
            updateSliderInput(session = session, "a", min=0, max = input$k3)
            updateSliderInput(session = session, "b", min=0, max = input$k3)
            observeEvent(input$k3,  {
                updateSliderInput(session = session, "a", min=0, max = input$k3)
                updateSliderInput(session = session, "b", min=0, max = input$k3)
            })
            fm = function(x){
                if(x>=input$r)
                    return(factorial(x-1)/(factorial(input$r-1)*factorial(x-input$r))*(1-input$pr2)^(x-input$r)*input$pr2^input$r)
                else
                    return (0)
            }
            F = function(x){
                if(x>=input$r)
                    return (fm(x)+F(x-1))
                else return (0)
            }
            F = Vectorize(F, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(0, input$k3, by=1)
                y=fm(0)
                for(i in 1:input$k3){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, main="Functie de masa")
                segments(x, 0, x, y, col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(-1, input$k3, length.out = 1000)
                y = F(x)
                plot(x, y, type= "l", col="red", ylim = c(0,1), main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                x = seq(0, input$k3, by=1)
                y=fm(0)
                for(i in 1:input$k3){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, main = "Ilustrarea probabilitatii")
                #segments(x, 0, x, y, col="red")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(0, input$a, by=1)
                    y=fm(0)
                    for(i in 1:input$a){
                        y = c(y,fm(i))
                    }
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    #x = seq(input$r, input$a)
                    
                    segments(x, 0, x, y, col="blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b, input$k3, by=1)
                    y=fm(input$b)
                    for(i in input$b+1:input$k3){
                        y = c(y,fm(i))
                    }
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    #x = seq(input$b, input$k3)
                    segments(x, 0, x, y, col="blue")
                }else{
                    x = seq(input$a, input$b, by=1)
                    y=fm(input$a)
                    for(i in input$a+1:input$b){
                        y = c(y,fm(i))
                    }
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    #x = seq(input$a, input$b)
                    segments(x, 0, x, y, col="blue")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            
        }
        else if(input$tabs == 18){
            
            updateSliderInput(session = session, "a", min=0, max = 5)
            updateSliderInput(session = session, "b", min=0, max = 5)
            
            f18 = function(x){
                return(dlnorm(x=x, meanlog = input$meanlog, sdlog = input$sdlog, log = FALSE))
            }
            
            F18 = function(xx){
                return(plnorm(q=xx, meanlog = input$meanlog, sdlog = input$sdlog, log = FALSE))
            }
            output$fctMasa <- renderPlot({
                x = seq(0:15)
                density <- f18(x)
                plot (x = x,y=density,type="l", main = "Functie de densitate")
            })
            
            output$fctRep <- renderPlot({
                x = seq(0:15)
                prob <- F18(x)
                plot (x = x,y=prob,type="l", main = "Functie de repartitie")
            })
            
            output$fctProb <-renderPlot({
                x = seq(0:15)
                y = f18(x)
                mini = min(y)
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii") 
                if(input$SelectProb=="P(x<=a)"){
                    i <- x <= input$a
                    polygon(c(input$lim_inf,x[i],input$a), c(0,y[i],0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    i <- x >= input$b
                    polygon(c(input$b,x[i],input$lim_sup), c(0,y[i],0), col="light blue")
                }else{
                    x = seq(input$a , input$b)
                    y = f18(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
            })
            
            P18 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F18(a))
                    }
                    else
                    {
                        return (1 - F18 (a))
                    }
                }
                else
                {
                    return (F18(b) - F18(a))
                }
            }
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P18(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P18(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P18(input$a, input$b))
                    })
                }
            })           
        }else if(input$tabs == 19){
            # 19 G~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Aproximarea binomiala prin Poisson
            updateSliderInput(session = session, "a", min=0, max = input$n)
            updateSliderInput(session = session, "b", min=0, max = input$n)
            observeEvent(input$n,  {
                updateSliderInput(session = session, "a", min=0, max = input$n)
                updateSliderInput(session = session, "b", min=0, max = input$n)
            })
            lmbd = input$n*input$pr3
            fm = function(x){
                return(dpois(x, lambda = lmbd))
            }
            F = function(x){
                return(ppois(x, lambda = lmbd))
            }
            F = Vectorize(F, vectorize.args = "x")
            output$fctMasa <- renderPlot({
                x = seq(0, input$n, by=1)
                y=fm(0)
                for(i in 1:input$n){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, main="Functie de masa")
                segments(x, 0, x, y, col="red")
            })
            output$fctRep <- renderPlot({
                x = seq(-1, input$n, length.out = 1000)
                y = F(x)
                #y=F(0)
                #for(i in 1:50){
                #    y = c(y,F(i))
                #}
                #print(y)
                plot(x, y, type= "l", col="red", main = "Functie de repartitie")
            })
            output$fctProb <- renderPlot({
                x = seq(0, input$n, by=1)
                y=fm(0)
                for(i in 1:input$n){
                    y = c(y,fm(i))
                }
                plot(x, y, lwd=5, main = "Ilustrarea probabilitatii")
                #segments(x, 0, x, y, col="red")
                if(input$SelectProb=="P(x<=a)"){
                    x = seq(0, input$a, by=1)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    x = seq(input$b, input$n, by=1)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
                }else{
                    x = seq(input$a, input$b)
                    y=fm(x)
                    #plot(x, y, lwd=5, ylim = c(0,1))
                    segments(x, 0, x, y, col="blue")
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
            observeEvent(input$a,  {
                updateSliderInput(session = session, "b", min = input$a)
            })
            
            
            observeEvent(input$b,  {
                updateSliderInput(session = session, "a", max = input$b)
            })
            
        }
        else if(input$tabs == 20){
            updateSliderInput(session = session, "a", min=0, max = 150)
            updateSliderInput(session = session, "b", min=100, max = 300)
            
            
            
            f20 = function(x){
                return(dgamma(x = x, shape = input$alpha, scale = input$theta))
            }
            F20 = function(xx){
                return(pgamma(q = xx, shape = input$alpha, scale = input$theta))
            }
            
            output$fctMasa <- renderPlot({
                x = seq(0:700)
                density <- f20(x)
                plot (x = x,y=density,type="l", main = "Functie de densitate")
            })
            
            output$fctRep <- renderPlot({
                x = seq(0:700)
                prob <- F20(x)
                plot (x = x,y=prob,type="l", main = "Functie de repartitie")
            })
            
            output$fctProb <-renderPlot({
                x = seq(0:700)
                y = f20(x)
                mini = min(y)
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii") # , ylim = c(0,1))
                if(input$SelectProb=="P(x<=a)"){
                    i <- x <= input$a
                    polygon(c(0,x[i],input$a), c(0,y[i],0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    i <- x >= input$b
                    polygon(c(input$b,x[i],max(x)), c(0,y[i],0), col="light blue")
                }else{
                    x = seq(input$a , input$b)
                    y = f20(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
            })
            
            P20 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F20(a))
                    }
                    else
                    {
                        return (1 - F20(a))
                    }
                }
                else
                {
                    return (F20(b) - F20(a))
                }
            }
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P20(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P20(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P20(input$a, input$b))
                    })
                }
            })    
        }else if (input$tabs == 21)
        {
            # Consider an experiment with probability of success of p (ex21_p) and n (ex21_n) trials, i.e. X???Bin(13,0.7).
            
            observeEvent(input$ex21_n,  {
                updateSliderInput(session = session, "a", max = input$ex21_n)
                updateSliderInput(session = session, "b", max = input$ex21_n)
            })
            
            f21 = function(x){
                return(dbinom(x, size = input$ex21_n, prob = input$ex21_p))
            }
            
            F21 = function(xx){
                return(pbinom(xx, size = input$ex21_n, prob = input$ex21_p))
            }
            
            output$fctMasa <- renderPlot({
                x = 0:input$ex21_n
                density <- f21(x)
                plot (x = x,y=density,type="l", main = "Functie de densitate")
            })
            
            output$fctRep <- renderPlot({
                x = 0:input$ex21_n
                prob <- F21(x)
                plot (x = x,y=prob,type="l", main = "Functie de repartitie")
            })
            
            output$fctProb <-renderPlot({
                x = 0:input$ex21_n
                y = f21(x)
                mini = min(y)
                plot(x, y, type= "l", col="red", main = "Ilustrarea probabilitatii") 
                
                if(input$SelectProb=="P(x<=a)"){
                    i <- x <= input$a
                    polygon(c(0,x[i],input$a), c(0,y[i],0), col="light blue")
                }else if(input$SelectProb=="P(x>=b)"){
                    i <- x >= input$b
                    polygon(c(input$b,x[i],max(x)), c(0,y[i],0), col="light blue")
                }else{
                    x = seq(input$a , input$b)
                    y = f21(x)
                    polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
                }
                
            })
            
            
            P21 = function(a, b=NULL, param=NULL)
            {
                if(is.null(b))
                {
                    if(is.null(param))
                    {
                        return(F21(a))
                    }
                    else
                    {
                        return (1 - F21 (a))
                    }
                }
                else
                {
                    return (F21(b) - F21(a))
                }
            }
            
            observeEvent(input$SelectProb, {
                if(input$SelectProb=="P(x<=a)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P21(input$a))
                    })
                }else if(input$SelectProb=="P(x>=b)"){
                    output$valueProb <- renderText({
                        c("Probability: ", P21(input$b, param = 1))
                    })
                }else{
                    output$valueProb <- renderText({
                        c("Probability: ", P21(input$a, input$b))
                    })
                }
            })   
        }
    })    
}

# Run the application 
shinyApp(ui = ui, server = server)