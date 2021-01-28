#
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
ui <- fluidPage(
    
    navbarPage("Navbar!",
               tabPanel("1",
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
                                    value = 1),
                        sliderInput("prob",
                                    "Probability:",
                                    min = 0.1,
                                    max = 1,
                                    value = 0.33),
                        mainPanel(
                            plotOutput("fctMasa"),
                            plotOutput("fctRep"),
                            plotOutput("fctProb"),
                            textOutput("valueProb")
                            
                        )
               ),
               tabPanel("2",
                        selectInput("SelectProb2", "Select probability formula", choices = c("P(x<=a)", "P(x>=b)", "P(a<=x<=b)")),
                        sliderInput("a2",
                                    "a:",
                                    step = 1,
                                    min = 0,
                                    max = 1,
                                    value = 0),
                        sliderInput("b2",
                                    "b:",
                                    step = 1,
                                    min = 0,
                                    max = 1,
                                    value = 1),
                        mainPanel(
                            plotOutput("fctDens2"),
                            plotOutput("fctRep2"),
                            plotOutput("fctProb2"),
                            textOutput("valueProb2")
                        )
               ),
               tabPanel("4",
                        selectInput("SelectProb4", "Select probability formula", choices= c("P(x<=a)", "P(x>=b)", "P(a<=x<=b)")),
                        sliderInput("a4",
                                    "a:",
                                    step = 1,
                                    min = 0,
                                    max = 300,
                                    value = 0),
                        sliderInput("b4",
                                    "b:",
                                    step = 1,
                                    min = 0,
                                    max = 300,
                                    value = 300),
                        sliderInput("prob4",
                                    "Probability:",
                                    min = 0.1,
                                    max = 1,
                                    value = 0.30),
                        numericInput("nr4",
                                     "Cate aruncari",
                                     min = 1,
                                     max = 1000,
                                     value = 300),
                        mainPanel(
                            plotOutput("fctDens4"),
                            plotOutput("fctRep4"),
                            plotOutput("fctProb4"),
                            textOutput("valueProb4")
                        )
            )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
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
    
    # when water change, update air
    observeEvent(input$a,  {
        updateSliderInput(session = session, "b", min = input$a)
    })
    
    # when air change, update water
    observeEvent(input$b,  {
        updateSliderInput(session = session, "a", max = input$b)
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
    
    # output$valueProb <- renderText({
    #     c("Probability: ", P(input$prob, input$a, input$b))
    # })
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
            x = seq(0,input$a,length.out = 1000)
            y = F(x,input$prob)
            polygon(c(0,x,input$a), c(0,y,0), col="light blue")
        }else if(input$SelectProb=="P(x>=b)"){
            x = seq(input$b,2,length.out = 1000)
            y = F(x,input$prob)
            polygon(c(input$b,x,2), c(0,y,0), col="light blue")
        }else{
            x = seq(input$a,input$b,length.out = 1000)
            y = F(x,input$prob)
            polygon(c(input$a,x,input$b), c(0,y,0), col="light blue")
        }
        
    })
    
    # 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # V.a. continua
    fd2 = function(x){
        return (exp(x)/(1 + exp(x))^2)
    }
    F2 = function(x){
        return (integrate(fd2,lower = -Inf, upper = x)$value)
    }
    F2 = Vectorize(F2, vectorize.args = "x")
    output$fctDens2 <- renderPlot({
        x = seq(-10, 10, length.out = 1000)
        y = fd2(x)
        plot(x, y, type= "l", col="red")
    })
    output$fctRep2 <- renderPlot({
        x = seq(-10, 10, length.out = 1000)
        y = F2(x) 
        plot(x, y, type= "l", col="red")
    })
    output$fctProb2 <- renderPlot({
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
    
    observeEvent(input$a2,  {
        updateSliderInput(session = session, "b2", min = input$a2)
    })
    
    # when air change, update water
    observeEvent(input$b2,  {
        updateSliderInput(session = session, "a2", max = input$b2)
    })
    
    observeEvent(input$SelectProb2, {
        if(input$SelectProb2=="P(x<=a)"){
            output$valueProb2 <- renderText({
                c("Probability: ", P2(input$a2))
            })
        }else if(input$SelectProb2=="P(x>=b)"){
            output$valueProb2 <- renderText({
                c("Probability: ", P2(input$b2, param = 1))
            })
        }else{
            output$valueProb2 <- renderText({
                c("Probability: ", P2( input$a2, input$b2))
            })
        }
    })


    # 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #  Suppose a baseball player has a p= .300 batting average. What is the probability of X<=150 hits in n=500 at bats? X=150? X>150?
    
    
    

    
    fd4 = function(x)
    {
        return(dpois(x = hits, input$prob4*input$nr4 , log = FALSE))
    }
    
    F4 = function(xx)
    {
        return (ppois(q = xx, lambda = input$nr4*input$prob4, lower.tail = TRUE))
    }
#    F4 = Vectorize(F4,vectorize.args = "x")

    output$fctDens4 <- renderPlot({
        hits <- 0:input$nr4 
        density <- dpois(x = hits, lambda = input$prob4 * input$nr4)
        plot (x = hits,y=density,type="l")
    })
    
    output$fctRep4 <- renderPlot({
        hits <- 0:input$nr4 
        prob <- ppois(q = hits, lambda = input$prob4 * input$nr4, lower.tail = TRUE)
        plot (x = hits,y=prob,type="l")
    })
    
    
    output$fctProb4 <- renderPlot({
        x = 0:input$nr4
        y = ppois(q = x, lambda = input$prob4 * input$nr4, lower.tail = TRUE)
        
        plot(x, y, type= "l", col="red")
        
        if(input$SelectProb4=="P(x<=a)"){
            polygon(c(input$a4,x[x<=input$a4]), c(0,y[x<=input$a4]), col="light blue")
        }else if(input$SelectProb4=="P(x>=b)"){
            polygon(c(input$b4,x[input$b4<=x],input$nr4),c(0,y[input$b4<=x],0), col="light blue")
        }else{
            x = seq(input$a4,input$b4)
            y = F4(x)
            polygon(c(input$a4,x,input$b4), c(0,y,0), col="light blue")
        }
    })
    
    observeEvent(input$a4,  {
        updateSliderInput(session = session, "b4", min = input$a4)
    })
    
  
    observeEvent(input$b4,  {
        updateSliderInput(session = session, "a4", max = input$b4)
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)