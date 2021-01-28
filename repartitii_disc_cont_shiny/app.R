#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Repartitii discrete si continue"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("SelectProb", "Select probability formula", choices = c("P(x<=a)", "P(x>=b)", "P(a<=x<=b)")),
            sliderInput("xmasa",
                        "X:",
                        step = 1,
                        min = 0,
                        max = 1,
                        value = 1),
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
            sliderInput("lmb",
                        "Lambda:",
                        min = 0.1,
                        max = 1,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("fctMasa"),
            plotOutput("fctRep"),
            plotOutput("fctProb"),
            textOutput("valueProb"),
            plotOutput("fctDens2"),
            plotOutput("fctRep2"),
            plotOutput("fctProb2"),
            textOutput("valueProb2"),
            plotOutput("fctDens3"),
            plotOutput("fctRep3"),
            plotOutput("fctProb3"),
            textOutput("valueProb3")
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
    observeEvent(input$SelectProb, {
        if(input$SelectProb=="P(x<=a)"){
            output$valueProb2 <- renderText({
                c("Probability: ", P2(input$a))
            })
        }else if(input$SelectProb=="P(x>=b)"){
            output$valueProb2 <- renderText({
                c("Probability: ", P2(input$b, param = 1))
            })
        }else{
            output$valueProb2 <- renderText({
                c("Probability: ", P2( input$a, input$b))
            })
        }
    })
    
    # x>0
    fd3 = function(x){
        return (input$lmb*exp(-input$lmb*x))
    }
    F3 = function(x){
        return (integrate(fd3,lower = 0, upper = x)$value)
    }
    F3 = Vectorize(F3, vectorize.args = "x")
    output$fctDens3 <- renderPlot({
        x = seq(-10, 10, length.out = 1000)
        y = fd3(x)
        plot(x, y, type= "l", col="red")
    })
    output$fctRep3 <- renderPlot({
        x = seq(-10, 10, length.out = 1000)
        y = F3(x) 
        plot(x, y, type= "l", col="red")
    })
    output$fctProb3 <- renderPlot({
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
            output$valueProb3 <- renderText({
                c("Probability: ", P3(input$a))
            })
        }else if(input$SelectProb=="P(x>=b)"){
            output$valueProb3 <- renderText({
                c("Probability: ", P3(input$b, param = 1))
            })
        }else{
            output$valueProb3 <- renderText({
                c("Probability: ", P3(input$a, input$b))
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
