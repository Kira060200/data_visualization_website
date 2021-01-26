#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

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
                        value = 0.33)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("fctMasa"),
            plotOutput("fctRep"),
            plotOutput("fctProb"),
            textOutput("valueProb")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    FctMasa = function(p){
        plot(c(1,0),c(p,1-p), lwd=5)
        segments(c(1,0), 0, c(1,0), c(p,1-p), col="red")
    }
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)
