library(shiny)

ui <- fluidPage(
  titlePanel("Correlation Quiz"),
  sidebarPanel(
    sliderInput(inputId='seed', label="Random seed", value=1743, min=1000, max=9999),
    sliderInput(inputId='numPts', label="Number of data points", value=10, min=5, max=100),
    radioButtons(inputId="plotlineQ", label="Plot the regression line?", choices=c("Yes"="1","No"=2),
               selected="2")
    ),
  mainPanel(plotOutput("scatter")),
  radioButtons(inputId="displayrQ", label="Display r?", choices=c("Yes"="1","No"=2), selected="2"),
  verbatimTextOutput("r")
)

# myPlot simply makes a scatterplot of the data in pts.  It includes a line of (least squares) best fit
# if linePlotQ=TRUE.
myPlot <- function(pts, linePlotQ){
  # Make a scatterplot of the data.
  plot(pts$x, pts$y, xlab="", ylab="", pch=19, col="blue")
  # Add line of best fit.
  if (linePlotQ){
  model <- lm(y~x, data=pts)
  lines(
    c(min(pts$x),max(pts$x)), 
    c(model$coefficients[[1]]+model$coefficients[[2]]*min(pts$x),
      model$coefficients[[1]]+model$coefficients[[2]]*max(pts$x)),
    lwd=2, col="blue"
  )
  }
}

# myData produces a data frame with 3 columns; x, y, and r.  The r column makes the correlation readily available.
# The second argument is the random seed.  Since myData is called twice below, it is important that the seed
# doesn't change.
# The intercept a_0 and the slope a_1 are randomly chosen, and the data have the form (x_i, a_0 + a_1 x_i + eps_i),
# where eps_i is normal with mean zero and standard deviation sigma.  Sigma is also randomly chosen.  The x_i are
# sampled from a uniform distribution on (0,10).
myData <- function(numPts, seed){
  set.seed(seed)
  slope <- runif(1, min=-5, max=5)
  intercept <- runif(1, min=-5, max=5)
  sigma <- runif(1, min=2, max=20)
  errors <- sigma*rnorm(numPts)
  x <- runif(numPts, min=0, max=10)
  y <- intercept + slope*x + errors
  pts <- data.frame(x=x, y=y, r=cor(x,y))
}

server <- function(input, output){
  output$r <- renderPrint({if (input$displayrQ=="1") myData(input$numPts,input$seed)$r[[1]] else "Guess the value of the Pearson correlation."})
  output$scatter <- renderPlot({myPlot(myData(input$numPts,input$seed), input$plotlineQ=="1")})
}

shinyApp(ui=ui, server=server)