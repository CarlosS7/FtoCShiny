# Fahrenheit to Celsius Converter Shiny App
# Carlos Alas
# 02/28/2020
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Load the keras library
library(keras)
library(tidyverse)
#install_keras()


# read in some data
df = read_csv('F_and_C.csv')

# Setup model
model <- keras_model_sequential()
model %>% 
    layer_dense(units=1, input_shape=c(1))
model %>% compile(
    loss='mean_squared_error',
    optimizer=optimizer_adam(0.1)
)


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Celsius to Fahrenheit"),

    # Sidebar with input sliders for number of datapoints to predict (model training is done with the whole dataset),
    # and for the number of epochs to train
    sidebarLayout(
        sidebarPanel(
            numericInput("temp",
                         "Temp in F",
                         value = 30,
                         min = -50,
                         max = 200,
                         step = 1),
            sliderInput("data_points",
                        "Number of data points",
                        min = 0,
                        max = 1500,
                        step = 10,
                        value = 20),
            sliderInput("epochs",
                        "Number of epochs:",
                        min = 1,
                        max = 500,
                        value = 5),
            #numericInput("layers",
                         #"Number of Layers",
                         #value = 1,
                         #min = 1,
                         #max = 5,
                         #step = 1),
            actionButton("run",
                         "Run")
        ),

        # Show the predicted temperature, plot of predicted values, and the real plot
        mainPanel("",
            fluidRow(
            splitLayout(cellWidths = c("33%", "33%", "33%"), h1("Prediction:", textOutput("distPlot"), style = "text-align: center"), plotOutput("predPlot"), plotOutput("realplot"))
        )
    )
        
))

# Define server logic 
server <- function(input, output) {
    df_pred <- df['f']
    # Randomize data points to avoid bias
    set.seed(3)
    rows <- sample(nrow(df_pred))
    df_rand <- df_pred[rows,]
    tempVal <- eventReactive(input$run, {
        runif(input$temp)
        history <- model %>% fit(df$f, df$c, epochs=input$epochs)
        df_pred <- df_rand[1:input$data_points,]
        df_pred <<- df_pred %>% mutate(c = model%>%predict(df_pred$f))
        temp <- model %>% predict(input$temp)
        print(temp)
        
    })

    output$distPlot <- renderText({
        # generate bins based on input$bins from ui.R
        # train the model now
        tempVal()
        
    })
    
    output$predPlot <- renderPlot({
        renderPredPlot()
    })
    output$realplot <- renderPlot({
        plot(df$f, df$c, main="Real Plot", col = "gray", pch = ".")
    })
    
    renderPredPlot <- eventReactive(input$run, {
        plot(df_pred$f, df_pred$c, main="Predicted Plot", col = "blue", pch = ".")
 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
