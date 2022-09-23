
# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Vital Status Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("CD8A", label = "CD8A", value = 5.0,
                 min = min(TrainSet$CD8A),
                 max = max(TrainSet$CD8A)),
    sliderInput("IFNG", label = "IFNG", value = 3.6,
                min = min(TrainSet$IFNG),
                max = max(TrainSet$IFNG)),
    sliderInput("PDLIM1", label = "PDLIM1", value = 1.4,
                min = min(TrainSet$PDLIM1),
                max = max(TrainSet$PDLIM1)),
    sliderInput("CTLA4", label = "CTLA4", value = 0.2,
                min = min(TrainSet$CTLA4),
                max = max(TrainSet$CTLA4)),
    sliderInput("GZMK", label = "GZMK", value = 0.2,
                min = min(TrainSet$GZMK),
                max = max(TrainSet$GZMK)),
    sliderInput("CD4", label = "CD4", value = 0.2,
                min = min(TrainSet$CD4),
                max = max(TrainSet$CD4)),
    sliderInput("CCR7", label = "CCR7", value = 0.2,
                min = min(TrainSet$CCR7),
                max = max(TrainSet$CCR7)),
    sliderInput("PERP", label = "PERP", value = 0.2,
                min = min(TrainSet$PERP),
                max = max(TrainSet$PERP)),
    sliderInput("MAPK1", label = "MAPK1", value = 0.2,
                min = min(TrainSet$MAPK1),
                max = max(TrainSet$MAPK1)),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Vital Status')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("CD8A",
               "IFNG",
               "PDLIM1",
               "CTLA4",
               "GZMK",
               "CD4",
               "CCR7",
               "PERP",
               "MAPK1"),
    Value = as.character(c(input$CD8A,
                             input$IFNG,
                             input$PDLIM1,
                             input$CTLA4,
                             input$GZMK,
                             input$CD4,
                             input$CCR7,
                             input$PERP,
                             input$MAPK1)),
      stringsAsFactors = FALSE)
    
    meta.vital_status <- 0
    df <- rbind(df, meta.vital_status)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)