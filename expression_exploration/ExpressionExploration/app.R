library(shiny)
library(tidyverse)

ddf <- read_tsv("data/design_utf8.tsv")
rdf <- read_tsv("data/data_utf8.tsv")

feature_col <- rdf[, "Feature"]
cond_col <- colnames(ddf)[2:ncol(ddf)]

ui <- fluidPage(
  titlePanel("Dynamic plot explorations"),
  
  sidebarLayout(
    
    sidebarPanel(
      wellPanel(
        selectInput("input_type", "Input type", c("type1", "type2"))
      ),
      wellPanel(
        uiOutput("ui")
      )
    ),
    
    mainPanel(
      #plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  
  output$ui <- renderUI({
    
    if (is.null(input$input_type)) {
      return();
    }
    
    if (input$input_type == "type1") {
      
      fluidRow(column(12,
                      
                      sliderInput("dynamic1", "Dynamic", min=1, max=20, value=10),
                      sliderInput("dynamic2", "Dynamic2", min=1, max=20, value=10)
      ))
    }
    else {
      
      fluidRow(column(12,
                      
                      selectInput(
                        inputId="rowid",
                        label="Row number:",
                        choices=feature_col,
                        selected = feature_col[1]
                      ),
                      selectInput(
                        inputId="levels",
                        label="Level factor:",
                        choices=cond_col,
                        selected = cond_col[1]
                      )
      ))
      
    }
  })
  
  output$scatterPlot <- renderPlot({
    
    row_nbr <- which(unlist(feature_col) %in% input$rowid)
    samples <- ddf$sample
    row <- unlist(rdf[row_nbr, samples])
    plot_df <- data.frame(x=1:length(row), y=row, color=ddf[, input$levels])
    colnames(plot_df) <- c("x", "y", "color")
    ggplot(plot_df, aes(x=x, y=y)) + geom_point(aes(size=2, color=color)) + ggtitle("Expression levels")
  })
}

shinyApp(ui = ui, server = server)
