library(shiny)
library(tidyverse)

ddf <- read_tsv("data/bull_design.tsv")
rdf <- read_tsv("data/bull_data.tsv")
sdf <- rdf[, ddf$sample]

print(colnames(rdf))
print(ddf$sample)
print(!which(colnames(rdf) %in% ddf$sample))

non_sample_head <- colnames(rdf)[which(!colnames(rdf) %in% ddf$sample)]

print(non_sample_head)

# feature_col <- rdf[, "Feature"]
cond_col <- colnames(ddf)[2:ncol(ddf)]

ui <- fluidPage(
  titlePanel("Dynamic plot explorations"),
  
  sidebarLayout(
    
    sidebarPanel(
      wellPanel(
        selectInput("plot_type", 
                    "Plot type:", 
                    c("Bars", "Scatter")),
        selectInput("feature_col", 
                    "Feature column:", 
                    non_sample_head)
      ),
      wellPanel(
        uiOutput("ui")
      )
    ),
    
    mainPanel(
      uiOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderUI({

    if (is.null(input$plot_type))  {
      return();
    }

    if (input$plot_type == "Bars") {
      plotOutput("barchart")
    }
    else if (input$plot_type == "Scatter") {
      plotOutput("scatter")
    }
  })
  
  output$ui <- renderUI({
    
    if (is.null(input$plot_type)) {
      return();
    }
    
    if (input$plot_type == "Bars") {
      
      fluidRow(column(12,
                      
                      selectInput(
                        inputId="levels",
                        label="Level factor:",
                        choices=cond_col,
                        selected = cond_col[1]
                      )
      ))
    }
    else if (input$plot_type == "Scatter") {
      
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
  
  output$barchart <- renderPlot({
    col_sums <- colSums(sdf)
    plot_df <- data.frame(sample=ddf$sample, amount=col_sums, color=ddf[, input$levels])
    colnames(plot_df) <- c("sample", "amount", "color")
    ggplot(plot_df, aes(x=sample, y=amount, fill=color)) + geom_bar(stat="identity")
  })
  
  output$scatter <- renderPlot({
    
    row_nbr <- which(unlist(feature_col) %in% input$rowid)
    samples <- ddf$sample
    row <- unlist(rdf[row_nbr, samples])
    plot_df <- data.frame(x=1:length(row), y=row, color=ddf[, input$levels])
    colnames(plot_df) <- c("x", "y", "color")
    ggplot(plot_df, aes(x=x, y=y)) + geom_point(aes(size=2, color=color)) + ggtitle("Expression levels")

  })
}

shinyApp(ui = ui, server = server)
