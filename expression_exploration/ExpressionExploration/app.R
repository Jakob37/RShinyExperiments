library(shiny)
library(tidyverse)

source("~/src/JupyterRReuse/MultivarVis.R")
source("~/src/JupyterRReuse/EvalVis.R")

# Data upload
# https://gist.github.com/dgrapov/dfcf6ab2339b1cf19b090cfb8dadc855

ddf <- read_tsv("data/bull_design.tsv")
rdf <- read_tsv("data/bull_data.tsv")
sdf <- rdf[, ddf$sample]

non_sample_head <- colnames(rdf)[which(!colnames(rdf) %in% ddf$sample)]
cond_col <- colnames(ddf)

ui <- fluidPage(
  titlePanel("Dynamic plot explorations"),
  
  sidebarLayout(
    
    sidebarPanel(
      wellPanel(
        fileInput("data_fp", "Choose data file",
                  multiple=FALSE,
                  accept=c("text/tsv", ".tsv")),
        fileInput("design_fp", "Choose data file",
                  multiple=FALSE,
                  accept=c("text/tsv", ".tsv"))
      ),
      wellPanel(
        selectInput("plot_type", 
                    "Plot type:", 
                    c("pca", "dendogram", "bars")),
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
  
  # get_data <- reactive({
  #   
  #   req(input$data_fp)
  #   
  #   infile <- input$data_fp
  #   if (is.null(data_fp)) {
  #     return(NULL)
  #   }
  #   objects_loaded <- load(input$data_fp$name)
  #   df <- eval(parse(text=objects_loaded[1]))
  #   return(df)
  # })
  
  output$plot <- renderUI({
    
    if (is.null(input$plot_type))  {
      return();
    }
    
    if (input$plot_type == "pca") {
      plotOutput("pca")
    }
    else if (input$plot_type == "bars") {
      plotOutput("bars")
    }
    else if (input$plot_type == "dendogram") {
      plotOutput("dendogram")
    }
  })
  
  output$ui <- renderUI({
    
    if (is.null(input$plot_type)) {
      return();
    }
    
    if (input$plot_type == "pca") {
      fluidRow(
        column(12,
               selectInput(
                 inputId="color_factor",
                 label="Color factor:",
                 choices=cond_col,
                 selected=cond_col[1]
               )
        ),
        column(12,
               numericInput(
                 inputId="pc1",
                 label="PC1:",
                 min=1,
                 max=10,
                 value=1,
                 step=1
               )
        ),
        column(12,
               numericInput(
                 inputId="pc2",
                 label="PC2:",
                 min=1,
                 max=10,
                 value=2,
                 step=1
               )
        )
      )
    }
    else if (input$plot_type == "dendogram") {
      fluidRow(column(12,
                      selectInput(
                        inputId="color_factor",
                        label="Color factor:",
                        choices=cond_col,
                        selected=cond_col[1]
                      )
      ))
    }
    else if (input$plot_type == "bars") {
      fluidRow(column(12,
                      selectInput(
                        inputId="color_factor",
                        label="Color factor:",
                        choices=cond_col,
                        selected=cond_col[1]
                      ),
                      checkboxInput(
                        inputId="order_factors",
                        label="Order on factors:",
                        value=FALSE
                      )
      ))
    }
  })
  
  output$bars <- renderPlot({
    ev$abundance_bars(sdf, color_col=ddf[[input$color_factor]], cond_order = input$order_factors)
  })
  
  output$pca <- renderPlot({
    
    #print(head(get_data()))
    
    mv$pca(sdf, unlist(ddf[, input$color_factor]), 
           ddf$sample, ddf$sample, pc1=input$pc1, pc2=input$pc2, only_text=F)
    
  }, width="auto", height="auto")
  
  output$dendogram <- renderPlot({
    mv$dendogram(sdf, ddf$sample, ddf$sample, ddf[[input$color_factor]])
  }, width="auto", height="auto")
}

shinyApp(ui = ui, server = server)
