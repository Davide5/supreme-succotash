#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rmarkdown)
library(knitr)
library(kableExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word', 'Power Point'),
                      inline = TRUE),
         downloadButton("export_test", "Download report", class = "butt"),
         tags$head(tags$style(".butt{background-color: white;} .butt{color: red;}"))
         ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   x <- reactive({faithful[, 2]})
   y <- reactive({data.frame(a1=x()[1:10],a2=x()[11:20],a3=x()[21:30])})
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      bins <- seq(min(x()), max(x()), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x(), breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$export_test <- downloadHandler(
     filename = function() {
       paste('report', sep = '.', switch(
         input$format, "PDF" = 'pdf', "HTML" = 'html', "Word" = 'docx', "Power Point" = "pptx"
       ))
     },
     
     content = function(file) {
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path(tempdir(), "Report_Generation.Rmd")
       #owd <- setwd(tempdir())
       #on.exit(setwd(owd))
       file.copy("Report_Generation.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       #params <- list(n = input$bins)
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       out <- render(tempReport, output_file = file,
                     output_format = switch(
                       input$format,
                       "PDF" = "pdf_document",
                       "HTML" = "html_document",
                       "Word" = "word_document",
                       "Power Point" = "powerpoint_presentation"
                     ),        
                     params = list(n= y(), format = input$format),
                     envir = new.env(parent = globalenv())
       )
       file.rename(out, file)
     }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

