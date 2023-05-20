library(shiny)

fluidPage(
  
    titlePanel("Visualizing Split Test Parameters"),

    sidebarLayout(
        sidebarPanel(
            selectInput("alternative", "Alternative:", c("two.sided", "less", "greater")),
            checkboxInput("transform", "Arcsine Transformation ", TRUE),
            numericInput("prob", "Conversion Rate (%):", 20, 0, 100),
            numericInput("mde", "Minimum Detectable Effect (%):", 5, 0, 100),
            numericInput("alpha", "Significance (%):", 5, 0, 100),
            checkboxInput("show_sig", "Draw Significance ", TRUE),
            numericInput("power", "Statistical Power (%):", 80, 0, 100),
            checkboxInput("show_power", "Draw Power ", TRUE),
            sliderInput("progress", "Progress (%)", 10, 100, 100),
            actionButton("go", "Update"),
            br(),
            br(),
            verbatimTextOutput("size")
        ),

        mainPanel(
            plotOutput("distPlot")
        )
    )
)
