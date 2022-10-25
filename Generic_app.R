# Import libraries for Shiny App
library(plotly)
library(shiny)
library(shinythemes)

# Import libraries for data and modeling
library(fpp3)
library(dplyr)

# Define UI for data upload app ----
ui <- navbarPage(
  theme = shinytheme("superhero"),
  "Explore Your Time Series!",
  tabPanel( # App title ----
    "Uploading Files",

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),

        # Horizontal line ----
        tags$hr(),

        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),

        # Input: Select separator ----
        radioButtons("sep", "Separator",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ","
        ),

        # Input: Select quotes ----
        radioButtons("quote", "Quote",
          choices = c(
            None = "",
            "Double Quote" = '"',
            "Single Quote" = "'"
          ),
          selected = '"'
        ),

        # Horizontal line ----
        tags$hr(),

        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
          choices = c(
            Head = "head",
            All = "all"
          ),
          selected = "head"
        ),


        # Horizontal line ----
        tags$hr(),

        # Input: Type date format
        textInput("text",
          label = "Date format input",
          value = "%Y-%m-%d"
        )
      ),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Data file ----
        tableOutput("contents")
      )
    )
  ),
  tabPanel(
    "Explore Data",
    sidebarLayout(
      sidebarPanel(
        "Ensure that your dataframe starts with a date column,
        the dataframe is regular and has no missing values, and
        the second column is the variable you want to select. If you
        are getting an error despite missing these requirements, check
        previous tab to see if date format is correctly specified",
        h4(""),

        # Input: Title
        textInput("title",
          label = "Title of graph",
          value = "Graph"
        ),

        # Horizontal line ----
        tags$hr(),

        # Input: X-axis
        textInput("x",
          label = "Title of x-axis",
          value = "Date"
        ),

        # Horizontal line ----
        tags$hr(),

        # Input: Y-axis
        textInput("y",
          label = "Title of y-axis",
          value = "Variable of Interest"
        ),

        # Horizontal line ----
        tags$hr(),

        # Input: decomp type
        div(
          selectInput(
            inputId = "dcmptype",
            label = "Select decomposition type", # User chooses graph
            choices = c(
              "additive",
              "multiplicative"
            )
          )
        ),

        # Horizontal line ----
        tags$hr(),

        # Choose type of analysis
        div(
          selectInput(
            inputId = "selected",
            label = "Select a graph", # User chooses graph
            choices = c(
              "Time Series",
              "Autocorrelation",
              "Seasonality",
              "Decomposition"
            )
          )
        )
      ),
      mainPanel(
        plotlyOutput("tsplot")
      )
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    df <- read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )

    if (input$disp == "head") {
      return(head(df))
    } else {
      return(df)
    }
  })

  output$tsplot <- renderPlotly({
    data_df <- read.csv(input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )

    # rename time column
    data_df <- rename(data_df, date = 1)
    data_df <- rename(data_df, var1 = 2)

    # save as date from user
    data_df$date <- as.Date(data_df$date, input$text)

    # save as tsibble
    data_df <- data_df %>%
      as_tsibble(index = date)

    if (input$selected == "Time Series") {
      p1 <- autoplot(data_df, data_df$var1) +
        ggtitle(input$title) +
        ylab(input$y) +
        xlab(input$x) +
        theme_classic()

      # configure to liking
      ggplotly(p1) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected == "Autocorrelation") {
      p2 <- autoplot(ACF(data_df, data_df$var1)) +
        ggtitle(input$title) +
        ylab(input$y) +
        xlab(input$x) +
        theme_classic()

      ggplotly(p2) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected == "Seasonality") {
      
      p3 <- gg_season(data_df) +
        ggtitle(input$title) +
        ylab(input$y) +
        xlab(input$x) +
        theme_classic()
      
      ggplotly(p3) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else if (input$selected == "Decomposition") {
      dcmp <- data_df %>%
        model(classical_decomposition(var1, type = input$dcmptype)) %>%
        components()

      p4 <- autoplot(dcmp) +
        theme_bw()

      ggplotly(p4) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    }
  })
}


# Run the app ----
shinyApp(ui, server)
