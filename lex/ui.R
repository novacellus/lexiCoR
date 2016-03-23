library(shiny)
library(rcqp)

shinyUI(fluidPage(
  # Application title
  titlePanel("Lexical Semantics with R"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "corpus",
        label = "Corpus choice:",
        choices = c("Choose your corpus", app_env$corpora),
        selected = "PATROLOGIA"
      ),
      conditionalPanel(
        condition = "input.corpora != 'Choose your corpus'  ",
        
        radioButtons(
          "what",
          label = "What?",
          choices = c("lemma", "word"),
          selected = "lemma",
          inline = FALSE,
          width = NULL
        ),
        
        conditionalPanel(
          condition = "input.what",
          
          textInput("query",
                    label = "Your query:",value = "",
                    placeholder = "Enter one word only"),
          checkboxGroupInput(
            "props",
            label = "Properties",
            choices = c(
              "lemma distribution" = "dist_lemma",
              "wordform distribution" = "dist_forms"
            )
          )
        )
      ),
      actionButton("lookup", "Search")
    ),
    
    mainPanel(
        tabsetPanel(id = "tabs", type = "pills",
        tabPanel("Corpus", value = "corpus", tableOutput("corp"),
                 tableOutput("corpus_rank"),
                 plotOutput("corpus_plot"),
                 plotOutput("corpus_plot_log")
                 ),
        tabPanel("Corpus distribution", value = "dist",
                 plotOutput("forms_plot1"), plotOutput("forms_plot2"),
                 plotOutput("forms_plot3"), plotOutput("scatt_pairs"),
                 plotOutput("scatt_scatt1"), plotOutput("scatt_scatt2"),
                 plotOutput("ecdf"), plotOutput("boxpl"),
                 plotOutput("corrpl"), plotOutput("meta") ),
        tabPanel("Collocations", value = "colls",
                 tableOutput("tab1"),
                 tableOutput("colloc_table"),
                 tableOutput("colloc_list")
                 )
      )
    )
  )
))
