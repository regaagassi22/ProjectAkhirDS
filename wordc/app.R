library(shiny)
library(wordcloud2)
library(tm)
library(colourpicker)
library(ggplot2)
library(RColorBrewer)
library(plotly)

ui <- fluidPage(
  h1("SentimentAnalisis"),
 
  tabsetPanel(
    
    # tab untuk menampilkan data-raw
    tabPanel(
      title = "data-raw",
      mainPanel(
        DT:: dataTableOutput('dataRaw'),
        br(),
        br()
      )
    ),
    
    # tab untuk menampilkan data sentiment emosi
    tabPanel(
      title = "Data Sentimen Emosi",
      mainPanel(
        DT::dataTableOutput('dataSentiment'),
        br(),
        br()
      )
    ),
    
    # tab untuk menampilkan Plot sentiment emosi
    tabPanel(
      title = "Plot Sentiment",
      mainPanel(
        plotOutput("plot1"),
        br(),
        br()
      )
    ),
    
    # tab menampilkan wordcoud
    tabPanel(
      title = "Word cloud",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "source",
            label = "Word source",
            choices = c(
          
              "Upload a file" = "file"
            )
          ),
          hr(),
          # memilih bahasa dalam menghapus stopword
          selectInput(
            inputId = "language",
            label = "Remove stopwords in",
            choices = c("Danish", "Dutch", "English", "Finnish", "French", "German", "Hungarian", "Italian", "Norwegian", "Portuguese", "Russian", "Spanish", "Swedish"),
            multiple = FALSE,
            selected = "English"
          ),
        #memilihfile
          conditionalPanel(
            condition = "input.source == 'file'",
            fileInput("file", "Select a file")
          ),
          hr(),
          checkboxInput("remove_words", "Remove specific words?", FALSE),
          conditionalPanel(
            condition = "input.remove_words == 1",
            textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
            textAreaInput("words_to_remove2", "", rows = 1)
          ),
          conditionalPanel(
            condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
            textAreaInput("words_to_remove3", "", rows = 1)
          ),
         
       
          hr(),
          numericInput("num", "Maximum number of words",
                       value = 100, min = 5
          ),
          hr(),
          colourInput("col", "Background color", value = "white"),
          hr()
     
        ),
        mainPanel(
          wordcloud2Output("cloud"),
          br(),
          br()
        )
      )
    )
    
  )
)

server <- function(input, output) {
  ##
  pathOutput = "D:\\KULYEAH SM 5\\Prak Data Science\\Sentimen Analisis Bencana\\"
  
  #output data-raw
  dataRaw<- read.csv(paste(pathOutput,'datakagel.csv',sep = ''))
  dataRaw<- data.frame(dataRaw)
  
  output$dataRaw = DT::renderDataTable({
    DT::datatable(dataRaw, options = list(lengthChange = FALSE))
  })
  
  ##output sentimentemosi
  sent_df<- read.csv(paste(pathOutput,'s.csv',sep = ''))
  sent_df <- data.frame(sent_df)
  
  output$dataSentiment = DT::renderDataTable({
    DT::datatable(sent_df, options = list(lengthChange = FALSE))
  })
  
  ##output plot
  
  ##plot emotion
  
  plotSentiments1 <- function(sentiment_dataframe) 
  {
    barplot(colSums(sentiment_dataframe),
            las = 2,
            col = rainbow(10),
            ylab = 'Count',
            main = 'Sentiment Scores for Disasters')
  }
  
  output$plot1 <- renderPlot({
    plotSentiments1(sent_df)
  })
  
  ##
  data_source <- reactive({
  if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  create_wordcloud <- function(data, num_words = 100, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data)) {
      corpus <- Corpus(VectorSource(data))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower(input$language)))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
    
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    wordcloud2(data, backgroundColor = background)
  }
  output$cloud <- renderWordcloud2({
    create_wordcloud(data_source(),
                     num_words = input$num,
                     background = input$col
    )
  })
}

shinyApp(ui = ui, server = server)