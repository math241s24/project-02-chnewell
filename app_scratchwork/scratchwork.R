library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

url <- 'https://media.wizards.com/2024/downloads/MagicCompRules%2020240308.txt'
mtgtext <- gettxt(url)
mtgtext <- str_split(mtgtext[1], "\\s+") %>% pluck(1)

mtg_df <- as.data.frame(mtgtext)
names(mtg_df) <- "word"

data("stop_words")

mtg_df <- mtg_df %>%
  anti_join(stop_words) %>%
  group_by(word) %>%
  mutate(word_count = n())

ui <- shinyUI(
  bootstrapPage(
    h3("Frequency Keyword"),
    numericInput(
      "startyear", "Enter search width",
      value = 20, min = 0, max = 100, step = 1
    ),
    selectizeInput(inputId = "names",
                   label = "Keyword to search",
                   choices = NULL,
                   multiple = TRUE),
    submitButton("Update Results!"),
    plotOutput("plot")
  )
)

server <- shinyServer(
  function(input, output, session) {
    updateSelectizeInput(session, 'names', 
                         choices = unique(mtg_df$word), 
                         server = TRUE)
    
    keyword <- reactive({
      input$names
    })
    
    indices <- reactive({
      which(mtg_df$word == keyword())
    })
    
    array_of_words <- reactive({
      character(length(indices())*2*startyear())
    })
    
    startyear <- reactive({
      input$startyear
    })
    
    reactive({
      for (x in 1:length(indices)) {
        for (j in -startyear():startyear()){
          array_of_words()[(j+startyear())*x] <- mtg_df$word[indices()[x] + j]
        }
      }
    })
    
    
    df_of_words <- reactive({
      data.frame(words = array_of_words()) %>%
        filter(words != "")
    })
    
    output$plot <- renderPlot({
      df_of_words() %>%
        count(words, sort = TRUE) %>%
        slice_head(n = 15) %>%
        ggplot(aes(y = fct_reorder(words, n), x = n, fill = n)) +
        geom_col() +
        guides(fill = FALSE) + 
        labs(y = "Most Common Words", x = "Count", title = paste("Frequency of", keyword()))
    })
  }
)

shinyApp(ui = ui, server = server)
