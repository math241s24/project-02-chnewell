

library(shiny)
library(tidyverse)
library(DT)
library(tidytext)
library(knitr)
library(janeaustenr) 
library(dplyr)
library(RCurl)
library(htm2txt)
library(stringr)


# User interface

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


ui <- fluidPage(
  titlePanel("What Words Are Closest To A Keyword?"),
  sidebarLayout(
    sidebarPanel(
      # Create a text input widget
      selectizeInput(inputId = "names",
                     label = "Keyword to search",
                     choices = unique(mtg_df),
                     multiple = TRUE),
      p("Only put one keyword."),
      radioButtons(inputId = "variable",
                   label = "Variable of Interest"),
      sliderInput("search_range", "Range of Search around Keyword:",
                  min = 1, max = max(mtg_df$word_count),
                  value = 500),
      submitButton("Update Results!")
      
    ),
    mainPanel(
      plotOutput(outputId = "graph"),
      dataTableOutput(outputId = "table")
      
    )
  )
)

# Server function
server <- function(input, output, session){
  
  updateSelectizeInput(session, 'names', 
                       choices = unique(mtg_df$word), 
                       server = TRUE)
  
  dat_names <- reactive({ 
    indices <- which(mtg_df == names)
    
    array_of_words <- character(length(indices)*2*search_range)
    
    for (x in 1:length(indices)) {
      print(indices[x])
      for (j in -search_range:search_range){
        array_of_words[(j+search_range)*x] <- mtg_df$word[indices[x] + j]
      }
    }
    
    df_of_words <- data.frame(words = array_of_words) %>%
      filter(words != "")
    
  })
  
  output$graph <- renderPlot({
    
    df_of_words %>%
      count(words, sort = TRUE) %>%
      slice_head(n = 15) %>%
      ggplot(aes(y = fct_reorder(words, n), x = n, fill = n)) +
      geom_col() +
      guides(fill = FALSE) + labs(y = "Most Common Words", x = "Count", title = word_to_test)
  })
  
  dat_names_agg <- reactive({ 
    dat_names() %>%
      group_by(word) %>%
      summarize(count = sum(n)) %>%
      arrange(desc(count))
  })
  
  output$table <- renderDataTable({
    datatable(dat_names_agg(), 
              options = list(paging = FALSE,
                             searching = FALSE,
                             orderClasses = TRUE))
  })
  
}

# Creates app
shinyApp(ui = ui, server = server)














library(shiny)
library(tidyverse)
library(DT)
library(tidytext)
library(knitr)
library(janeaustenr) 
library(dplyr)
library(RCurl)
library(htm2txt)
library(stringr)

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

word_to_test <- "pool"
area_of_search <- 40

indices <- which(mtg_df == word_to_test)

array_of_words <- character(length(indices)*2*area_of_search)

for (x in 1:length(indices)) {
  print(indices[x])
  for (j in -area_of_search:area_of_search){
    array_of_words[(j+area_of_search)*x] <- mtg_df$word[indices[x] + j]
  }
}

df_of_words <- data.frame(words = array_of_words) %>%
  filter(words != "")


df_of_words %>%
  count(words, sort = TRUE) %>%
  slice_head(n = 15) %>%
  ggplot(aes(y = fct_reorder(words, n), x = n, fill = n)) +
  geom_col() +
  guides(fill = FALSE) + labs(y = "Most Common Words", x = "Count", title = word_to_test)


mtg_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 100) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = n)) +
  geom_col() +
  guides(fill = FALSE)
