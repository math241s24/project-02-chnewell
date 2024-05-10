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

library(babynames)

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
                     choices = NULL,
                     multiple = TRUE),
      p("Only put one keyword."),
      radioButtons(inputId = "variable",
                   label = "Variable of Interest",
                   choices = c("n", "prop"),
                   selected = "prop"),
      sliderInput("year_range", "Range of Years:",
                  min = min(babynames$year), 
                  max = max(babynames$year),
                  value = c(1980, 2010),
                  sep = ""),
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
                       choices = unique(babynames$name), 
                       server = TRUE)
  
  
  dat_names <- reactive({ babynames %>%
      group_by(year, name) %>%
      summarize(n = sum(n)) %>%
      group_by(year) %>%
      mutate(prop = n/sum(n)) %>%
      filter(name %in% c(unlist(str_split(input$names, " "))),
             year >= input$year_range[1], 
             year <= input$year_range[2]) })
  
  
  output$graph <- renderPlot({
    
    ggplot(data = dat_names(), 
           mapping = aes_string(x = 'year', y = input$variable, color = 'name')) +
      geom_line(size = 2)
  })
  
  dat_names_agg <- reactive({ 
    dat_names() %>%
      group_by(name) %>%
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