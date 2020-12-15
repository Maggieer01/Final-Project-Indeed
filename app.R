
library(shiny)
library(dplyr)
library(ggplot2)
library(tidytext)

listings <- read.csv("listings.csv",header=TRUE)
listings$description <- gsub("[^0-9A-Za-z ]", "" , listings$description,ignore.case = TRUE)
github <- read.csv("github.csv")
west <- read.csv("west.csv")
east <- read.csv("east.csv")


f <- function(df){
    text <- df$description
    descript_df <- tibble(line = 1:nrow(df), text = text)
    
    descript_df <- descript_df %>% unnest_tokens(word, text)
    
    ##  Remove stop-words such as the, a, this, and that ...
    data("stop_words")     
    
    descript_df <- descript_df %>% anti_join(stop_words)
    
    result <- descript_df %>%
        count(word, sort = TRUE) %>%
        mutate(word = reorder(word, n))
    return(result)
}

# Define UI for application that contains three tabPanels
ui <- navbarPage("My Map",
                 tabPanel("Indeed Top Descrption Words",
                          
                          # Sidebar with a slider input for number of bins 
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput("range_indeed",
                                              "Choose a range:",
                                              min= 0, max = 60, value = c(0,10),
                                                step=10)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                  plotOutput("countIndeed")
                              )
                          )
                 
                 ),
                  tabPanel("Github Top Description Words",
                           # Sidebar with a slider input for number of bins
                           sidebarLayout(
                               sidebarPanel(
                                   sliderInput("range_github",
                                               "Choose a range:",
                                               min= 0, max = 50, value = c(0,10),
                                               step=10)
                               ),
                               
                               # Show a plot of the generated distribution
                               mainPanel(
                                   plotOutput("countGithub")
                               )
                           )
                  ),
                     tabPanel("Top Words by region",
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("region",
                                                  "Choose a region:",
                                                  c("west","east"))
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                      plotOutput("countRegion")
                                  )
                              )
                    )
)

# Define server logic required to draw the plots
server <- function(input, output) {
    # tabPanel 1
    r = reactive({
        i = input$range_indeed
    })
    
    output$countIndeed <- renderPlot({
        f(listings)[r()[1]:r()[2],] %>% 
            ggplot(aes(x = n, y = word, fill=word, label=n)) +
            geom_col() +
            labs(title = paste("Indeed Top words in job description, range",r()[1],":",r()[2]), x = "Word Count", y = "Word")

    })
    
    # tabPanel 2
    r1 = reactive({
        i = input$range_github
    })
    
    output$countGithub <- renderPlot({
        f(github)[r1()[1]:r1()[2],] %>% 
            ggplot(aes(x = n, y = word, fill=word, label=n)) +
            geom_col() +
            labs(title = paste("Github Top words in job description, range",r1()[1],":",r1()[2]), x = "Word Count", y = "Word")
        
    })    
    
    # tabPanel 3
    r2 = reactive({
        i = input$region
    })
    
    output$countRegion <- renderPlot({
        if (r2() == 'west') {
            f(west)[1:40,] %>% 
                ggplot(aes(x = n, y = word, fill=word, label=n)) +
                geom_col() +
                labs(title = paste("Top 40 words in the", r2()), x = "Word Count", y = "Word")
        } else{
            f(east)[1:40,] %>% 
                ggplot(aes(x = n, y = word, fill=word, label=n)) +
                geom_col() +
                labs(title = paste("Top 40 words in the", r2()), x = "Word Count", y = "Word")
        }
    })    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
