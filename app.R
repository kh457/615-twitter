#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(wordcloud)
library(plotly)
library(dplyr)
cp3_corpus<-readRDS('cp3_corpus.rds')
bing_word_counts<-readRDS('bing_word_counts.rds')
cppoints<-readRDS('cppoints.rds')
cpdata<-map_data('state')
jh_corpus<-readRDS('jh_corpus.rds')
bing_word_counts1<-readRDS('bing_word_counts1.rds')
jhpoints<-readRDS('jhpoints.rds')
# Define UI for application that draws a histogram
ui<-fluidPage(titlePanel(fluidRow(
  column(8, 
         h3("MA615 Final Project"),
         h6("Presented by Kailun Huang. 
                              All codes can be found at GITHUB", a(href="https://github.com/kh457/615-twitter", "kh457 Github"))
  ))),
  navbarPage(title= "",
             tabPanel("Introduction",
                      hr(),
                      p("Chris Paul and James Harden are two famous NBA players and they are my idol, I liked him very much. This summer he chose to left LA Clippers and joined Houston Rockets. James Harden is a super star in Rockets. I want to know how his fans or other people
view his trade. I collected data from twitter and made word cloud, sentiment analysis, map for where his comments come from.
                        ")),
             tabPanel("Maps",
                      h4("Tweets from different places comments relate to Chris Paul and James Harden"),
                      p("Since clippers is in Carlifonia and Rockets is in Texas, these two places have lots of comments about these two players"),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "mapinput",
                                      label="Select players",
                                      choices = c("Chris Paul","James Harden"))
                        ),
                        mainPanel(plotOutput("maps"))
                      )
             ),
             tabPanel("Sentiment",
                      h4("Sentiment Analysis"),
                      p("From the sentiment analysis, we can know most twitter opinion about Chris Paul and James Harden. It is because sentiment analysis show frequencies of postive and negative words"),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(selectInput(inputId = "sentiinput",
                                                 label="Select players",
                                                 choices = c("Chris Paul","James Harden"))
                        ),
                        mainPanel(plotOutput("sentiment")))
                      ),
             tabPanel("Word Cloud",
                      h4("Word Cloud for two players"),
                      p("Here showed the common words appear on Tweets relate to these two players.The more a specific word appears, the bigger and bolder the word will appear in the visualization
"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "wordinput",
                                      label="Select players",
                                      choices = c("Chris Paul","James Harden")),
                          sliderInput("freq", "Minimum Frequency:",
                                      min = 1,  max = 50, value = 15),
                          sliderInput("max", "Maximum Number of Words:", 
                                      min = 1,  max = 300,  value = 100)),
                        mainPanel(
                          plotOutput("wordcloud")
                        )
                      )
             )
             
  )
  )

#server
server<-function(input, output){
  
  ### maps
  output$maps<-renderPlot({
    if(input$mapinput=="Chris Paul"){
      ggplot(cpdata) + 
        geom_map(aes(map_id = region),  
                 map = cpdata,  
                 fill = "white",             
                 color = "grey20", size = 0.25) + 
        expand_limits(x = cpdata$long, y = cpdata$lat) +            
        theme(axis.line = element_blank(),  
              axis.text = element_blank(),  
              axis.ticks = element_blank(),                     
              axis.title = element_blank(),  
              panel.background = element_blank(),  
              panel.border = element_blank(),                     
              panel.grid.major = element_blank(), 
              plot.background = element_blank(),                     
              plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
        geom_point(data = cppoints,             
                   aes(x = x, y = y), size = 1,  
                   alpha = 1/5, color = "red")+labs(title="Map for Chris Paul")
    }
    else{
      ggplot(cpdata) + 
        geom_map(aes(map_id = region),  
                 map = cpdata,  
                 fill = "white",             
                 color = "grey20", size = 0.25) + 
        expand_limits(x = cpdata$long, y = cpdata$lat) +            
        theme(axis.line = element_blank(),  
              axis.text = element_blank(),  
              axis.ticks = element_blank(),                     
              axis.title = element_blank(),  
              panel.background = element_blank(),  
              panel.border = element_blank(),                     
              panel.grid.major = element_blank(), 
              plot.background = element_blank(),                     
              plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
        geom_point(data = jhpoints,             
                   aes(x = x, y = y), size = 1,  
                   alpha = 1/5, color = "blue")+labs(title="Map for James Harden")
    }
  }
  )
  
  ##sentiment
  output$sentiment<-renderPlot({
    if(input$sentiinput=="Chris Paul"){
      bing_word_counts%>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()+labs(title="Sentiment Analysis for Chris Paul")
    }
    else{
      bing_word_counts1%>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment",
             x = NULL) +
        coord_flip()+labs(title="Sentiment Analysis for James Harden")
    }
  }
  )
  ##word cloud
  output$wordcloud<-renderPlot({
    if(input$wordinput=="Chris Paul"){
      wordcloud(cp3_corpus,random.order = F,max.words = 40,scale = c(3,0.5),colors = rainbow(50))
    }
    else{
      wordcloud(jh_corpus,random.order = F,max.words = 40,scale = c(3,0.5),colors = rainbow(50))
    }
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

  
             