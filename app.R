# SnowNotice applications

library(shiny)
library(shinydashboard)
library(RMySQL)
library(wordcloud)
library(KoNLP)
useSejongDic()

ui = dashboardPage(skin="blue",
                   dashboardHeader(title = "Tweet DB"),
                   dashboardSidebar(
                     dateRangeInput(inputId="dateRange", label="시작날짜 지정:",
                                    start=Sys.Date()-2, end=Sys.Date(),
                                    separator="-", format="yyyy-mm-dd",
                                    startview="year", language="en", 
                                    weekstart=1),
                     actionButton(inputId="action",
                                  label="Apply Changes")
                   ),
                   dashboardBody(
                     tabBox(
                       tabPanel("Sentimetal Analysis", "Analysis: ", tableOutput("tb")),
                       tabPanel("Raw data", tableOutput("ts"))
                     )
                     )
)

server = function(input, output) {
  df = reactive({
    input$action
    con = dbConnect(MySQL(), host='203.252.196.68',
                    dbname='sql1714670', user='db1714670', pass='stat1234')
    query2 = sprintf("SELECT * FROM TweetMetadata 
                     WHERE CreatedTime
                     BETWEEN STR_TO_DATE(\'%s\', \'%%Y-%%m-%%d\')
                     AND STR_TO_DATE(\'%s\', \'%%Y-%%m-%%d\') ;
                     ", as.character(as.Date(isolate(input$dateRange[1])), origin="1970-01-01"), 
      as.character(as.Date(isolate(input$dateRange[2])), origin="1970-01-01") )
    res <- dbGetQuery(con, statement = query2)  
    res <- res[!is.na(res$Text),]
    res$Text <- iconv(as.character(res$Text), from="UTF-8")
    
    dbDisconnect(con)
    return(res)
  }) 
  output$ts = renderTable({
    df()
  })
  output$tb = renderTable({
    tweet_db <- df()
    
    library(KnuSentiLexR)
    library(dplyr)
    library(tidytext)
    tweet_db %>%
      select(Text, Source) -> tar
    
    tar %>%
      unnest_tokens(sent, Text, token = "sentences") %>%
      filter(nchar(sent) < 20) %>%
      select(sent) ->
      senti_tar
    
    senti_tar %>%
      mutate(score = senti_score(sent),
             magni = senti_magnitude(sent))%>%
      filter(score != 0)
  })
}

shinyApp(ui = ui, server = server)

