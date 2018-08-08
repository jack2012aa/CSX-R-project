library(shiny)
library(markdown)
library(ggplot2)
library(png)
rank <- read.csv("./Data/College ranking.csv", stringsAsFactors = F, na.strings = c("NA", ""))
data <- read.csv("./Data/edu_var.csv", stringsAsFactors = F, na.strings = c("NA",""))
College_ranking100 <- read.csv("./Data/College_ranking100.csv")
College_ranking <- read.csv("./Data/College ranking.csv")
rank_2012 <- rank[rank$year == "2012",]
rank_2013 <- rank[rank$year == "2013",]
rank_2014 <- rank[rank$year == "2014",]
rank_2014 <- rank_2014[1:100,]
rank_2015 <- rank[rank$year == "2015",]
rank_2015 <- rank_2015[1:100,]
countrymean <- function(x,y,z){
  eval.parent(substitute(z[z$country == y, paste("x",x[1,"year"], sep = "")] <- mean(x[x$country == y,2])))
}
country <- c("Australia", "Canada", "Denmark", "France", "Germany", 'Israel','Japan','Netherlands','South Korea','Sweden','Switzerland','United Kingdom','USA')
x2012 <- c(1:13)
x2013 <- c(1:13)
x2014 <- c(1:13)
x2015 <- c(1:13)
score <- data.frame(country, x2012, x2013,x2014,x2015)
score_2012 <- as.data.frame(rank_2012[,c(3,13,14)])
score_2013 <- as.data.frame(rank_2013[,c(3,13,14)])
score_2014 <- as.data.frame(rank_2014[,c(3,13,14)])
score_2015 <- as.data.frame(rank_2015[,c(3,13,14)])
countrymean(score_2012, "USA", score)
countrymean(score_2013, "USA", score)
countrymean(score_2014, "USA", score)
countrymean(score_2015, "USA", score)
countrymean(score_2012, "Australia", score)
countrymean(score_2013, "Australia", score)
countrymean(score_2014, "Australia", score)
countrymean(score_2015, "Australia", score)
countrymean(score_2012, "Canada", score)
countrymean(score_2013, "Canada", score)
countrymean(score_2014, "Canada", score)
countrymean(score_2015, "Canada", score)
countrymean(score_2012, "Denmark", score)
countrymean(score_2013, "Denmark", score)
countrymean(score_2014, "Denmark", score)
countrymean(score_2015, "Denmark", score)
countrymean(score_2012, "France", score)
countrymean(score_2013, "France", score)
countrymean(score_2014, "France", score)
countrymean(score_2015, "France", score)
countrymean(score_2012, "Germany", score)
countrymean(score_2013, "Germany", score)
countrymean(score_2014, "Germany", score)
countrymean(score_2015, "Germany", score)
countrymean(score_2012, "Israel", score)
countrymean(score_2013, "Israel", score)
countrymean(score_2014, "Israel", score)
countrymean(score_2015, "Israel", score)
countrymean(score_2012, "Japan", score)
countrymean(score_2013, "Japan", score)
countrymean(score_2014, "Japan", score)
countrymean(score_2015, "Japan", score)
countrymean(score_2012, "Netherlands", score)
countrymean(score_2013, "Netherlands", score)
countrymean(score_2014, "Netherlands", score)
countrymean(score_2015, "Netherlands", score)
countrymean(score_2012, "South Korea", score)
countrymean(score_2013, "South Korea", score)
countrymean(score_2014, "South Korea", score)
countrymean(score_2015, "South Korea", score)
countrymean(score_2012, "Sweden", score)
countrymean(score_2013, "Sweden", score)
countrymean(score_2014, "Sweden", score)
countrymean(score_2015, "Sweden", score)
countrymean(score_2012, "Switzerland", score)
countrymean(score_2013, "Switzerland", score)
countrymean(score_2014, "Switzerland", score)
countrymean(score_2015, "Switzerland", score)
countrymean(score_2012, "United Kingdom", score)
countrymean(score_2013, "United Kingdom", score)
countrymean(score_2014, "United Kingdom", score)
countrymean(score_2015, "United Kingdom", score)

ui <- navbarPage(h3("大學排名"),
  tabPanel(
    h3("Task"),
    fluidRow(
    column(12,
    pre(h2(includeText("./Data/intro.txt"))),
    br()
    ))
  ),
  tabPanel(
    h3("EDA"),
    fluidRow(
      column(12,
             pre(h2(includeText("./Data/EDA.txt"))),
             br()
      ))
  ),
  
  navbarMenu(h3("大學排名"),
             tabPanel(h3("Explore the Data"),
                      
                      fluidRow(
                        column(4,
                               selectInput("country",
                                           "Country:",
                                           c("All",
                                             unique(as.character(College_ranking100$country))))
                        ),
                        column(4,
                               selectInput("institution",
                                           "Institution:",
                                           c("All",
                                             unique(as.character(College_ranking100$institution))))
                        ),
                        column(4,
                               selectInput("year",
                                           "Year:",
                                           c("All",
                                             unique(as.character(College_ranking100$year))))
                        )
                      ),
                      fluidRow(dataTableOutput("table2")),
                      tabPanel("Explore the Data 2",
                               
                               sidebarLayout(
                                 sidebarPanel(
                                   conditionalPanel(
                                     'input.dataset === "College_ranking"',
                                     checkboxGroupInput("show_vars", "Columns in College_ranking to show:",
                                                        names(College_ranking), selected = names(College_ranking))
                                   )
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     id = 'dataset',
                                     tabPanel("College_ranking", dataTableOutput("mytable1"))
                                   )
                                 )
                               )
                      )),   
             
             tabPanel(h3("大學排名"),
                      titlePanel("看看前100大學排名"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("datayear","Data:",choices = c(2012,2013,2014,2015)),
                          hr(),
                          helpText(h3("各國大學成績分佈-按年份"))
                        ),
                        mainPanel(
                          plotOutput("Country"),
                          h3("可見除了美國以外成績分佈都還算平均,所以各國在我們選擇的時間內的代表分數就用平均分計算")
                        ))),
             
             tabPanel(h3("Maps"),
                      
                      fluidRow( 
                        column(4, wellPanel(
                          radioButtons("picture", "Maps:",
                                       c("USA", "UK","Taiwan","Switzerland","Sweden","South Korea","Singapore","Russia","Norway","Netherlands","Japan","Israel","Germany","France","Denmark","China","Canada","Belgium","Australia"))
                        )),
                        column(4,
                               imageOutput("image"))))
             
             
             ),
                      
  navbarMenu(h3("各國分數的回歸"),
    tabPanel(h3("資料"),
      titlePanel("先來看看要比較的資料"),
      fluidRow(
        column(4,
          selectInput("choice", "Data:", choices = c("College ranking","edu_var"))
    )),
      dataTableOutput("table")
    ),
    
    tabPanel(h3("回歸"),
      titlePanel("看看各資料和各國分數的回歸"),       
      sidebarLayout(
        sidebarPanel(
          selectInput("dataselect","Data:",choices = data$topic),
          hr(),
          helpText("選擇要看什麼數據和排名的關係"),
          sliderInput("range", "Range:",min = 2012, max = 2015, value = c(2012,2014)),
          hr(),
          helpText("想看的年份"),
          checkboxInput("checkbox", label = "平均", value = TRUE),
          hr()
        ),
        mainPanel(plotOutput("distPlot"),verbatimTextOutput("summary")))
        )
    ),
  tabPanel(h3("Summary"),
           h3("Summary"),
           fluidRow(
             column(12,
                    pre(h2(includeText("./Data/end.txt"))),
                    br()
             ))
  ))
    
server <- function(input, output,session) {
  output$distPlot <- renderPlot({
      if (input$checkbox){
        year <-  merge(score, data[data$topic == input$dataselect,], by = "country")
        Value <- input$range - c(2010,2010)
        Value_ <- Value + c(5,5)
        year$score <- apply(year[Value[1]:Value[2]], 1, mean)
        year$score_ <- apply(year[Value_[1]:Value_[2]], 1, mean)
      } else {
        x <-  merge(score, data[data$topic == input$dataselect,], by = "country")
        Value <- input$range - c(2010,2010)
        yearnum <- Value[1]:Value[2]
        subs <- 13*length(yearnum)
        year <- data.frame(c(1:subs), c(1:subs),as.character(eval(c(1:subs))), stringsAsFactors=FALSE)
        count <- 1
        for(n in yearnum){
        rown1 <- 13*count - 12
        rown2 <- 13*count
        year[rown1 : rown2,1] <- x[,n]
        year[rown1 : rown2,2] <- x[,n + 5]
        year[rown1 : rown2,3] <- as.character(x$country)
        count <- count + 1
        }
        colnames(year) <- c("score","score_", "country")
      }
    ggplot(year, aes(score_,score)) + geom_point() +geom_smooth(method = "lm") + labs(x = input$dataselect, y = "score") + geom_text(aes(label = country, vjust = 1.1))
  })
  
  output$Country <- renderPlot({
    rankplot <- rank[rank$year == input$datayear,]
    rankplot <- rankplot[1:100,]
    ggplot(rankplot, aes(x = score)) + geom_histogram() + facet_grid(~country) + theme_bw() + stat_bin(bins = 100)
  })
  
  output$summary <- renderPrint({
    if (input$checkbox){
      year <-  merge(score, data[data$topic == input$dataselect,], by = "country")
      Value <- input$range - c(2010,2010)
      Value_ <- Value + c(5,5)
      year$score <- apply(year[Value[1]:Value[2]], 1, mean)
      year$score_ <- apply(year[Value_[1]:Value_[2]], 1, mean)
    } else {
      x <-  merge(score, data[data$topic == input$dataselect,], by = "country")
      Value <- input$range - c(2010,2010)
      subs <- 13*length(Value)
      yearnum <- Value[1]:Value[2]
      year <- data.frame(c(1:subs), c(1:subs))
      count <- 1
      for(n in yearnum){
        rown1 <- 13*count - 12
        rown2 <- 13*count
        year[rown1 : rown2,1] <- x[,n]
        year[rown1 : rown2,2] <- x[,n + 5]
        colnames(year) <- c("score","score_")
        count <- count + 1
      }
    }
    summary(lm(score_~score,data = year), data = year)
  })
  
  output$table <- renderDataTable({
    if (input$choice == "Collage ranking"){
      show <- rank
    }
    if (input$choice == "edu_var"){
      show <- data
    }
    show
  })
  
  College_ranking1002 = College_ranking100[sample(nrow(College_ranking100),400), ]
  
  output$mytable1 <- renderDataTable({
    College_ranking1002[, input$show_vars, drop = FALSE]
  })
  
  output$table2 <- renderDataTable({
    data <- College_ranking100
    if (input$country != "All") {
      data <- data[data$country == input$country,]
    }
    if (input$institution != "All") {
      data <- data[data$institution == input$institution,]
    }
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    data
  })
  
  output$image <- renderImage({
    if (is.null(input$picture))
      return(NULL)
    
    if (input$picture == "USA") {
      return(list(
        src = "./Data/USAmap.png",
        contentType = "image/png",
        alt = "USA"
      ))
    } else if (input$picture == "UK") {
      return(list(
        src = "./Data/UKmap.png",
        filetype = "image/png",
        alt = "UK"
      ))
    } else if (input$picture == "Taiwan") {
      return(list(
        src = "./Data/Taiwanmap.png",
        filetype = "image/png",
        alt = "Taiwan"
      ))
    } else if (input$picture == "Switzerland") {
      return(list(
        src = "./Data/Switzerlandmap.png",
        filetype = "image/png",
        alt = "Switzerland"
      ))
    } else if (input$picture == "Sweden") {
      return(list(
        src = "./Data/Swedenmap.png",
        filetype = "image/png",
        alt = "Sweden"
      ))
    } else if (input$picture == "South Korea") {
      return(list(
        src = "./Data/SouthKoreamap.png",
        filetype = "image/png",
        alt = "South Korea"
      ))
    } else if (input$picture == "Singapore") {
      return(list(
        src = "./Data/Singaporemap.png",
        filetype = "image/png",
        alt = "Singapore"
      ))
    } else if (input$picture == "Russia") {
      return(list(
        src = "./Data/Russiamap.png",
        filetype = "image/png",
        alt = "Russia"
      ))
    } else if (input$picture == "Norway") {
      return(list(
        src = "./Data/Norwaymap.png",
        filetype = "image/png",
        alt = "Norway"
      ))
    } else if (input$picture == "Netherlands") {
      return(list(
        src = "./Data/Netherlandsmap.png",
        filetype = "image/png",
        alt = "Netherlands"
      ))
    } else if (input$picture == "Japan") {
      return(list(
        src = "./Data/Japanmap.png",
        filetype = "image/png",
        alt = "Japan"
      ))
    } else if (input$picture == "Israel") {
      return(list(
        src = "./Data/Israelmap.png",
        filetype = "image/png",
        alt = "Israel"
      ))
    } else if (input$picture == "Germany") {
      return(list(
        src = "./Data/Germanymap.png",
        filetype = "image/png",
        alt = "Germany"
      ))
    } else if (input$picture == "France") {
      return(list(
        src = "./Data/Francemap.png",
        filetype = "image/png",
        alt = "France"
      ))
    } else if (input$picture == "Denmark") {
      return(list(
        src = "./Data/Denmarkmap.png",
        filetype = "image/png",
        alt = "Denmark"
      ))
    } else if (input$picture == "China") {
      return(list(
        src = "./Data/Chinamap.png",
        filetype = "image/png",
        alt = "China"
      ))
    } else if (input$picture == "Canada") {
      return(list(
        src = "./Data/Canadamap.png",
        filetype = "image/png",
        alt = "Canada"
      ))
    } else if (input$picture == "Belgium") {
      return(list(
        src = "./Data/Belgiummap.png",
        filetype = "image/png",
        alt = "Belgium"
      ))
    } else if (input$picture == "Australia") {
      return(list(
        src = "./Data/Australiamap.png",
        filetype = "image/png",
        alt = "Australia"
      ))
    }
    
  }, deleteFile = FALSE)
  }

# Run the application 
shinyApp(ui = ui, server = server)

