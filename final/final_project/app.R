library(shiny)
library(markdown)
library(ggplot2)
rank <- read.csv("./Data/College ranking.csv", stringsAsFactors = F, na.strings = c("NA", ""))
GDP <- read.csv("./Data/Country GDP.csv", stringsAsFactors = F, na.strings = c("NA",""))
data <- read.csv("./Data/edu_var.csv", stringsAsFactors = F, na.strings = c("NA",""))
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

ui <- navbarPage("大學排名",
  tabPanel(
    "Introduction",
    tags$h1("")
  ),
                                
  navbarMenu("H",
    tabPanel("Data",
      fluidRow(
        column(4,
          selectInput("choice", "Data:", choices = c("College ranking","edu_var"))
    )),
      dataTableOutput("table")
    ),
             
    tabPanel("2",
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
    
    tabPanel("3",
     titlePanel("用數據看世界大學排名"),
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
        mainPanel(
          plotOutput("distPlot")
      ))),
           
    tabPanel("Summary",
      titlePanel("經過各種回歸後..."),
      sidebarLayout(
       sidebarPanel(
         selectInput("dataselectreg","Data:",choices = data$topic),
         hr(),
         helpText("選擇要看什麼數據和排名的回歸"),
         sliderInput("rangereg", "Range:",min = 2012, max = 2015, value = c(2012,2014)),
         hr(),
         helpText("想看的年份"),
         checkboxInput("checkboxreg", label = "平均", value = TRUE),
         hr()
       ),
       mainPanel(
         verbatimTextOutput("summary")
       )))
))
    
server <- function(input, output,session) {
  output$distPlot <- renderPlot({
      if (input$checkbox){
        year <-  merge(score, data[data$topic == input$dataselect,], by = "country")
        Value <- input$range - c(2004,2004)
        year$score <- apply(year[2:5], 1, mean)
        year$score_ <- apply(year[Value[1]:Value[2]], 1, mean)
      } else {
        x <-  merge(score, data[data$topic == input$dataselect,], by = "country")
        Value <- input$range - c(2010,2010)
        subs <- 12*length(Value)
        yearnum <- Value[1]:Value[2]
        year <- data.frame(c(1:subs), c(1:subs))
        count <- 1
        for(n in yearnum){
        rown1 <- 12*count - 11
        rown2 <- 12*count
        year[rown1 : rown2,1] <- x[,n]
        year[rown1 : rown2,2] <- x[,n + 5]
        colnames(year) <- c("score","score_")
        count <- count + 1
        }
      }
    ggplot(year, aes(score_,score)) + geom_point() +geom_smooth(method = "lm") + labs(x = input$dataselect, y = "score")
  })
  output$Country <- renderPlot({
    rankplot <- rank[rank$year == input$datayear,]
    rankplot <- rankplot[1:100,]
    ggplot(rankplot, aes(x = score)) + geom_histogram() + facet_grid(~country) + theme_bw() + stat_bin(bins = 100)
  })
  output$summary <- renderPrint({
    if (input$checkboxreg){
      year <-  merge(score, data[data$topic == input$dataselectreg,], by = "country")
      Value <- input$rangereg - c(2004,2004)
      year$score <- apply(year[2:5], 1, mean)
      year$score_ <- apply(year[Value[1]:Value[2]], 1, mean)
    } else {
      x <-  merge(score, data[data$topic == input$dataselectreg,], by = "country")
      Value <- input$rangereg - c(2010,2010)
      subs <- 12*length(Value)
      yearnum <- Value[1]:Value[2]
      year <- data.frame(c(1:subs), c(1:subs))
      count <- 1
      for(n in yearnum){
        rown1 <- 12*count - 11
        rown2 <- 12*count
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
  }

# Run the application 
shinyApp(ui = ui, server = server)

