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
rank <- read.csv("./Data/College ranking.csv", stringsAsFactors = F, na.strings = c("NA", ""))
GDP <- read.csv("./Data/Country GDP.csv", stringsAsFactors = F, na.strings = c("NA",""))
data <- read.csv("./Data/edu_var.csv", stringsAsFactors = F, na.strings = c("NA",""))
expend <- read.csv("./Data/Government expenditure.csv", stringsAsFactors = F, na.strings = c("NA",""))
rank_2012 <- rank[rank$year == "2012",]
rank_2013 <- rank[rank$year == "2013",]
rank_2014 <- rank[rank$year == "2014",]
rank_2014 <- rank_2014[1:100,]
rank_2015 <- rank[rank$year == "2015",]
rank_2015 <- rank_2015[1:100,]
rank_total1 <- merge(rank_2012, rank_2013, by = "institution")
rank_total2 <- merge(rank_2014, rank_2015, by = "institution")
rank_total <- merge(rank_total1, rank_total2, by = "institution")
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
score$avesc <- apply(score[2:5], 1, mean)
score.GPI <- merge(score, data[data$topic == "School enrollment, tertiary (gross), gender parity index (GPI)",], by = "country")
score.GPI$avescG <- apply(score.GPI[8:11], 1, mean)
year.GPI <- data.frame(c(1:48),c(1:48))
year.GPI[1:12,1] <- score.GPI[,2]
year.GPI[13:24,1] <- score.GPI[,3]
year.GPI[25:36,1] <- score.GPI[,4]
year.GPI[37:48,1] <- score.GPI[,5]
year.GPI[1:12,2] <- score.GPI[,8]
year.GPI[13:24,2] <- score.GPI[,9]
year.GPI[25:36,2] <- score.GPI[,10]
year.GPI[37:48,2] <- score.GPI[,11]
colnames(year.GPI) <- c("score","GPI")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("用數據看世界大學排名"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("dataselect",
                     "Data:",
                  choices = data$topic),
         
         hr(),
         helpText("選擇要看什麼數據和排名的關係")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <-  merge(score, data[data$topic == input$dataselect,], by = "country")
      x$avescG <- apply(x[8:10], 1, mean)
      
      # draw the histogram with the specified number of bins
      ggplot(x, aes(avesc, avescG)) + geom_point() + geom_smooth(method ="lm")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

