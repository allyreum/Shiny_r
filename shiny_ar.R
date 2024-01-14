#install.packages('shiny')
#install.packages('shinydashboard')
#install.packages('tidy')
library(shiny)
library(shinydashboard)
library(MASS)
library(tidyverse)
library(broom)

# 입출력위젯
ui <- fluidPage(
  numericInput("sel", "input value", value = 50, min = 40, max = 100),
  # numericInput(inputID, label, ~~~)
  textInput("mytext","텍스트를 입력하세요."),
  verbatimTextOutput("txt") # 텍스트 출력용 위젯. 없으면 출력상자 없음
)

# 서버코드
server <- function(input, output, session){
  output$txt <- renderPrint({
    req(input$mytext) # 결과창 같은 거?
    input$mytext
  })
}

########################################################
ui <- fluidPage(
  passwordInput("pw", "패스워드") # 입력창이 마스킹처리됨
)
server <- function(input, output, session){}
shinyApp(ui,server)

########################################################
ui <- fluidPage(
  numericInput("obs","observations:",10, 1,100),
  verbatimTextOutput("value"),
  sliderInput("Integer","sliderbar(Integer)", min = 0, max= 1000, value = 500) #슬라이더
)

server <- function(input, output, session){
  output$value <- renderText({input$obs})
}
shinyApp(ui,server)

########################################################
ui <- fluidPage(
  selectInput('sel1', "셀렉박스",
              choices = c('초급' = "beginner",
                          '중급' = "intermediate",
                          '고급' = 'advanced')),
  selectInput('sel2', "세부 셀렉박스",
              choices = list('컴파일 언어' = c('C++', 'JAVA'),
                             '스크립트 언어' = c('R', 'JavaScropt', 'Python'))
  )
)

server <- function(input, output, session){}
shinyApp(ui,server)
########################################################

ui <- fluidPage(
  radioButtons('dist', 'distribution type:',
               c('Normal' = 'norm', 'Uniform' = 'unif', 'Log-normal' = 'lnorm', 'Exponential' = 'exp')),
  # radioButtons( inputId,label,choices = NULL,
  #                         selected = NULL,inline = FALSE, width = NULL, choiceNames = NULL,choiceValues = NULL)
  plotOutput('distPlot')
)


server <- function(input,output){
  output$distPlot <- renderPlot({
    dist < - switch(input$dist, norm = rnorm, unif = runif, lnorm = rlnorm, exp = rexp, rnorm)
    # switch(statement, list) 사용자가 선택한값에 따라 계산하게 되는 경우에 주로 사용한다...!!!
    hist(dist(5300))
  })
}
shinyApp(ui,server)

########################################################
ui <- fluidPage(
  checkboxGroupInput('sels', '체크리스트',
                     c('유산균', '공부', '책읽기', '물 많이')
  ), # checkboxGroupInput()은 논리값으로 사용되지 않음
  verbatimTextOutput('langs'), #renderText로 만든 것을 출력할 때 textOutput으로 출력되는 거
  dateInput('date', '날짜선택', value = Sys.Date() , language = 'ko'),
  dateRangeInput('date', '날짜선택', 
                 start = Sys.Date() , end = Sys.Date() + 30, language =  "ko"),
  div(p('오늘 공부했나요?'), 
      checkboxInput('check', c('Yes'), value = TRUE))
)

server <- function(input, output){
  output$langs <- renderText({
    input$sels #input$date
  })
}

shinyApp(ui,server)

########################################################
ui <- fluidPage(
  sliderInput('obs', '# of obs', 0, 1000, 500),
  actionButton('goButton', 'Go!'), # observeEvent(), EventReactiv()와 함께 쓰임!
  plotOutput('distPlot')
)

server <- function(input, output){
  output$distPlot <- renderPlot({
    input$goButton #반응성 값이 변경되면 자동으로 재실행됨! 바뀌면 renderPlot 전체가 바뀜
    dist <- isolate(rnorm(input$obs))
    hist(dist)
  })
}
shinyApp(ui,server)

########################################################
ui<-fluidPage(
  h2(textOutput('txt1')), verbatimTextOutput('sum1'),
  h3(textOutput('txt2')), tableOutput('tab1')
  # h1~h6 헤더 크기(?)
)

server <- function(input, output){
  output$txt1 <- renderText({ 'linear regression by using mtcars data'})
  output$txt2 <- renderText({"Table 만들기"})
  output$sum1 <- renderPrint({ 
    summary(lm(mpg ~ wt + qsec, mtcars)) })
  # renderText는 텍스트 형태로 값을 표시하는 데 주로 사용되고
  # renderPrint는 더 구조화된 데이터나 객체를 출력하는 데 주로 사용
  output$tab1 <- renderTable({
    tidy(lm(mpg ~ wt + qsec, mtcars))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

shinyApp(ui,server)

# 표 출력 DataTables (By using JavaScript)  ##########################

shinyApp(
  ui <- fluidPage(
    fluidRow( #  UI에서의 행을 나타냄
      column(12, dataTableOutput('table')) # column의 공간을 12로 지정하고, 
      # table은 출력 객체의 이름을 나타내며, 서버 로직에서 해당 이름으로 데이터를 전달
    ) ),
  server <- function(input, output){
    output$table <- renderDataTable(iris, option = list(pageLength = 5, # pageLength)를 5로 설정
                                                        initComplete = I("function(settings, json) {alert('Done.');}"))
                                    #  테이블 초기화가 완료될 때 경고창을 띄움
    )  }
)

# Plot ##################################################
ui <- fluidPage(
  plotOutput('myPlot')
)

server <- function(input, output, session){
  output$myPlot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  })
}
shinyApp(ui,server)

# Insert Image ############################################
ui <- fluidPage(
  fluidRow(
    column(3, img(src = "quokka2.png", width = "100%")),
    column(9,
           sliderInput('obs', 'No. of Random Numbers', min = 30, max = 100, value = 50, width = '100%'),
           plotOutput('myplot', width ='100%'))
  )
)

server <- function(input, output, session){
  output$myplot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

shinyApp(ui, server)

