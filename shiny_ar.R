# install.packages('shiny')
library(shiny)
library(shinydashboard)
library(MASS)
library(tidyverse)
library(broom)
library(ggplot2)
library(stringr)

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
    column(3, img(src = "quokka2.png", width = "100%")), #주소때문인가?
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


# conditionalPanel ##########################################
ui <- fluidPage(
  titlePanel("diamonds 데이터"),
  selectInput(
    "plotType", "Plot Type", c(Scatter = "scatter", Histogram = "hist")),
  conditionalPanel( # 참일 때만 실행됨 , $를 쓰는 거 처럼 java에서는 .으로 이용
    condition = "input.plotType == 'hist'", #  hist로 선택되었을 때만 실행되는 조건 패널
    selectInput( "breaks","Breaks", 
      c("Sturges", "Scott", "Freedman-Diaconis","[Custom]" = "custom")),
    conditionalPanel(
      condition = "input.breaks == 'custom'",
      sliderInput("breakCount","Break Count", min = 1, max = 1000, value = 10) 
      )
    ),
  plotOutput("plot") # 플롯이 표시될 출력 영역 정의
)

server <- function(input, output, session){
  brs <- reactive({ # reactive 입력 값에 따라 동적으로 반응하는 리액티브 객체
    if(input$breaks == "custom"){ # break에 대한 조건문
      input$breakCount
    } else{
      input$breaks
    }
  })
  p <- reactive({ # plotType에 대한 조건문(scatter인데 breaks가 왜 나오나 했네..)
    if(input$plotType == "scatter"){
      plot(diamonds$carat, diamonds$price, col = "red")
      } else{ 
        hist(diamonds$carat, breaks = brs())
        }
    })
  output$plot <- renderPlot({
    p()
    })
}

shinyApp(ui, server)

# 6-2. renderUI(), uiOutput() ##############################
ui <- fluidPage(
  uiOutput("moreControls") #  동적으로 UI를 추가할 수 있는 영역을 추가(?)
)

server <- function(input, output){
  output$moreControls <- renderUI({ 
    tagList(
      sliderInput("n","N",1,1000,500), textInput("label", "Label")
    )
  })
}
shinyApp(ui, server)

##
ui <- fluidPage(
  titlePanel("Which one would you like to choose?"),
  radioButtons("selected", "table or plot",
               choices = list("table", "plot")),
  uiOutput("tbl2"),
  uiOutput("plot2")
  )

server <- function(input, output, session){
  output$tbl <- renderTable({ mtcars })
  output$tbl2 <- renderUI({
    if(input$selected == "table"){
      tableOutput("tbl")
    }
  })
  output$plt <- renderPlot({ plot(mtcars$wt, mtcars$mpg) })
  output$plot2 <- renderUI({
    if(input$selected == "plot") {
      plotOutput("plt")
    }
  })
}
shinyApp(ui, server)

# 6-3 UI 삽입/삭제#######################################3
# INSERT
ui <- fluidPage(
  actionButton("add","Add UI")
)
server <- function(input, output, session){
  observeEvent(input$add,{
    insertUI(
      selector = "#add", #삽입한 UI의 위치를 정하는 기준이 되는 인수
      where = "afterEnd", # default = beforeEnd
      ui = textInput(paste0("txt", input$add), "Insert some text") # text widget
    )
  })
}
shinyApp(ui, server)

# DELETE
ui <- fluidPage(
  actionButton("rmv", "Remove UI"),
  textInput("txt", "This is no longer useful")
)
server <- function(input, output, session){
  observeEvent(input$rmv,{
    removeUI(
      selector = "div:has(>#txt)" 
      # div:has(>#txt)는 <div> 요소들을 선택하는데, 그 자식요소가 txt라는 아이디(#)를 가진 것을 선택함(jQuery의 콘텐츠 필터)
    )
  })
}
shinyApp(ui, server)

# 6-4 insertUI, removeUI ################################
ui <- fluidPage(
  actionButton("add","Add UI"),
  verbatimTextOutput("allText")
)

server <- function(input, output, session){
  observeEvent(input$add,{
    insertUI( select = "#add", where = "afterEnd",
              ui = textInput(paste0("txt", input$add),
                             "Insert some text", placeholder = "문자를 입력하세요.")
              # textInput의 아이디는 "txt1", "txt2", ...와 같이 순차적으로 생성
              )
  } )
  
  output$allText <- renderPrint({
    # output$allText <- renderPrint({...}): 동적으로 추가된 모든 텍스트 입력 상자에 입력된 내용 출력
    req(input$add)
    txts <- unlist(lapply(seq(1, input$add), function(x) paste0("txt",x)))
    paste(unlist(lapply(txts, function(x) str_trim(input[[x]]))), collapse = " ")
  })
}
shinyApp(ui, server)

# chapter 7 ###############################
ui <- fluidPage(
  textInput("myText", "텍스트 입력"),
  textOutput("result") #c
)

server <- function(input, output, session){
  output$result <- renderText({
    input$myText
  })
}
shinyApp(ui, server) # 입력한 값이 그대로 출력됨

# 3-2. 반응성 맥락 > render(), reactive(), observe()
myData <- mtcars
ui <- fluidPage(
  numericInput('sel', 'mtcars 데이터 행의 개수 입력', value = 7, min =6, max = nrow(myData)),
  tableOutput('input_data_name')
)

server <- function(input, output, session){
  output$input_data_name <- renderTable({ # 반응성 맥락
    head(myData, n = input$sel) # value = n
  })
}
shinyApp(ui, server) 

ui <- fluidPage(
  numericInput('sel', 'mtcars 데이터 행의 개수 입력', value = 6, min =6, max = nrow(myData)),
  tableOutput('carData')
)
server <- function(input, output, session){
  output$carData <- renderTable({
    head(myData, input$sel)
  })
  tryCatch({ # 비반응성 맥락
    x <- input$sel # 입력값을 변경하면서 발생할 수 있는 예외를 처리하기 위해 tryCatch 이용
  }, error = function(c) cat(c$message)) # cat(c$message)는 개발자에게는 콘솔을 통해 예외 메시지를 확인
}

# minimum limit
server <- function(input, output, session){
  output$carData <- renderTable({
    if (input$sel < 6) {
      cat("선택 가능한 최소값은 6입니다.\n")
      return(NULL) # 사용자에게는 안보임
    }
    head(myData, input$sel)
  })
}

shinyApp(ui, server)

#3-4. 반응성 표현식 ###############################
ui <- fluidPage(
  titlePanel("간단한 계산을 위한 앱"),
  wellPanel(p("숫자 2개와 계산법을 선택하세요.")),
  textInput("num1","첫번째 숫자"),
  textInput("num2","두번째 숫자"),
  radioButtons("cal", "계산법의 선택", c("+"="더하기", "-"="빼기", "*"="곱하기", "/"="나누기"),
               inline=TRUE),
  h2("결과"),
  textOutput("result")
)

server <- function(input, output,session){
  outputTxt <- reactive({
    req(input$num1, input$num2) # NULL이 아닌지, 비어있지 않은지>> null이면 실행안됨
    num1 <- as.numeric(input$num1)
    num2 <- as.numeric(input$num2)
    switch(input$cal, # 함수 반환값
           "더하기" = {paste(num1, "+", num2, "=", num1+num2)},
           "빼기" = {paste(num1, "-", num2, "=", num1-num2)},
           "곱하기" = {paste(num1, "*", num2, "=", num1*num2)},
           "나누기" = {paste(num1, "/", num2, "=", round(num1/num2,2))}
           )
  })
  output$result <- renderText({
    outputTxt() # reactive함수 호출.  outputTxt의 값을 얻어와서 output$result를 업데이트하라는 의미
  })
}
shinyApp(ui,server)

# 3-5. reactive observer ##########################
ui <- fluidPage(
  titlePanel("계산기 app"),
  wellPanel(p("숫자2개와 계산법을 선택하세요.")),
  textInput("obs1","숫자1"),
  textInput("obs2","숫자2"),
  radioButtons("cal", "계산법의 선택",c("+"="더하기", "-"="빼기", "*"="곱하기", "/"="나누기"),
               inline = TRUE),
  h2("결과"),
  textOutput("result") )

server <- function(input, output, session){
  outputTxt <- reactive({
    req(input$obs1, input$obs2)
    obs1 <- as.numeric(input$obs1)
    obs2 <- as.numeric(input$obs2)
    switch(input$cal, 
           "더하기" = {paste(obs1, "+", obs2, "=", obs1+obs2)},
           "빼기" = {paste(obs1, "-", obs2, "=", obs1-obs2)},
           "곱하기" = {paste(obs1, "*", obs2, "=", obs1*obs2)},
           "나누기" = {paste(obs1, "/", obs2, "=", round(obs1/obs2,2))} )
  })
  output$result <- renderText({
    outputTxt()
  })
  observe({ # console이 안나오는데...^_ㅜ
    cat(outputTxt(),"\n")
    # showNotification(outputTxt())
  })
}
shinyApp(ui,server)

# 4. 무효화(invalidation) ########################################
values <- reactiveValues(a=3) # reactiveValues()눈 반응성 값을 만든다!!+ values$a
isolate(values$a)
b <- isolate(values$a + 4)
isolate(b)

values$a <- 5
isolate(values$a)
isolate(b) # 9가 아닌 7의 값을 갖음
# 유효화 상태: values$a 값이 반영되어 객체 b의 값이 확정된 상태
#무효화 상태:  values$a 값이 변경되었지만, 아직 values$a + 4가 실행되지 않아서 객체b의 값이 확정되지 못한 상태

# how to automatically make  invalidation > validation? by reactive()
values2 <- reactiveValues(aa =3)
isolate(values2$aa)
bb <- reactive({
  values2$aa + 4
})
isolate(bb())

obsB <- observe({
  print(values2$aa + 4)
})
shiny:::flushReact()
