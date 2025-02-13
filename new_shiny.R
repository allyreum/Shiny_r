library(shiny)

# UI 정의
ui <- fluidPage(
  titlePanel("Hello, Shiny!"),
  sidebarLayout(
    sidebarPanel( # 사용자가 값을 입력하면 input$ID를 통해 서버에서 접근
      textInput("name", "이름을 입력하세요:", "홍길동") 
      # 숫자 입력
      # numericInput("age", "나이를 입력하세요:", value = 30, min = 1, max = 100, step = 1)
      # 슬라이더
      # sliderInput("range", "값 선택:", min = 0, max = 100, value = 50)
      # 체크박스
      # checkboxInput("agree", "동의함", value = FALSE)
      # 라디오 버튼
      # radioButtons("gender", "성별 선택:", choices = c("남성", "여성"))
      # 드롭다운
      # selectInput("fruit", "과일 선택:", choices = c("사과", "바나나", "오렌지"))
      # 파일 업로드
      # fileInput("file", "파일 업로드")
    ),
    mainPanel(
      textOutput("greeting") # 텍스트 출력 (textOutput)
    )
  )
)

# Server 정의
server <- function(input, output) {
  output$greeting <- renderText({
    paste("안녕하세요,", input$name, "님!")
    # tableOutput("table")
    # 동적 테이블 dataTableOutput("datatable")
  })
}

# 앱 실행
shinyApp(ui = ui, server = server)

# 연령대 치매환자 수
ui <- fluidPage(
  titlePanel("연령대별 치매 환자 수 (인터랙티브)"),
  sidebarLayout(
    sidebarPanel( selectInput("year", "연도 선택:", choices = unique(dementia$year))  ),
    mainPanel( plotlyOutput("demen_plot") ) # plotOutput → plotlyOutput 변경
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    dementia %>% 
      filter(age %in% c('60~64세', '65~69세', '70~74세', '75~79세', '80~84세', '85세이상')) %>%
      filter(year == input$year) # 선택한 연도의 데이터 필터링
    })
  output$demen_plot <- renderPlotly({ # 인터랙티브 그래프 출력
    p <- ggplot(filtered_data(), aes(x = age, y = demen, fill = age, text = paste("환자 수:", demen))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste(input$year, "년 연령대별 치매 환자 수"),
           x = "연령대", y = "치매 환자 수") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")  # 마우스 오버 시 툴팁 표시
  })
}

shinyApp(ui = ui, server = server)


