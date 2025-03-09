library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(writexl)

# UI 정의
ui <- fluidPage(
  titlePanel("Shiny 그만 만나!"),
  sidebarLayout(
    sidebarPanel( # 사용자가 값을 입력하면 input$ID를 통해 서버에서 접근
      textInput("text", "지금하고 싶은 말은:", "집에 가고 싶어요") 
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
    paste("안녕하세요,", input$text, "!!!!")
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
    dementia %>% filter(sido == sigungu & sex == '전체' & sido != '전국') %>%
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


ui <- fluidPage(
  titlePanel("시도별 연령대별 치매 환자 수 히스토그램"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sido", "시도 선택:", choices = unique(dementia$sido[dementia$sido != "전국"])),
      selectInput("year", "연도 선택:", choices = unique(dementia$year)) 
    ),
    # mainPanel( plotOutput("age_histogram")  ) # 히스토그램 출력
    mainPanel( 
      plotlyOutput("age_histogram") , 
      DTOutput('data_table'),
      downloadButton("download_excel", "엑셀 다운로드")  # 다운로드 버튼 추가
      ) # 마우스 오버: plotOutput -> plotlyOutput 변경
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    dementia %>% filter(sido == sigungu & sex == '전체' & sido != '전국') %>%
      filter(age %in% c('60~64세', '65~69세', '70~74세', '75~79세', '80~84세', '85세이상')) %>%
      filter(sido == input$sido, year == input$year)  # 시도와 연도에 맞는 데이터 필터링
  })
  
  # 히스토그램 출력
  output$age_histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = age, y = demen, fill = age)) +
      geom_bar(stat = "identity") +  # 히스토그램
      theme_minimal() +
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000, suffix = "천명")) +  # y축 1000명 단위로 표시
      labs(title = paste(input$sido, "의 연령대별 치매 환자 수"),
           x = "연령대", y = "치매 환자 수 (천명)") +
      theme(legend.position = "none")  # 범례 제거
  })
}



# 시군구 선택 할 때 종속성 없이 전체의 시군구가 보임
ui <- fluidPage(
  titlePanel("시도별 연령대별 치매 환자 수 히스토그램"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sido", "시도 선택:", choices = unique(dementia$sido[dementia$sido != "전국"])),
      selectInput("sigungu", "시군구 선택:", choices = unique(dementia$sigungu[dementia$sigungu != "전국" & 
                                                                            dementia$sido != dementia$sigungu])),
      selectInput("year", "연도 선택:", choices = unique(dementia$year)) 
    ),
    # mainPanel( plotOutput("age_histogram")  ) # 히스토그램 출력
    mainPanel( 
      plotlyOutput("age_histogram") , 
      DTOutput('data_table'),
      downloadButton("download_excel", "엑셀 다운로드")  # 다운로드 버튼 추가
    ) # 마우스 오버: plotOutput -> plotlyOutput 변경
  )
)


server <- function(input, output) {
  filtered_data <- reactive({
    dementia %>% filter(sex == '전체' & sido != '전국') %>%
      filter(age %in% c('60~64세', '65~69세', '70~74세', '75~79세', '80~84세', '85세이상')) %>%
      filter(sido == input$sido, sigungu == input$sigungu, year == input$year)
  })
  
  # 히스토그램 출력 (Plotly 적용)
  output$age_histogram <- renderPlotly({
    p <- ggplot(filtered_data(),
                aes(x = age, y = demen, fill = age
                    , text = paste("연령대:", age, "<br>환자 수:", scales::comma(demen), "명"))) +
      geom_bar(stat = "identity", width = 0.7) +  # 바 너비 조정
      theme_minimal() +  # 깔끔한 테마 적용
      scale_fill_brewer(palette = "Set2") +  # 예쁜 색상 적용
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000, suffix = "천 명")) + #단위
      labs(title = paste(input$sido, "의 연령대별 치매 환자 수"), 
           x = "연령대", y = "치매 환자 수 (천명)") +
      theme(legend.position = "none")  # 범례 제거
    
    ggplotly(p, tooltip = "text")  # 마우스 오버 시 'text' 내용 표시
  })
  
  # 데이터 테이블 출력
  output$data_table <- renderDT({
    filtered_data() %>%
      select(year, sido, age, demen) %>%  # 필요한 컬럼 선택
      rename(연도 = year, 시도 = sido, 연령대 = age, 치매환자수 = demen) %>%  # 한글 이름 변경
      mutate(치매환자수 = scales::comma(치매환자수)) %>%  # 천 단위 콤마 추가
      datatable(options = list(pageLength = 5))  # 한 페이지에 5개씩 표시
  })
  # 엑셀 다운로드 기능 추가
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("치매_데이터_", input$sido, "_", input$year, ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(filtered_data(), file)  # 데이터 저장
    }
  )
}

shinyApp(ui = ui, server = server)


# updateSelectInput()**을 사용해서 sido 선택 시 sigungu 옵션이 자동으로 변경되도록 만들 수 있어!
ui2 <- fluidPage(
  titlePanel("시도별 추청 치매 환자 수"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sido", "시도 선택:", choices = unique(dementia$sido[dementia$sido != "전국"])),  # 전국 제외
      selectInput("sigungu", "시군구 선택:", choices = NULL),  # 초기에는 비워둠 observeEvent() + updateSelectInput()을 추가!
      selectInput("year", "연도 선택:", choices = sort(unique(dementia$year), decreasing = TRUE)),
      downloadButton("download_excel", "엑셀 다운로드")  # 다운로드 버튼 추가
    ),
  mainPanel( 
    plotlyOutput("age_histogram") , 
    DTOutput('data_table')
    ) # 마우스 오버: plotOutput -> plotlyOutput 변경
  )
)

server2 <- function(input, output, session) {
  observeEvent(input$sido, { # 선택한 sido에 따라 sigungu 목록을 업데이트
    filtered_sigungu <- dementia %>%
      # filter(sido == input$sido & sigungu != "전국" & sido != sigungu) %>%
      filter(sido == input$sido ) %>%
      pull(sigungu) %>% # select 개념. pull은 벡터로 변환하고 select는 df
      unique()
    updateSelectInput(session, "sigungu", choices = filtered_sigungu) # choices 전달을 위해 vector
  })

  filtered_data <- reactive({
    dementia %>%
      filter(sex == '전체' & sido != '전국') %>%
      filter(age %in% c('60~64세', '65~69세', '70~74세', '75~79세', '80~84세', '85세이상')) %>%
      filter(sido == input$sido, sigungu == input$sigungu, year == input$year)
  })
  
  # 히스토그램 출력 (Plotly 적용)
  output$age_histogram <- renderPlotly({
    p <- ggplot(filtered_data(),
                aes(x = age, y = demen, fill = age,
                    text = paste("연령대:", age, "<br>환자 수:", scales::comma(demen), "명"))) +
      geom_bar(stat = "identity", width = 0.7) +  # 바 너비 조정
      theme_minimal() +  # 깔끔한 테마 적용
      scale_fill_brewer(palette = "Set2") +  # 예쁜 색상 적용
      scale_y_continuous(labels = scales::comma_format(scale = 1/1000, suffix = "천 명")) +  # y축 단위 변경
      labs(title = paste(input$sido, "의 연령대별 치매 환자 수"),
           x = "연령대", y = "치매 환자 수 (천명)") +
      theme(legend.position = "none")  # 범례 제거
    
    ggplotly(p, tooltip = "text")  # 마우스 오버 시 'text' 내용 표시
  })
  
  # 데이터 테이블 출력
  output$data_table <- renderDT({
    filtered_data() %>%
      select(year, sido, sigungu, age, demen) %>%  # 시군구 추가
      rename(연도 = year, 시도 = sido, 시군구 = sigungu, 연령대 = age, 치매환자수 = demen) %>%
      mutate(치매환자수 = scales::comma(치매환자수)) %>%
      datatable(options = list(pageLength = 5))  # 한 페이지에 5개씩 표시
  })
  
  output$download_excel <- downloadHandler(
    filename = function() { paste("치매_데이터_", input$sido, "_", input$year, ".xlsx", sep = "") },
    content = function(file) { 
      data <- filtered_data()
      colnames(data) <- c("연도", "시도", "시군구", "성별", "연령대", "고령 인구", "치매 환자 수", "MCI 환자 수")
      write_xlsx(data, file)  # 지정된 컬럼명으로 엑셀 저장
    }
  )
}

shinyApp(ui2, server2)  
