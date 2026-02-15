library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
# [2] โหลดข้อมูลจากไฟล์ glass.csv
filename <- "glass.csv"
glass_data <- read.csv("glass.csv")
glass_data$Type <- factor(glass_data$Type, 
                          levels = c(1, 2, 3, 5, 6, 7),
                          labels = c("Building_Win_Float", "Building_Win_NonFloat", 
                                     "Vehicle_Win", "Containers", "Tableware", "Headlamps"))

# [3] UI - Dashboard Layout
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Glass Identification"),
  
  dashboardSidebar(
    # จัดการเมนูให้ครบ 4 หน้า เพื่อให้แยกเนื้อหาได้ชัดเจน
    sidebarMenu(id = "tabs",
                menuItem("เกี่ยวกับข้อมูล", tabName = "intro", icon = icon("info-circle")),
                menuItem("ภาพรวมและความน่าจะเป็น", tabName = "overview", icon = icon("th-large")),
                menuItem("การแจกแจงทวินาม", tabName = "binomial", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(title = "บทนำ", width = 12, status = "primary", solidHeader = TRUE,
                    p(style = "font-size: 16px;",
                      "ชุดข้อมูล Glass Identification นี้เป็นข้อมูลที่ใช้ในการจำแนกประเภทของแก้ว 
                      โดยมีที่มาจากการสืบสวนทางนิติวิทยาศาสตร์ (Criminological Investigation) 
                      เพื่อใช้ระบุว่าเศษแก้วที่พบในที่เกิดเหตุเป็นแก้วประเภทใด 
                      ซึ่งจะช่วยในการหาหลักฐานเชื่อมโยงในคดีต่างๆ")
                )
              ),
              fluidRow(
                box(title = "ประเภทของแก้ว", width = 12, status = "info", solidHeader = TRUE,
                    h4("แก้วในชุดข้อมูลนี้ถูกแบ่งออกเป็น 6 ประเภทหลัก:"),
                    tags$ul(style = "font-size: 16px; line-height: 1.8;",
                            tags$li(tags$b("ประเภท 1:"), " กระจกหน้าต่างอาคาร (ผลิตด้วยวิธี Float)"),
                            tags$li(tags$b("ประเภท 2:"), " กระจกหน้าต่างอาคาร (ผลิตด้วยวิธีอื่น)"),
                            tags$li(tags$b("ประเภท 3:"), " กระจกหน้าต่างรถยนต์"),
                            tags$li(tags$b("ประเภท 5:"), " ภาชนะบรรจุ (ขวด/แก้วน้ำ)"),
                            tags$li(tags$b("ประเภท 6:"), " เครื่องบนโต๊ะอาหาร (Tableware)"),
                            tags$li(tags$b("ประเภท 7:"), " โคมไฟหน้ารถยนต์ (Headlamps)")
                    ),
                    p(tags$i("*หมายเหตุ: ในชุดข้อมูลต้นฉบับไม่มีการระบุประเภทที่ 4 ไว้"))
                )
              ),
              fluidRow(
                box(title = "ที่มาของข้อมูล", width = 12, status = "success", solidHeader = TRUE,
                    p(style = "font-size: 16px;",
                      tags$a(href = "https://www.kaggle.com/datasets/uciml/glass",
                             "Glass Identification Dataset on Kaggle",
                             target = "_blank",
                             style = "color: blue; text-decoration: underline; font-weight: bold;"))
                )
              ),
              fluidRow(
                box(title = "คำอธิบายตัวแปรเคมี", width = 12, status = "warning", solidHeader = TRUE,
                    tags$ul(style = "font-size: 16px; line-height: 2;",
                            tags$li(tags$b("RI (Refractive Index):"), " ค่าดัชนีหักเหของแสง"),
                            tags$li(tags$b("Na (Sodium):"), " โซเดียม"),
                            tags$li(tags$b("Mg (Magnesium):"), " แมกนีเซียม"),
                            tags$li(tags$b("Al (Aluminum):"), " อะลูมิเนียม"),
                            tags$li(tags$b("Si (Silicon):"), " ซิลิคอน"),
                            tags$li(tags$b("K (Potassium):"), " โพแทสเซียม"),
                            tags$li(tags$b("Ca (Calcium):"), " แคลเซียม"),
                            tags$li(tags$b("Ba (Barium):"), " แบเรียม"),
                            tags$li(tags$b("Fe (Iron):"), " เหล็ก")
                    )
                )
              )
      ),
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "ตารางค่าความน่าจะเป็นรายประเภท (Probability Distribution)", width = 12, 
                    status = "success", solidHeader = TRUE,
                    p("โอกาสที่จะสุ่มพบแก้วแต่ละประเภทในชุดข้อมูลนี้:"),
                    tableOutput("probTable"))
              ),
              fluidRow(
                box(title = "กราฟแสดงจำนวนข้อมูลในแต่ละประเภท", width = 12, 
                    status = "primary", solidHeader = TRUE, 
                    plotOutput("distPlot", height = "450px"))
              ),
              fluidRow(
                valueBox(nrow(glass_data), "จำนวนตัวอย่างทั้งหมด", icon = icon("database"), color = "green"),
                valueBox(length(unique(glass_data$Type)), "จำนวนประเภทแก้ว", icon = icon("vials"), color = "blue")
              )
      ),
      tabItem(tabName = "binomial",
              fluidRow(
                box(title = "ทฤษฎีการแจกแจงแบบทวินาม (Binomial Distribution)", width = 12, status = "info", solidHeader = TRUE,
                    p(style = "font-size: 16px;", "ในงานนิติวิทยาศาสตร์ เราสามารถประยุกต์ใช้การแจกแจงแบบทวินาม เพื่อจำลองสถานการณ์การสุ่มเก็บเศษแก้วในที่เกิดเหตุได้"),
                    # ใช้ข้อความธรรมดาแทน MathJax ป้องกันหน้าเว็บพัง
                    p(style = "font-size: 18px; font-weight: bold; color: #c0392b;", "สูตรการคำนวณ: P(X = x) = nCx * p^x * (1-p)^(n-x)"),
                    tags$ul(style = "font-size: 16px;",
                            tags$li("n = จำนวนเศษแก้วทั้งหมดที่สุ่มเก็บมาตรวจ"),
                            tags$li("x = จำนวนเศษแก้วประเภทที่สนใจ ที่คาดว่าจะพบ"),
                            tags$li("p = ค่าความน่าจะเป็นตั้งต้นของแก้วประเภทนั้น (คำนวณจากชุดข้อมูล)")
                    )
                )
              ),
              fluidRow(
                box(title = "เครื่องมือจำลองสถานการณ์", width = 4, status = "warning", solidHeader = TRUE,
                    selectInput("bino_type", "เลือกประเภทแก้วเป้าหมาย:", 
                                choices = c("Building_Win_Float", "Building_Win_NonFloat", "Vehicle_Win", "Containers", "Tableware", "Headlamps")),
                    numericInput("bino_n", "จำนวนที่สุ่มเก็บมาทั้งหมด (n):", value = 10, min = 1, step = 1),
                    numericInput("bino_x", "จำนวนเป้าหมายที่คาดว่าจะพบ (x):", value = 3, min = 0, step = 1),
                    hr(),
                    # ปุ่มกด
                    actionButton("calc_btn", " คำนวณความน่าจะเป็น", icon = icon("calculator"), width = "100%"),
                    hr(),
                    uiOutput("bino_p_info")
                ),
                box(title = "ผลลัพธ์และกราฟการแจกแจง", width = 8, status = "success", solidHeader = TRUE,
                    uiOutput("bino_result_text"),
                    plotOutput("bino_plot", height = "350px")
                )
              )
      )
      
    )
  )
)

# [4] Server - Logic
server <- function(input, output, session) {
  
  output$probTable <- renderTable({
    glass_data %>% count(Type) %>%
      mutate(Probability = n / sum(n), Percentage = paste0(round(Probability * 100, 2), "%")) %>%
      rename("ประเภทของแก้ว" = Type, "จำนวน (ชิ้น)" = n, "ความน่าจะเป็น" = Probability, "เปอร์เซ็นต์" = Percentage)
  })
  
  output$distPlot <- renderPlot({
    ggplot(glass_data, aes(x = Type, fill = Type)) + geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5, fontface = "bold") +
      theme_minimal() + scale_fill_brewer(palette = "Set3") +
      expand_limits(y = max(table(glass_data$Type)) * 1.1) +
      labs(y = "จำนวน (Count)", x = "") + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  observeEvent(input$calc_btn, {
    
    p_val <- sum(glass_data$Type == input$bino_type, na.rm = TRUE) / nrow(glass_data)
    
    output$bino_p_info <- renderUI({
      p(style = "color: #e67e22; font-weight: bold; font-size: 16px;", 
        paste("ค่า p (Probability) ของ", input$bino_type, "คือ:", round(p_val, 4)))
    })
    
    output$bino_result_text <- renderUI({
      n <- input$bino_n
      x <- input$bino_x
      
      if(x > n) {
        return(h4(style = "color: red; font-weight: bold;", "❌ ข้อผิดพลาด: ค่า x ต้องไม่มากกว่า n"))
      }
      
      prob_exact <- dbinom(x, n, p_val)
      
      tagList(
        h3(style = "color: #27ae60; font-weight: bold;", 
           paste("ความน่าจะเป็น P(X =", x, ") = ", round(prob_exact, 4))),
        p(style = "font-size: 18px;", 
          paste("หรือคิดเป็น", round(prob_exact * 100, 2), "%"))
      )
    })
    
    output$bino_plot <- renderPlot({
      n <- input$bino_n
      x <- input$bino_x
      
      if(x > n) return(NULL)
      
      df_bino <- data.frame(Successes = 0:n, Probability = dbinom(0:n, n, p_val))
      df_bino$Highlight <- ifelse(df_bino$Successes == x, "เป้าหมาย (x)", "ค่าอื่นๆ")
      
      ggplot(df_bino, aes(x = factor(Successes), y = Probability, fill = Highlight)) +
        geom_bar(stat = "identity", color = "black", alpha = 0.8) +
        scale_fill_manual(values = c("ค่าอื่นๆ" = "steelblue", "เป้าหมาย (x)" = "#e74c3c")) +
        geom_text(aes(label = round(Probability, 3)), vjust = -0.5, size = 4) +
        theme_minimal() +
        labs(title = paste("กราฟการแจกแจงแบบทวินาม: B(n =", n, ", p =", round(p_val, 3), ")"),
             x = "จำนวนเศษแก้วที่พบ (x)", y = "ความน่าจะเป็น") +
        theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 14))
    })
  })
}

# [5] รันแอปพลิเคชัน
shinyApp(ui = ui, server = server)