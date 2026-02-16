library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)

# [2] โหลดข้อมูล
file_path <- "glass"
if(!file.exists(file_path))file_path <- "glass.csv"
glass_data <- read.csv("glass.csv")
if("Type" %in% names(glass_data)) {
  glass_data$Type <- factor(glass_data$Type, 
                            levels = c(1, 2, 3, 5, 6, 7),
                            labels = c("Building_Win_Float", "Building_Win_NonFloat", 
                                       "Vehicle_Win", "Containers", "Tableware", "Headlamps"))
}

# [3] UI - Dashboard Layout
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Glass Identification"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("เกี่ยวกับข้อมูล", tabName = "intro", icon = icon("info-circle")),
                menuItem("ชุดข้อมูล", tabName = "overview", icon = icon("th-large")),
                # --- เปลี่ยนชื่อเมนูเป็น ปัวซง ---
                menuItem("การแจกแจงปัวซง", tabName = "poisson", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(title = "บทนำ", width = 12, status = "primary", solidHeader = TRUE,
                    p(style = "font-size: 16px;", "ชุดข้อมูล Glass Identification นี้เป็นข้อมูลที่ใช้ในการจำแนกประเภทของแก้ว โดยมีที่มาจากการสืบสวนทางนิติวิทยาศาสตร์"))
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
                box(title = "ประเภทของแก้ว", width = 12, status = "info", solidHeader = TRUE,
                    tags$ul(style = "font-size: 16px; line-height: 1.8;",
                            tags$li(tags$b("ประเภท 1:"), " กระจกหน้าต่างอาคาร (ผลิตด้วยวิธี Float)"),
                            tags$li(tags$b("ประเภท 2:"), " กระจกหน้าต่างอาคาร (ผลิตด้วยวิธีอื่น)"),
                            tags$li(tags$b("ประเภท 3:"), " กระจกหน้าต่างรถยนต์"),
                            tags$li(tags$b("ประเภท 5:"), " ภาชนะบรรจุ (ขวด/แก้วน้ำ)"),
                            tags$li(tags$b("ประเภท 6:"), " เครื่องบนโต๊ะอาหาร"),
                            tags$li(tags$b("ประเภท 7:"), " โคมไฟหน้ารถยนต์")
                    )
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
                box(title = "ตารางแสดงข้อมูลในไฟล์ (Raw Data)", width = 12, status = "success", solidHeader = TRUE,
                    downloadButton("downloadData", " ดาวน์โหลดไฟล์ CSV", style = "margin-bottom: 15px; background-color: #27ae60; color: white; border: none;"),
                    dataTableOutput("rawDataTable")) 
              ),
              fluidRow(
                box(title = "กราฟแสดงจำนวนข้อมูลในแต่ละประเภท", width = 12, status = "primary", solidHeader = TRUE, 
                    plotOutput("distPlot", height = "450px"))
              )
      ),
      
      # ==========================================
      # --- Tab 4: การแจกแจงแบบปัวซง (แก้ไขใหม่ทั้งหมด) ---
      # ==========================================
      tabItem(tabName = "poisson",
              fluidRow(
                box(title = "ทฤษฎีการแจกแจงแบบปัวซง (Poisson Distribution)", width = 12, status = "info", solidHeader = TRUE,
                    p(style = "font-size: 16px;", "ใช้เพื่อจำลองเหตุการณ์ที่มีการกระจายตัวแบบสุ่ม โดยสนใจจำนวนครั้งที่เกิดเหตุการณ์ในขอบเขตที่กำหนด เช่น การคาดการณ์จำนวนเศษแก้วเป้าหมายที่จะพบจากการสุ่มตรวจ"),
                    
                    # วาดกล่องสูตรสีฟ้าอ่อนแบบปัวซง
                    div(style = "text-align: center; margin: 15px 0;",
                        HTML('<div style="background-color: #bde0e8; border-radius: 30px; padding: 15px 30px; font-family: \'Times New Roman\', serif; font-size: 24px; color: #000; display: inline-block;">
                          <i>P ( X = x )</i> = 
                          <div style="display: inline-block; vertical-align: middle; text-align: center; margin: 0 10px;">
                            <div style="border-bottom: 2px solid #000; padding-bottom: 2px;"><i>e<sup> -&lambda;</sup> &lambda;<sup> x</sup></i></div>
                            <div style="padding-top: 2px;"><i>x</i> !</div>
                          </div>
                          &nbsp;&nbsp; ; <i>x = 0, 1, 2, ...</i>
                        </div>')
                    ),
                    
                    # วาดกล่อง X ~ Poi ลูกศร และกล่องข้อความ
                    div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 20px;",
                        HTML('<div style="background-color: #f4f4f4; border: 2px solid #ccc; padding: 10px 20px; font-weight: bold; font-family: \'Times New Roman\', serif; font-size: 20px; margin-right: 15px;">
                          X ~ Poi (&lambda;)
                        </div>
                        <div style="color: #6fb2fb; font-size: 40px; margin-right: 15px; font-weight: bold;">&#10145;</div>
                        <div style="background-color: white; border: 2px solid #a1c4fd; border-radius: 10px; padding: 10px 20px; font-size: 16px; font-weight: bold; box-shadow: 4px 4px 0px #a1c4fd; color: #333;">
                          มีการแจกแจงแบบปัวซงที่มี<br>พารามิเตอร์อัตราเฉลี่ยเท่ากับ &lambda;
                        </div>')
                    ),
                    
                    # คำอธิบายตัวแปรปัวซง
                    tags$ul(style = "font-size: 16px; line-height: 1.8; margin-top: 15px;",
                            tags$li(HTML("<b>&lambda; (Lambda)</b> คือ อัตราการเกิดเหตุการณ์เฉลี่ยที่คาดว่าจะพบ (คำนวณจาก n &times; p)")),
                            tags$li(HTML("<b>x</b> คือ จำนวนเศษแก้วเป้าหมายที่สนใจ")),
                            tags$li(HTML("<b>e</b> คือ ค่าคงที่ทางคณิตศาสตร์ (ประมาณ 2.71828)"))
                    )
                )
              ),
              fluidRow(
                box(title = "เครื่องมือจำลองสถานการณ์", width = 4, status = "warning", solidHeader = TRUE,
                    selectInput("pois_type", "เลือกประเภทแก้วเป้าหมาย:", 
                                choices = c("Building_Win_Float", "Building_Win_NonFloat", "Vehicle_Win", "Containers", "Tableware", "Headlamps")),
                    numericInput("pois_n", "จำนวนที่สุ่มเก็บมาทั้งหมด (n):", value = 50, min = 1, step = 1),
                    numericInput("pois_x", "จำนวนเป้าหมายที่สนใจ (x):", value = 3, min = 0, step = 1),
                    hr(),
                    # ปุ่มกดคำนวณ
                    actionButton("calc_btn", " คำนวณความน่าจะเป็น", icon = icon("calculator"), width = "100%", style="background-color: #27ae60; color: white; border-radius: 5px; border: none; font-size: 16px; font-weight: bold; padding: 10px;"),
                    hr(),
                    uiOutput("pois_p_info")
                ),
                box(title = "ผลลัพธ์และกราฟการแจกแจงแบบปัวซง", width = 8, status = "success", solidHeader = TRUE,
                    uiOutput("pois_result_text"),
                    plotOutput("pois_plot", height = "350px")
                )
              )
      ) 
    ) 
  ) 
)

# [4] Server
server <- function(input, output, session) {
  
  output$rawDataTable <- renderDataTable({
    glass_data
  }, options = list(
    pageLength = 5,
    scrollX = TRUE,
    searching = TRUE
  ))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Glass_Identification_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(glass_data, file, row.names = FALSE)
    }
  )
  
  output$distPlot <- renderPlot({
    ggplot(glass_data, aes(x = Type, fill = Type)) + geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 5, fontface = "bold") +
      theme_minimal() + scale_fill_brewer(palette = "Set3") +
      expand_limits(y = max(table(glass_data$Type)) * 1.1) +
      labs(y = "จำนวน (Count)", x = "") + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # --- ระบบคำนวณปัวซง ---
  observeEvent(input$calc_btn, {
    
    # 1. คำนวณความน่าจะเป็นตั้งต้น (p) จากชุดข้อมูล
    p_val <- sum(glass_data$Type == input$pois_type, na.rm = TRUE) / nrow(glass_data)
    
    # 2. คำนวณค่าเฉลี่ย Lambda = n * p
    lambda_val <- input$pois_n * p_val
    
    # แสดงค่า Lambda ให้ผู้ใช้เห็น
    output$pois_p_info <- renderUI({
      tagList(
        p(style = "color: #7f8c8d; font-size: 14px;", 
          paste("โอกาสพบแก้วประเภทนี้ (p) ≈", round(p_val, 4))),
        p(style = "color: #e67e22; font-weight: bold; font-size: 18px;", 
          paste("ค่าเฉลี่ยที่คาดว่าจะพบ (λ) =", round(lambda_val, 4), "ชิ้น"))
      )
    })
    
    # คำนวณผลลัพธ์ ปัวซง dpois()
    output$pois_result_text <- renderUI({
      x <- input$pois_x
      
      prob_exact <- dpois(x, lambda_val)
      
      tagList(
        h3(style = "color: #27ae60; font-weight: bold;", 
           paste("ความน่าจะเป็น P(X =", x, ") = ", round(prob_exact, 4))),
        p(style = "font-size: 18px;", 
          paste("หรือคิดเป็น", round(prob_exact * 100, 2), "%"))
      )
    })
    
    # พล็อตกราฟปัวซง
    output$pois_plot <- renderPlot({
      x <- input$pois_x
      
      # หาขอบเขตแกน x ที่เหมาะสมสำหรับพล็อตกราฟให้สวยงาม
      max_x_plot <- max(15, x + 5, ceiling(lambda_val + 3 * sqrt(lambda_val)))
      x_range <- 0:max_x_plot
      
      # ใช้สูตร dpois
      df_pois <- data.frame(
        Successes = x_range, 
        Probability = dpois(x_range, lambda_val)
      )
      df_pois$Highlight <- ifelse(df_pois$Successes == x, "เป้าหมาย (x)", "ค่าอื่นๆ")
      
      ggplot(df_pois, aes(x = factor(Successes), y = Probability, fill = Highlight)) +
        geom_bar(stat = "identity", color = "black", alpha = 0.8) +
        scale_fill_manual(values = c("ค่าอื่นๆ" = "steelblue", "เป้าหมาย (x)" = "#e74c3c")) +
        geom_text(aes(label = ifelse(Probability > 0.005, round(Probability, 3), "")), vjust = -0.5, size = 3.5) +
        theme_minimal() +
        labs(title = paste("กราฟการแจกแจงแบบปัวซง: Poi(λ =", round(lambda_val, 3), ")"),
             x = "จำนวนเศษแก้วที่พบ (x)", y = "ความน่าจะเป็น") +
        theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 14))
    })
  })
}

# [5] รันแอปพลิเคชัน
shinyApp(ui = ui, server = server)