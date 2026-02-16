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
glass_data$Type <- factor(glass_data$Type, 
                          levels = c(1, 2, 3, 5, 6, 7),
                          labels = c("Building_Win_Float", "Building_Win_NonFloat", 
                                     "Vehicle_Win", "Containers", "Tableware", "Headlamps"))
# [3] UI - Dashboard Layout
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Glass Identification"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("เกี่ยวกับข้อมูล", tabName = "intro", icon = icon("info-circle")),
                menuItem("ชุดข้อมูล", tabName = "overview", icon = icon("th-large")),
                menuItem("ภาพรวมและความน่าจะเป็น", tabName = "binomial", icon = icon("calculator"))
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
                    # --- ส่วนที่ 1: เพิ่มปุ่มกดดาวน์โหลดตรงนี้ ---
                    downloadButton("downloadData", " ดาวน์โหลดไฟล์ CSV", style = "margin-bottom: 15px; background-color: #27ae60; color: white; border: none;"),
                    dataTableOutput("rawDataTable")) 
              ),
              fluidRow(
                box(title = "กราฟแสดงจำนวนข้อมูลในแต่ละประเภท", width = 12, status = "primary", solidHeader = TRUE, 
                    plotOutput("distPlot", height = "450px"))
              )
      ),
      
      tabItem(tabName = "binomial",
              fluidRow(
                box(title = "ทฤษฎีการแจกแจงแบบทวินาม (Binomial Distribution)", width = 12, status = "info", solidHeader = TRUE,
                    p(style = "font-size: 16px;", "การแจกแจงความน่าจะเป็นของ X:"),
                    
                    # วาดกล่องสูตรสีฟ้าอ่อน (ตามรูปภาพเป๊ะๆ ด้วย HTML)
                    div(style = "text-align: center; margin: 15px 0;",
                        HTML('<div style="background-color: #bde0e8; border-radius: 30px; padding: 15px 30px; font-family: \'Times New Roman\', serif; font-size: 24px; color: #000; display: inline-block;">
                          <i>P ( X = x )</i> = 
                          <span style="font-size: 32px; vertical-align: middle;">(</span>
                          <span style="display: inline-block; text-align: center; vertical-align: middle; line-height: 1.1; font-size: 20px; font-style: italic; margin: 0 2px;">
                            n<br>x
                          </span>
                          <span style="font-size: 32px; vertical-align: middle;">)</span>
                          <i>p<sup> x</sup> (1 - p)<sup> n - x</sup></i> &nbsp;&nbsp; ; <i>x = 0, 1, ..., n</i>
                        </div>')
                    ),
                    
                    # วาดกล่อง X ~ Bin ลูกศร และกล่องข้อความ
                    div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: 20px;",
                        HTML('<div style="background-color: #f4f4f4; border: 2px solid #ccc; padding: 10px 20px; font-weight: bold; font-family: \'Times New Roman\', serif; font-size: 20px; margin-right: 15px;">
                          X ~ Bin (n, p)
                        </div>
                        <div style="color: #6fb2fb; font-size: 40px; margin-right: 15px; font-weight: bold;">&#10145;</div>
                        <div style="background-color: white; border: 2px solid #a1c4fd; border-radius: 10px; padding: 10px 20px; font-size: 16px; font-weight: bold; box-shadow: 4px 4px 0px #a1c4fd; color: #333;">
                          มีการแจกแจงแบบทวินามที่มี<br>พารามิเตอร์เท่ากับ n และ p
                        </div>')
                    ),
                    
                    # คำอธิบาย
                    tags$ul(style = "font-size: 16px; line-height: 1.8; margin-top: 15px;",
                            tags$li(tags$b("n"), " คือจำนวนครั้งในการทดลอง (จำนวนเศษแก้วที่สุ่มเก็บมา)"),
                            tags$li(tags$b("p"), " ความน่าจะเป็นที่จะเกิดสิ่งที่สนใจ P(S) = p และ q = 1-p")
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
                    # ปุ่มกดคำนวณ!
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

# [4] Server
server <- function(input, output, session) {

  output$rawDataTable <- renderDataTable({
    glass_data
  }, options = list(
    pageLength = 5,       # แสดงหน้าละ 5 แถว
    scrollX = TRUE,       # ให้เลื่อนซ้าย-ขวาได้ กรณีคอลัมน์เยอะ
    searching = TRUE      # เปิดโหมดค้นหา
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

# [5] รันแอป
shinyApp(ui = ui, server = server)