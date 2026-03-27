# ==========================================
# ShinyLive App: AI Compute vs. Citizen Survival
# File Name MUST be: app.R
# ==========================================

# 明確列出輕量化套件，並強制宣告底層相依套件以修復 WebR Bug
library(shiny)
library(dplyr)    # 取代 tidyverse (處理資料流 %>%)
library(tibble)   # 取代 tidyverse (處理資料表)
library(bslib)
library(plotly)
library(scales)
library(munsell)  # 強制讓 ShinyLive 下載這個缺失的底層套件

# --- 1. 雙語字典與解讀模版 ---
dict <- list(
  zh = list(
    title = "AI 算力與市民生存真相看板",
    region = "分析區域 (Region)",
    var1_title = "AI 算力耗電年增率 (%)",
    var1_help = "單位: %/年。科技巨頭每年增加的用電需求擴張速度。",
    var2_title = "實體綠電自建率 (%)",
    var2_help = "單位: %。巨頭真實自建再生能源的比例，排除帳面憑證。",
    var3_title = "基建成本轉嫁率 (%)",
    var3_help = "單位: %。電網升級成本轉嫁給一般居民分攤的比例。",
    plot1_title = "指標 1：每月電費帳單比較",
    plot2_title = "指標 2：極端氣候斷電風險",
    plot3_title = "指標 3：本地常規職缺剩餘率",
    plot4_title = "指標 4：資源掠奪指數",
    plot5_title = "指標 5：實際化石燃料缺口 (碳排)",
    plot6_title = "指標 6：人均稅金倒貼基建累計",
    legend_base = "無擴張(基準)", legend_ai = "AI擴張後",
    desc1_pre = "【荷包衝擊】到 2030 年，您每月將強迫多繳 ", desc1_post = " / 月。",
    desc2_pre = "【生存威脅】夏季尖峰跳電發生率逼近 ", desc2_post = "%，限電將成常態。",
    desc3_pre = "【工作流失】當地重複性行政與技術職缺將永久消失 ", desc3_post = "%。",
    desc4_pre = "【分配不公】掠奪評分為 ", desc4_post = "。數值 >70 代表嚴重剝削資源。",
    desc5_pre = "【綠電幻象】深夜無太陽能時，需額外燃燒 ", desc5_post = " 單位化石燃料。",
    desc6_pre = "【稅金黑洞】為供養巨頭電網，您實質倒貼了 ", desc6_post = "。",
    source = "基準：EIA / 台電 / ISO 42001。依據美台市場結構進行差異化模擬。",
    warning = "【合規與風險警示】預測數據僅供公共政策探討參考，非實質財務或法律建議。"
  ),
  en = list(
    title = "AI Compute vs. Citizen Survival Truth",
    region = "Analysis Region",
    var1_title = "AI Power Demand Growth (%)",
    var1_help = "Unit: %/Year. Annual expansion rate of power demand.",
    var2_title = "Physical Green Energy Ratio (%)",
    var2_help = "Unit: %. Actual ratio of self-built renewable energy.",
    var3_title = "Cost Pass-Through to Citizens (%)",
    var3_help = "Unit: %. Grid upgrade costs passed onto residential bills.",
    plot1_title = "Ind 1: Monthly Bill Comparison",
    plot2_title = "Ind 2: Extreme Weather Blackout Risk",
    plot3_title = "Ind 3: Conventional Job Remaining",
    plot4_title = "Ind 4: Resource Greed Index",
    plot5_title = "Ind 5: Actual Fossil Fuel Gap",
    plot6_title = "Ind 6: Per Capita Tax Drain",
    legend_base = "Baseline", legend_ai = "With AI Surge",
    desc1_pre = "[Wallet] By 2030, forced to pay an extra ", desc1_post = " / month.",
    desc2_pre = "[Survival] Peak blackout probability hits ", desc2_post = "%.",
    desc3_pre = "[Job Loss] ", desc3_post = "% of routine jobs will disappear.",
    desc4_pre = "[Inequality] Greed Index is ", desc4_post = ". >70 means severe extraction.",
    desc5_pre = "[Greenwash] ", desc5_post = " units of fossil fuels burned at night for AI.",
    desc6_pre = "[Tax Hole] You subsidize AI grid upgrades by ", desc6_post = ".",
    source = "Ref: EIA / Taipower / ISO 42001. Differentiated simulation based on US vs TW grid models.",
    warning = "[Warning] Predictive data is for policy discussion only, not financial/legal advice."
  )
)

# --- UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2C3E50"),
  title = uiOutput("ui_title"),
  sidebar = sidebar(
    width = 350,
    radioButtons("lang", "Language / 語言", choices = c("中文" = "zh", "English" = "en"), inline = TRUE),
    selectInput("region", "Region / 區域", choices = c("Northern Taiwan (TPC)" = "TW", "Virginia, USA (PJM)" = "US")),
    hr(),
    uiOutput("ui_sliders")
  ),
  layout_columns(
    col_widths = c(4, 4, 4, 4, 4, 4),
    card(full_screen = TRUE, card_header(uiOutput("header1"), class="bg-primary text-white"), card_body(plotlyOutput("plot1")), card_footer(textOutput("desc1"), class="text-danger small fw-bold")),
    card(full_screen = TRUE, card_header(uiOutput("header2"), class="bg-primary text-white"), card_body(plotlyOutput("plot2")), card_footer(textOutput("desc2"), class="text-danger small fw-bold")),
    card(full_screen = TRUE, card_header(uiOutput("header3"), class="bg-primary text-white"), card_body(plotlyOutput("plot3")), card_footer(textOutput("desc3"), class="text-danger small fw-bold")),
    card(full_screen = TRUE, card_header(uiOutput("header4"), class="bg-dark text-white"), card_body(plotlyOutput("plot4")), card_footer(textOutput("desc4"), class="text-danger small fw-bold")),
    card(full_screen = TRUE, card_header(uiOutput("header5"), class="bg-dark text-white"), card_body(plotlyOutput("plot5")), card_footer(textOutput("desc5"), class="text-danger small fw-bold")),
    card(full_screen = TRUE, card_header(uiOutput("header6"), class="bg-dark text-white"), card_body(plotlyOutput("plot6")), card_footer(textOutput("desc6"), class="text-danger small fw-bold"))
  ),
  div(class = "mt-2 p-2 bg-light border text-muted small", uiOutput("ui_footer"))
)

# --- Server ---
server <- function(input, output, session) {
  t <- reactive({ dict[[input$lang]] })
  reg <- reactive({
    if(input$region == "US") {
      list(base_bill = 120, currency = " USD", tax_base = 250, bm = 1.8, sr = 1.0, tm = 2)
    } else {
      list(base_bill = 2000, currency = " TWD", tax_base = 5000, bm = 1.2, sr = 0.2, tm = 10)
    }
  })
  
  output$ui_title <- renderUI({ t()$title })
  output$header1 <- renderUI({ t()$plot1_title }); output$header2 <- renderUI({ t()$plot2_title }); output$header3 <- renderUI({ t()$plot3_title })
  output$header4 <- renderUI({ t()$plot4_title }); output$header5 <- renderUI({ t()$plot5_title }); output$header6 <- renderUI({ t()$plot6_title })
  output$ui_footer <- renderUI({ HTML(paste0("<b>", t()$source, "</b><br><span class='text-danger'>", t()$warning, "</span>")) })
  
  output$ui_sliders <- renderUI({
    tagList(
      sliderInput("var1", t()$var1_title, min = 5, max = 50, value = 25), helpText(t()$var1_help),
      sliderInput("var2", t()$var2_title, min = 0, max = 100, value = 20), helpText(t()$var2_help),
      sliderInput("var3", t()$var3_title, min = 0, max = 100, value = 70), helpText(t()$var3_help)
    )
  })
  
  sim_data <- reactive({
    req(input$var1, input$var2, input$var3)
    r <- reg()
    growth <- input$var1 / 100; green <- input$var2 / 100; pass <- input$var3 / 100
    tibble(Year = 2024:2030) %>%
      mutate(
        Time = Year - 2024,
        Bill_Base = r$base_bill * (1 + 0.02)^Time,
        Bill_AI = Bill_Base + (r$base_bill * ((growth * 2 * Time)^r$bm) * pass * r$sr * (1 - green * 0.7)),
        Blackout_Risk = pmin(100, 5 + (growth * Time * 150) * (1 - green * 0.5)),
        Job_Rate = pmax(0, 100 * exp(-0.06 * (growth * Time * 8))),
        Greed_Idx = pmin(100, (growth * (1 - green) / 0.4) * 100),
        Fossil_Gap = pmax(0, 100 * growth * Time * (1 - green * 0.2)),
        Tax_Drain = r$tax_base * (growth * Time * pass * r$tm)
      )
  })
  
  m <- list(l = 50, r = 30, b = 60, t = 40, pad = 0)
  
  output$plot1 <- renderPlotly({
    plot_ly(sim_data(), x = ~Year) %>%
      add_lines(y = ~Bill_Base, name = t()$legend_base, line = list(dash = 'dash', color = 'blue')) %>%
      add_lines(y = ~Bill_AI, name = t()$legend_ai, line = list(color = 'red')) %>%
      layout(margin = m, xaxis = list(title=""), yaxis = list(title=""), legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.3))
  })
  output$plot2 <- renderPlotly({ plot_ly(sim_data(), x = ~Year, y = ~Blackout_Risk, type='scatter', mode='lines+markers', line=list(color='#E67E22')) %>% layout(margin = m, yaxis = list(range = c(0, 105))) })
  output$plot3 <- renderPlotly({ plot_ly(sim_data(), x = ~Year, y = ~Job_Rate, type='scatter', mode='lines', fill='tozeroy', line=list(color='#2980B9')) %>% layout(margin = m, yaxis = list(range = c(0, 105))) })
  output$plot4 <- renderPlotly({ plot_ly(type = "indicator", mode = "gauge+number", value = tail(sim_data()$Greed_Idx, 1), gauge = list(axis = list(range = list(NULL, 100)), bar = list(color = "#C0392B"))) %>% layout(margin = list(l=30, r=30, t=50, b=20)) })
  output$plot5 <- renderPlotly({ plot_ly(sim_data(), x = ~Year, y = ~Fossil_Gap, type='bar', marker=list(color='#7F8C8D')) %>% layout(margin = m) })
  output$plot6 <- renderPlotly({ plot_ly(sim_data(), x = ~Year, y = ~Tax_Drain, type='scatter', mode='none', stackgroup='one', fillcolor='#9B59B6') %>% layout(margin = m) })
  
  output$desc1 <- renderText({ d <- tail(sim_data(), 1); paste0(t()$desc1_pre, round(d$Bill_AI - d$Bill_Base, 0), reg()$currency, t()$desc1_post) })
  output$desc2 <- renderText({ paste0(t()$desc2_pre, round(tail(sim_data()$Blackout_Risk, 1), 1), t()$desc2_post) })
  output$desc3 <- renderText({ paste0(t()$desc3_pre, round(100 - tail(sim_data()$Job_Rate, 1), 1), t()$desc3_post) })
  output$desc4 <- renderText({ paste0(t()$desc4_pre, round(tail(sim_data()$Greed_Idx, 1), 0), t()$desc4_post) })
  output$desc5 <- renderText({ paste0(t()$desc5_pre, round(tail(sim_data()$Fossil_Gap, 1), 0), t()$desc5_post) })
  output$desc6 <- renderText({ paste0(t()$desc6_pre, format(round(tail(sim_data()$Tax_Drain, 1), 0), big.mark=","), reg()$currency, t()$desc6_post) })
}

shinyApp(ui, server)