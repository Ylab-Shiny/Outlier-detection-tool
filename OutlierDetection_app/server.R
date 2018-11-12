##############################################################################################################################
#### 中部大電力消費実績描画アプリ -- server.R ################################################################################
##############################################################################################################################
# ライブラリ一覧
{
  library(shiny)
  library(RColorBrewer)
  library(DT)
  library(shinyBS)
  library(tidyverse)
  library(ruler)
}
# ブラウザでの立ち上げ
options(shiny.launch.browser = T)
# アップロードできるファイルのサイズを50MBまでとする
options(shiny.maxRequestSize = 50*1024^2)

# 信頼区間の設定
within_range <- function(x, na.rm = T) {
  -5 < x & x < 10 
}

row_packs_isnt_out <- row_packs(
  # 列に基づく非外れ値
  column = . %>% transmute_if(is.numeric, within_range)
)

## shinyサーバー ##
shinyServer(function(input, output, session){
  
  # zスコアに変換
  dataZ <- reactive({
    firstData <- read_csv(input$file$datapath)
    # 文字型になっている数値を数値型に変換
    secondData <- firstData[, -1] %>% transmute_all(as.numeric)
    thirdData <- secondData %>% transmute_all(funs((.-mean(., na.rm = T)) / sd(., na.rm = T)))
    
    return(thirdData)
  }) ### dataZの最終部分
  
  report <- reactive({
    breaker_report <- dataZ() %>% expose(row_packs_isnt_out, .remove_obeyers = T) %>% get_report() %>% 
      filter(value == F) %>% select(rule, id)
    names(breaker_report) <- c("列名", "外れ値のある行番号")
    
    return(breaker_report)
  }) ### reportの最終部分 
    
  
  # データテーブルのアウトプット
  output$DataTable <- renderDataTable({
    datatable(report(),
              options = list(
                lengthMenu = c(10, 100, 1500),
                pageLength = 100,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  }) ### DataTableの最終部分
  
}) ###  shinyServerの最終部分