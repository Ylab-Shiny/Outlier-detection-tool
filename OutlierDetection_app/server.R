##############################################################################################################################
#### 外れ値特定ツール -- server.R ############################################################################################
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
# 信頼区間内かどうかを判定する
row_packs_isnt_out <- row_packs(
  # 列に基づく非外れ値
  column = . %>% transmute_if(is.numeric, within_range)
)

# オリジナルの欠損値をNAorに変換する
conversion_NAor <- function(x) {
  data <- x
  data[is.na(data)] <- "NAor"
  
  return(data)
}

## shinyサーバー ##
shinyServer(function(input, output, session){
  
  # zスコアに変換
  dataZ <- reactive({
    if (!is.null(input$file)) {
      firstData <- read_csv(input$file$datapath)
      # 文字型になっている数値を数値型に変換
      secondData <- firstData[, -1] %>% transmute_all(as.numeric)
      thirdData <- secondData %>% transmute_all(funs((.-mean(., na.rm = T)) / sd(., na.rm = T)))
    } else {
      thirdData <- NULL
    }
    
    return(thirdData)
  }) ### dataZの最終部分
  
  report <- reactive({
    if (!is.null(input$file)) {
      breaker_report <- dataZ() %>% expose(row_packs_isnt_out, .remove_obeyers = T) %>% get_report() %>% 
        filter(value == F) %>% select(rule, id)
      names(breaker_report) <- c("列名", "外れ値のある行番号")
    } else {
      breaker_report <- NULL
    }
    
    return(breaker_report)
  }) ### reportの最終部分 
    
  
  # データテーブルのアウトプット
  output$DataTable <- renderDataTable({
    datatable(report(),
              options = list(
                lengthMenu = c(10, 100, 1000),
                pageLength = 100,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  }) ### DataTableの最終部分
  
  # データ列のプルダウンリスト
  output$columns_out <- renderUI({
    if (!is.null(input$file)) {
      cnames <- read_csv(input$file$datapath, col_names = F, n_max = 1)
      ls <- unique(as.character(cnames[,-1]))
      si <- selectInput("c_out", "列名を選択してください", ls, multiple = F)
    } else {
      si <- NULL
    }
    
    return(si)
  }) ### columns_outの最終部分
  
  # 列ごとに外れ値と時刻を取り出す
  outliers_each <- reactive({
    if (!is.null(input$file)) {
      firstData <- read_csv(input$file$datapath)
      names(firstData)[1] <- "時刻"
      secondData <- firstData %>% select(`時刻`, input$c_out)
      ids <- report() %>% filter(`列名` == input$c_out)
      thirdData <- secondData[ids[[2]],]
    } else {
      thirdData <- NULL
    }
    
    return(thirdData)
  }) ### outliers_eachの最終部分
  
  # 各列の外れ値をDTで出力
  output$dt_each <- renderDataTable({
    datatable(outliers_each(),
              options = list(
                lengthMenu = c(10, 100, 1000),
                pageLength = 100,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  }) ### dt_eachの最終部分
  
}) ###  shinyServerの最終部分