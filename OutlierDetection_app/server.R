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
  interval <- -5 < x & x < 10
  return(interval)
}
# 信頼区間内かどうかを判定する
row_packs_isnt_out <- row_packs(
  # 列に基づく非外れ値
  column = . %>% transmute_if(is.numeric, within_range)
)

## shinyサーバー ##
shinyServer(function(input, output, session){
  
  # 初期データ
  initData <- reactive({
    firstData <- read_csv(input$file$datapath)
    names(firstData)[1] <- "label"
    
    return(firstData)
  }) ### initDataの最終部分
  
  # zスコアに変換
  dataZ <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
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
    
  
  # reportのアウトプット
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
  output$columns_ls <- renderUI({
    if (!is.null(input$file)) {
      cnames <- read_csv(input$file$datapath, col_names = F, n_max = 1)
      ls <- unique(as.character(cnames[,-1]))
      si <- selectInput("c_ls", "列名を選択してください", ls, multiple = F)
    } else {
      si <- NULL
    }
    
    return(si)
  }) ### columns_outの最終部分
  
  # 列ごとに外れ値と時刻を取り出す
  outliers_each <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% mutate(id = c(1:nrow(initData()))) %>% select(label, id, input$c_ls)
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      ids <- report() %>% filter(`列名` == input$c_ls)
      thirdData <- secondData[ids[[2]],]
      names(thirdData)[1] <- "時刻"
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
  
  # オリジナルのNAorと検出された外れ値のNAdeをNAで表したデータセット
  ConvData <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% select(label, input$c_ls)
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      secondData[outliers_each()$id, 2] <- NA
    } else {
      secondData <- NULL
    }
    
    return(secondData)
  }) ### ConvDataの最終部分
  
  # 外れ値をNAに変換したデータセットをDTで出力
  output$dt_conv <- renderDataTable({
    datatable(ConvData(),
              options = list(
                lengthMenu = c(10, 100, 1000),
                pageLength = 100,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  }) ### dt_convの最終部分
  
  ## トレンドグラフ ##
  Tgragh <- reactive({
    if (!is.null(input$file)) {
      Data_trend <- initData() %>% select(label, input$c_ls) %>% 
        mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      names(Data_trend) <- c("Time", "Values")
      
      p <- Data_trend %>% ggplot(aes_string(x="Time", y="Values")) + geom_line() +
        ggtitle(paste0(input$c_ls, "のトレンドグラフ")) + ylim(0, max(ConvData()[[2]])) +
        labs(x="時刻", y="電力消費[kW]")
      
      print(p)
    } else {
      print(NULL)
    }
  }) ### Tgraghの最終部分
  
  # トレンドグラフオブジェクトの出力
  output$Trendgragh <- renderPlot({
    Tgragh()
  }) ### Trendgraghの最終部分
  
  # カレンダーによる日付範囲の設定
  output$DateRange <- renderUI({
    if (!is.null(input$file)) {
      Range <- dateRangeInput(inputId = "theRange", label = "日付範囲を指定することができます",
                              start = substr(initData()$label[1], 1, 10),
                              end = substr(initData()$label[nrow(initData())], 1, 10),
                              format = "yyyy-mm-dd")
    } else {
      Range <- NULL
    }
    
    return(Range)
  }) ### DateRangeの最終部分
  
  # オリジナルデータの集計
  S_ori <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% select(label, input$c_ls)
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      thirdData <- summary(secondData) %>% as.data.frame() %>% 
        filter(Var2 == input$c_ls) %>% select(Freq)
      names(thirdData)[1] <- "統計情報"
    } else {
      thirdData <- NULL
    }
    
    return(thirdData)
  }) ### S_oriの最終部分
  
  # オリジナルデータの集計の出力
  output$summary_ori <- renderDataTable({
    datatable(S_ori())
  }) ### summary_oriの最終部分
  
  # 外れ値をNAとした後の集計
  S_con <- reactive({
    if (!is.null(input$file)) {
      firstData <- ConvData() %>% select(label, input$c_ls)
      secondData <- summary(firstData) %>% as.data.frame() %>% 
        filter(Var2 == input$c_ls) %>% select(Freq)
      names(secondData)[1] <- "統計情報"
    } else {
      secondData <- NULL
    }
    
    return(secondData)
  })
  
  # オリジナルデータの集計の出力
  output$summary_con <- renderDataTable({
    datatable(S_con())
  }) ### summary_conの最終部分
  
  ## データの散布図 ##
  # 散布図用のデータ
  Data_scat <- reactive({
    if (!is.null(input$file)) {
      # NAのある行を除いておく
      firstData <- initData() %>% select(label, input$c_ls)
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      C_interval <- secondData %>% select(input$c_ls) %>% 
        transmute_all(funs((.-mean(., na.rm = T)) / sd(., na.rm = T))) %>% 
        transmute_all(within_range)
      thirdData <- cbind(secondData, C_interval) %>% as.data.frame()
      names(thirdData) <- c("Time", "Values", "CoinfidenceInterval")
    } else {
      thirdData <- NULL
    }
    
    return(thirdData)
  }) ### Data_scatの最終部分
  
  # 散布図オブジェクト Splot
  Splot <- reactive({
    if (!is.null(input$file)) {
      p <- Data_scat() %>% ggplot(aes_string(x="Time", y="Values", color="CoinfidenceInterval")) +
        geom_point() + ylim(min(Data_scat()[[2]]), max(Data_scat()[[2]])) + 
        ggtitle(paste0(input$c_ls, "の散布図")) + 
        labs(x="時刻", y="電力消費[kW]", color="信頼区間内かどうか")
      
      print(p)
    } else {
      print(NULL)
    }
  }) ### Splotの最終部分
  
  # 散布図オブジェクトの出力
  output$scatterPlot <- renderPlot({
    Splot()
    }) ### scatterPlotの最終部分
  
  ## ダウンロードボタン ##
  # 外れ値をNAに変換したのデータセット
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("DF", substr(input$theRange[1],1,4), "_", substr(input$theRange[1],6,7), 
             substr(input$theRange[1],9,10), "_", substr(input$theRange[2],6,7), 
             substr(input$theRange[2],9,10), "_", input$c_ls,
             ".csv")
    },
    
    content <- function(file) {
      readr::write_excel_csv(ConvData(), file)
    }
  ) ### downloadDataの最終部分 ###
  
  # 散布図のダウンロード 
  output$downloadSplot <- downloadHandler(
    contentType = 'image/png',
    filename <- function() {
      paste0("Splot", substr(input$theRange[1],1,4), "_", substr(input$theRange[1],6,7), 
            substr(input$theRange[1],9,10), "_", substr(input$theRange[2],6,7), 
            substr(input$theRange[2],9,10), "_", input$c_ls, ".png")
    },
    
    content <- function(file) {
      png(file, width = 980, height = 400,
          units = "px", pointsize = 12,
          bg = "white", res = NA)
      
      plot <- Splot()
      print(plot)
      dev.off()
    }) ### downloadSplotの最終部分 ###
  
  ## 散布図のダウンロード 
  output$downloadTgragh <- downloadHandler(
    contentType = 'image/png',
    filename <- function() {
      paste0("Tgragh", substr(input$theRange[1],1,4), "_", substr(input$theRange[1],6,7), 
             substr(input$theRange[1],9,10), "_", substr(input$theRange[2],6,7), 
             substr(input$theRange[2],9,10), "_", input$c_ls, ".png")
    },
    
    content <- function(file) {
      png(file, width = 980, height = 400,
          units = "px", pointsize = 12,
          bg = "white", res = NA)
      
      plot <- Tgragh()
      print(plot)
      dev.off()
    }) ### downloadTgraghの最終部分 ###
  
}) ###  shinyServerの最終部分