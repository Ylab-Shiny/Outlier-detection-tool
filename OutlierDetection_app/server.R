##############################################################################################################################
#### 外れ値検出ツール -- server.R ############################################################################################
##############################################################################################################################

# 読み込むパッケージ ---------------------------------------------------------------
library(shiny)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(ruler)
library(tidyr)


# shinyServer関数外 ----------------------------------------------------------

# ブラウザでの立ち上げ
options(shiny.launch.browser = T)
# アップロードできるファイルのサイズを150MBまでとする
options(shiny.maxRequestSize = 150*1024^2)

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


# shinyServer関数内 ----------------------------------------------------------

shinyServer(function(input, output, session){
  

  # 初期データ initData ----------------------------------------------------------

  initData <- reactive({
    if (!is.null(input$file)) {
      firstData <- read_csv(input$file$datapath)
      names(firstData)[1] <- "label"
    } else {
      firstData <- NULL
    }
    
    return(firstData)
  }) 
  

  # initDataの出力 -------------------------------------------------------------

  output$initData <- renderDataTable({
    datatable(initData(),
              options = list(
                lengthMenu = c(24, 48, 1440),
                pageLength = 24,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  })
  

  # 外れ値判定を行う日付範囲　theDate ----------------------------------------------------

  theDate <- reactive({
    if (!is.null(input$file)) {
      texts <- paste0("外れ値検出を行った日付範囲は", substr(initData()$label[1], 1, 10),
                      "から", substr(initData()$label[nrow(initData())], 1, 10), "です")
    } else {
      texts <- NULL
    }
    
    return(texts)
  }) 
  

  # theDateの出力 --------------------------------------------------------------

  output$d_date <-  renderText(theDate())
  

  # initData2 ---------------------------------------------------------------

  initData2 <- reactive({
    firstData <- initData() %>% select(label, input$c_ls)
    C_interval <- firstData %>% select(input$c_ls) %>% 
      transmute_all(funs((.-mean(., na.rm = T)) / sd(., na.rm = T))) %>% 
      transmute_all(within_range)
    names(C_interval) <- "CI"
    secondData <- cbind(firstData, C_interval) %>% as.data.frame()
    
    return(secondData)
  })

  # dataZ -------------------------------------------------------------------

  # zスコアに変換
  dataZ <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() 
      # 文字型になっている数値を数値型に変換
      secondData <- firstData[, -1] %>% transmute_all(as.numeric)
      thirdData <- secondData %>% transmute_all(funs((.-mean(., na.rm = T)) / sd(., na.rm = T)))
    } else {
      thirdData <- NULL
    }
    
    return(thirdData)
  })

  # report ------------------------------------------------------------------

  report <- reactive({
    if (!is.null(input$file)) {
      breaker_report <- dataZ() %>% 
        expose(row_packs_isnt_out, .remove_obeyers = T) %>% get_report() %>% 
        filter(value == F) %>% select(rule, id)
      names(breaker_report) <- c("列名", "外れ値のある行番号")
    } else {
      breaker_report <- NULL
    }
    
    return(breaker_report)
  })
    

  # reportのアウトプット DataTable -------------------------------------------------

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
  })

  # データ列のプルダウンリスト columns_ls ------------------------------------------------

  output$columns_ls <- renderUI({
    if (!is.null(input$file)) {
      cnames <- names(initData())
      ls <- unique(as.character(cnames[-1]))
      si <- selectInput("c_ls", "列名を選択してください", ls, multiple = F)
    } else {
      si <- NULL
    }
    
    return(si)
  })
  

  # outliers_each -----------------------------------------------------------

  outliers_each <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% select(label, input$c_ls) %>% mutate(`行番号` = c(1:nrow(initData())))
      ids <- report() %>% filter(`列名` == input$c_ls)
      secondData <- firstData[ids[[2]],]
      names(secondData)[1] <- "時刻"
    } else {
      secondData <- NULL
    }
    
    return(secondData)
  })
  

  # dt_each -----------------------------------------------------------------

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
  })
  

  # ConvData ----------------------------------------------------------------

  # オリジナルのNAorと検出された外れ値のNAdeをNAで表したデータセット
  ConvData <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% select(label, input$c_ls)
      ids <- report() %>% filter(`列名` == input$c_ls)
      firstData[[2]][ids[[2]]] <- NA
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
    } else {
      secondData <- NULL
    }
    
    return(secondData)
  })

  # ConvData2 ---------------------------------------------------------------

  ConvData2 <- reactive({
    if (!is.null(input$file)) {
      namels <- names(initData())
      
      for (i in 1:length(namels)) {
        if (i == 1) {
          firstData <- initData()
          df <- as.character(firstData[[1]])
        } else {
          x <- firstData[[i]]
          ids <- report() %>% filter(`列名` == namels[i])
          x[ids[[2]]] <- NA
          df <- cbind(df, x)
        }
      }
      df <- as.data.frame(df)
      names(df) <- namels
      
    } else {
      df <- NULL
    }
    
    return(df)
  })

  # dt_conv -----------------------------------------------------------------

  # 外れ値をNAに変換したデータセットをDTで出力
  output$dt_conv <- renderDataTable({
    datatable(ConvData2(),
              options = list(
                lengthMenu = c(24, 48, 1440),
                pageLength = 24,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  })
  

  # トレンドグラフ Tgragh ----------------------------------------------------------

  Tgragh <- reactive({
    if (!is.null(input$file)) {
      Data_trend <- initData() %>% select(label, input$c_ls) %>% 
        mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      names(Data_trend) <- c("Time", "Values")
      
      p <- Data_trend %>% ggplot(aes_string(x="Time", y="Values")) + geom_line() +
        ggtitle(paste0(input$c_ls, "のトレンドグラフ")) + ylim(0, max(ConvData()[[2]])) +
        labs(x="時間", y="データ値")
      
      print(p)
    } else {
      print(NULL)
    }
  })
  

  # トレンドグラフオブジェクトの出力 --------------------------------------------------------

  output$Trendgragh <- renderPlot({
    Tgragh()
  })

  # DateRange ---------------------------------------------------------------

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
  })

  # S_ori -------------------------------------------------------------------

  # オリジナルデータの集計
  S_ori <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% select(label, input$c_ls)
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      thirdData <- summary(secondData[,2]) %>% as.data.frame() %>% select(Freq)
      fourthData <- thirdData %>% separate(Freq, into = c("tags", "values"), sep = ":")
    } else {
      fourthData <- NULL
    }
    
    return(fourthData)
  })
  

  # summary_ori -------------------------------------------------------------

  # オリジナルデータの集計の出力
  output$summary_ori <- renderDataTable({
    datatable(S_ori())
  })

  # S_con -------------------------------------------------------------------

  # 外れ値をNAとした後の集計
  S_con <- reactive({
    if (!is.null(input$file)) {
      firstData <- ConvData() %>% select(label, input$c_ls)
      secondData <- summary(firstData[,2]) %>% as.data.frame() %>% select(Freq)
      thirdData <- secondData %>% separate(Freq, into = c("tags", "values"), sep = ":")
    } else {
      thirdData <- NULL
    }
    
    return(thirdData)
  })

  # summary_con -------------------------------------------------------------

  # オリジナルデータの集計の出力
  output$summary_con <- renderDataTable({
    datatable(S_con())
  })
  

  # データの散布図 -----------------------------------------------------------------

  # 散布図用のデータ
  Data_scat <- reactive({
    if (!is.null(input$file)) {
      # NAのある行を除いておく
      firstData <- initData2()
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      names(secondData) <- c("Time", "Values", "CoinfidenceInterval")
    } else {
      secondData <- NULL
    }
    
    return(secondData)
  })
  

  # 散布図オブジェクト Splot ---------------------------------------------------------

  Splot <- reactive({
    if (!is.null(input$file)) {
      p <- Data_scat() %>% ggplot(aes_string(x="Time", y="Values", color="CoinfidenceInterval")) +
        geom_point() + ylim(min(Data_scat()[[2]]), max(Data_scat()[[2]])) + 
        ggtitle(paste0(input$c_ls, "の散布図")) + 
        labs(x="時間", y="データ値", color="信頼区間内かどうか")
      
      print(p)
    } else {
      print(NULL)
    }
  })
  

  # scatterPlot -------------------------------------------------------------

  # 散布図オブジェクトの出力
  output$scatterPlot <- renderPlot({
    Splot()
    })
  

  # downloadData ---------------------------------------------------------------

  # 外れ値をNAに変換したのデータセット
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("DF", substr(input$theRange[1],1,4), "_", substr(input$theRange[1],6,7), 
             substr(input$theRange[1],9,10), "_", substr(input$theRange[2],6,7), 
             substr(input$theRange[2],9,10), ".csv")
    },
    
    content <- function(file) {
      readr::write_excel_csv(ConvData2(), file)
    }
  ) 

  # downloadSummaryB --------------------------------------------------------

  # 外れ値をNAに変換する前の集計のダウンロード
  output$downloadSummaryB <- downloadHandler(
    filename = function() {
      paste0("BeforeSummary", substr(input$theRange[1],1,4), "_", substr(input$theRange[1],6,7), 
             substr(input$theRange[1],9,10), "_", substr(input$theRange[2],6,7), 
             substr(input$theRange[2],9,10),"_", input$c_ls, ".csv")
    },
    
    content = function(file) {
      readr::write_excel_csv(S_ori(), file)
    }
  )

  # downloadSummaryA --------------------------------------------------------

  # 外れ値をNAに変換後の集計のダウンロード
  output$downloadSummaryA <- downloadHandler(
    filename = function() {
      paste0("AfterSummary", substr(input$theRange[1],1,4), "_", substr(input$theRange[1],6,7), 
             substr(input$theRange[1],9,10), "_", substr(input$theRange[2],6,7), 
             substr(input$theRange[2],9,10), "_", input$c_ls, ".csv")
    },
    
    content = function(file) {
      readr::write_excel_csv(S_con(), file)
    }
  )

  # downloadSplot -----------------------------------------------------------

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
    })

  # downloadTgragh ----------------------------------------------------------

  # トレンドグラフのダウンロード 
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
    })

  # downloadOutliers --------------------------------------------------------

  # 外れ値の時刻と外れ値のリストをダウンロード
  output$downloadOutliers <- downloadHandler(
    filename = function() {
      paste0("Outliers", substr(input$theRange[1],1,4), "_", substr(input$theRange[1],6,7), 
             substr(input$theRange[1],9,10), "_", substr(input$theRange[2],6,7), 
             substr(input$theRange[2],9,10),"_", input$c_ls, ".csv")
    },
    
    content = function(file) {
      readr::write_excel_csv(outliers_each(), file)
    }
  )
  
  
})