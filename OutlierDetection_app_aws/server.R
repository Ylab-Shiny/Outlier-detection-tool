##############################################################################################################################
#### 外れ値検出ツール AWS -- server.R ############################################################################################
##############################################################################################################################

# 読み込むパッケージ ---------------------------------------------------------------
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)


# shinyServer関数外 ----------------------------------------------------------

OutlierDetection <- function(BivariateDataframe, k1, k2) {
  var1 <- BivariateDataframe[[2]] %>% data.frame()
  qq <- data.frame(quantile(var1, c(0.25, 0.75), na.rm = T))
  Q1 <- qq[1, 1]
  Q3 <- qq[2, 1]
  
  outer_l_Q1 <- Q1 - k1 * (Q3 - Q1)
  outer_m_Q3 <- Q3 + k2 * (Q3 - Q1)
  outer_ll <- which(var1 < outer_l_Q1)
  outer_mm <- which(var1 > outer_m_Q3)
  
  # 重複なく昇順に行番号を抽出
  row_num_out <- unique(c(outer_ll, outer_mm)) %>% sort()
  
  # 外れ値の出力
  outer_outlier <- cbind.data.frame(BivariateDataframe[row_num_out,], row_number = row_num_out)
  return(outer_outlier)
  
}

# テストデータ ------------------------------------------------------------------
TestData <- read.csv("TestData.csv", header = F, skip = 1, row.names = NULL, stringsAsFactors = F)
TestData <- as_tibble(TestData)
label <- paste0(substr(TestData$V1, 1, 10), " ", substr(TestData$V1, 12, 19))
TestData$V1 <- label

# ブラウザでの立ち上げ
options(shiny.launch.browser = T)
# アップロードできるファイルのサイズを150MBまでとする
options(shiny.maxRequestSize = 150*1024^2)

# shinyServer関数内 ----------------------------------------------------------

shinyServer(function(input, output, session){
  
  
  # イメージ図の出力 ----------------------------------------------------------------
  
  output$preImage <- renderImage({
    filename <- normalizePath(file.path('./', paste('Splot_sample', '.png', sep='')))
    
    list(src = filename, align = "center")
    
  }, deleteFile = FALSE)
  
  # テストデータのダウンロード -----------------------------------------------------------
  
  output$downloadTest <- downloadHandler(
    filename = function() {
      "TestData.csv"
    },
    
    content <- function(file) {
      write.csv(TestData, file, row.names = F)
    }
  )
  
  # 初期データ initData ----------------------------------------------------------
  
  initData <- reactive({
    if (!is.null(input$file)) {
      firstData <- read.csv(input$file$datapath, header = F, skip = 1, row.names = NULL, stringsAsFactors = F)
      firstData <- as_tibble(firstData)
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
  
  output$d_date <- renderText(theDate())
  
  # initData2 ---------------------------------------------------------------
  
  initData2 <- reactive({
    firstData <- initData() %>% select(label, input$c_ls)
    firstData[[1]] <- as.POSIXct(firstData[[1]], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    
    return(firstData)
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
  
  # 下側フェンスの定数k1入力UI ---------------------------------------------------------
  
  output$UI_k1 <- renderUI({
    if (!is.null(input$file)) {
      UI <- numericInput("k1", "下側フェンスの定数 k1 を入力してください", value = 1.5, step = 0.5) 
    } else {
      UI <- NULL
    }
    return(UI)
  })
  
  # 上側フェンスの定数k2入力UI ---------------------------------------------------------
  
  output$UI_k2 <- renderUI({
    if (!is.null(input$file)) {
      UI <- numericInput("k2", "上側フェンスの定数 k2 を入力してください", value = 3.0, step = 0.5)
    } else {
      UI <- NULL
    }
    return(UI)
  })
  
  # 下側フェンスの値 ----------------------------------------------------------------
  
  lower_value <- reactive({
    if (!is.null(input$file)) {
      Var <- initData() %>% select(input$c_ls)
      qq <- data.frame(quantile(Var[[1]], c(0.25, 0.75), na.rm = T))
      Q1 <- qq[1, 1]
      Q3 <- qq[2, 1]
      
      l_value <- Q1 - input$k1[1] * (Q3 - Q1)
    } else {
      l_value <- NULL
    }
    return(l_value)
  })
  
  # 上側フェンスの値 ----------------------------------------------------------------
  
  upper_value <- reactive({
    if (!is.null(input$file)) {
      Var <- initData() %>% select(input$c_ls)
      qq <- data.frame(quantile(Var[[1]], c(0.25, 0.75), na.rm = T))
      Q1 <- qq[1, 1]
      Q3 <- qq[2, 1]
      
      u_value <- Q3 + input$k2[1] * (Q3 - Q1)
    } else {
      u_value <- NULL
    }
    return(u_value)
  })
  
  # 下限閾値と上限閾値のテキスト出力 --------------------------------------------------------
  
  output$text_fence_values <- renderText({
    if (!is.null(input$file)) {
      Text <- paste0("下限閾値が", round(lower_value(), 2), ", 上限閾値が", round(upper_value(), 2), "です")
    } else {
      Text <- NULL
    }
    return(Text)
  })
  
  # outliers_each -----------------------------------------------------------
  
  # 選択された列名の外れ値を検出
  outliers_each <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% select(label, input$c_ls)
      secondData <- OutlierDetection(firstData, input$k1[1], input$k2[1])
      names(secondData) <- c("時刻", "観測値", "行番号")
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
  
  # 検出された外れ値NAで置き換えたデータセット
  ConvData <- reactive({
    if (!is.null(input$file)) {
      firstData <- initData() %>% select(label, input$c_ls)
      ids <- outliers_each()[[3]]
      firstData[[2]][ids] <- NA
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
    } else {
      secondData <- NULL
    }
    
    return(secondData)
  })
  
  # dt_conv -----------------------------------------------------------------
  
  # 外れ値をNAに変換したデータセットをDTで出力
  output$dt_conv <- renderDataTable({
    datatable(ConvData(),
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
      Data_trend <- initData2() %>% 
        mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      names(Data_trend) <- c("Time", "Values")
      
      p <- Data_trend %>% ggplot(aes_string(x="Time", y="Values")) + geom_line() +
        ggtitle(paste0(input$c_ls, "のトレンドグラフ")) + ylim(min(ConvData()[[2]]), max(ConvData()[[2]])) +
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
      firstData <- initData2()
      
      secondData <- firstData %>% mutate(date = substr(label, 1, 10)) %>% 
        filter(date >= input$theRange[1] & date <= input$theRange[2]) %>% select(-date)
      names(secondData) <- c("Time", "Values")
    } else {
      secondData <- NULL
    }
    
    return(secondData)
  })
  
  
  # 散布図オブジェクト Splot ---------------------------------------------------------
  
  Splot <- reactive({
    if (!is.null(input$file)) {
      lower <- lower_value()
      upper <- upper_value()
      p <- Data_scat() %>% ggplot(aes(x = Time, y = Values, color = lower<Values & Values<upper)) +
        geom_point() + ylim(lower-200, upper+200) + 
        ggtitle(paste0(input$c_ls, "の散布図")) + 
        labs(x="時間", y="データ値", color="信頼区間内か？") + 
        geom_hline(yintercept = lower) + geom_hline(yintercept = upper) +
        annotate("text",x=Data_scat()[[1]][10], y=lower-150, parse=T, size=4, label="{Q[1]}-{k[1]}*({Q[3]}-{Q[1]})") +
        annotate("text", x=Data_scat()[[1]][10], y=upper+150, parse=T, size=4, label="{Q[1]}+{k[2]}*({Q[3]}-{Q[1]})")
      
      
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
      write.csv(ConvData(), file, row.names = F)
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
      write.csv(S_ori(), file, row.names = F)
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
      write.csv(S_con(), file, row.names = F)
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
      write.csv(outliers_each(), file, row.names = F)
    }
  )
  
  
})