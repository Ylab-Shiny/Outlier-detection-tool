##############################################################################################################################
#### 外れ値検出ツール -- ui.R ################################################################################################
##############################################################################################################################

# 読み込むパッケージ ---------------------------------------------------------------

library(shiny)
library(shinydashboard)


### 構成要素 ###

# ヘッダー --------------------------------------------------------------------

header <- dashboardHeader(title = "時系列データの外れ値検出ツール",
                          titleWidth = 500) 


# サイドバー -------------------------------------------------------------------

sidebar <- dashboardSidebar(
  # サイドバーメニュー
  sidebarMenu(
    menuItem("はじめに", tabName = "Intro", icon = icon("question")),
    menuItem("アップロードされたデータ", tabName = "initDF", icon = icon("table")),
    menuItem("外れ値の時刻と外れ値", tabName = "Outliers", icon = icon("crosshairs")),
    menuItem("処理後のデータセット", tabName = "Dataset", icon = icon("file")),
    menuItem("基本統計量", tabName = "Summary", icon = icon("info")),
    menuItem("データの散布図", tabName = "Scatter", icon = icon("print")),
    menuItem("トレンドグラフ", tabName = "Trend", icon = icon("dashboard"))
  ),
  
  # ファイルのアップロードUI
  fileInput("file", "csvファイルをアップロードしてください",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  uiOutput("columns_ls"),
  
  uiOutput("DateRange"),
  
  uiOutput("UI_k1"),
  
  uiOutput("UI_k2")
)


# ボディ ---------------------------------------------------------------------

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Intro",
            h2("このツールについて"),
            p("このツールに解析したい時系列データ（単変量・多変量どちらでも可）をアップロードすることで
              データ中の外れ値を特定することができます。"),
            p("アップロードするデータの仕様については、以下のダウンロードボタンから
              テストデータをダウンロードし、テストデータの形式に合わせるようにしてください。"),
            downloadButton("downloadTest", "テストデータのダウンロード"),
            h2("外れ値を検出する手法について"),
            p("Tukey's fencesは、四分位範囲（IQR)を利用してデータセット内の外れ値を検出します。
              この方法は統計的平均および標準偏差とは無関係であるので、データセット内の極値によって影響されることはありません。"),
            p("4分位範囲（IQR）は次のように定義されます："),
            h3("IQR = (第3四分位値) - (第1四分位値)"),
            p("信頼区間の上限と下限を次のように定義されます："),
            h3("(下限閾値) = (第1四分位値) - k1 * IQR"),
            h3("(上限閾値) = (第3四分位値) - k2 * IQR"),
            p("デフォルトの係数は", strong("k1が1.5"), ",", strong("k2が3.0"), 
              "に設定されています（ただし、ユーザーがカスタマイズできます）。"),
            p("上限閾値を超え、下限閾値を下回る値は、外れ値と見なされます。図として示すと以下のようになります。
              この図では赤くプロットされた点が外れ値ということになります。"),
            imageOutput("preImage")
            ),
    
    tabItem(tabName = "initDF", dataTableOutput("initData")),
    tabItem(tabName = "Outliers", h2(textOutput("d_date")), dataTableOutput("dt_each"), 
            downloadButton("downloadOutliers", "ダウンロード")),
    tabItem(tabName = "Dataset",
            h2("外れ値を欠損値に置き換えた単変量時系列データをダウンロードすることができます"),
            dataTableOutput("dt_conv"), 
            downloadButton("downloadData", "データセットのダウンロード")),
    tabItem(tabName = "Summary", h2("元データの基本統計量"), dataTableOutput("summary_ori"), 
            downloadButton("downloadSummaryB", "基本統計量(元データ)のダウンロード"),
            h2("外れ値を欠損値に置き換えた後の基本統計量"), dataTableOutput("summary_con"),
            downloadButton("downloadSummaryA", "基本統計量(変換データ)のダウンロード")),
    tabItem(tabName = "Scatter", 
            h2("元データの散布図を描画します"),
            plotOutput("scatterPlot"),
            downloadButton("downloadSplot", "散布図のダウンロード"),
            h2(textOutput("text_fence_values"))
            ),
    tabItem(tabName = "Trend", 
            h2("元データのトレンドグラフを描画します"),
            plotOutput("Trendgragh"),
            downloadButton("downloadTgragh", "トレンドグラフのダウンロード"))
  )
)


# アセンブリ -------------------------------------------------------------------

dashboardPage(header, sidebar, body)