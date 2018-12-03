##############################################################################################################################
#### 外れ値特定ツール -- ui.R ################################################################################################
##############################################################################################################################
# ライブラリ一覧
{
  library(shiny)
  library(shinydashboard)
  library(shinyBS)
  library(shinythemes)
}

### 構成要素 ###
# header #
header <- dashboardHeader(title = "外れ値検出ツール", titleWidth = 500) ### headerの最終部分

# sidebar #
sidebar <- dashboardSidebar(
  # サイドバーメニュー
  sidebarMenu(
    menuItem("初期データ", tabName = "initDF", icon = icon("table")),
    menuItem("外れ値の所在リスト", tabName = "OutlierList", icon = icon("list")),
    menuItem("外れ値の時刻と外れ値", tabName = "Outliers", icon = icon("crosshairs")),
    menuItem("置き換え後のデータ", tabName = "Dataset", icon = icon("file")),
    menuItem("集計", tabName = "Summary", icon = icon("info")),
    menuItem("データの散布図", tabName = "Scatter", icon = icon("print")),
    menuItem("トレンドグラフ", tabName = "Trend", icon = icon("dashboard"))
  ),
  
  # ファイルのアップロードUI
  fileInput("file", "csvファイルをアップロードしてください",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  uiOutput("columns_ls"),
  
  uiOutput("DateRange")
) ### sidebarの最終部分

# body #
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "initDF", dataTableOutput("initData")),
    tabItem(tabName = "OutlierList", h2(textOutput("d_date")),
            dataTableOutput("DataTable")),
    tabItem(tabName = "Outliers", dataTableOutput("dt_each"), 
            downloadButton("downloadOutliers", "ダウンロード")),
    tabItem(tabName = "Dataset", dataTableOutput("dt_conv"), 
            downloadButton("downloadData", "データセットのダウンロード")),
    tabItem(tabName = "Summary", h2("外れ値を含めた集計"), dataTableOutput("summary_ori"), 
            downloadButton("downloadSummaryB", "集計(元データ)のダウンロード"),
            h2("外れ値を欠損値に置き換えた後の集計"), dataTableOutput("summary_con"),
            downloadButton("downloadSummaryA", "集計(変換データ)のダウンロード")),
    tabItem(tabName = "Scatter", plotOutput("scatterPlot"),
            downloadButton("downloadSplot", "散布図のダウンロード")),
    tabItem(tabName = "Trend", plotOutput("Trendgragh"),
            downloadButton("downloadTgragh", "トレンドグラフのダウンロード"))
  )
) ### bodyの最終部分

## 組み立て ##
dashboardPage(header, sidebar, body)