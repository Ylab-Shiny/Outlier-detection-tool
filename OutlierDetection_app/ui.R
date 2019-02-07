##############################################################################################################################
#### 外れ値検出ツール -- ui.R ################################################################################################
##############################################################################################################################
# ライブラリ一覧
{
  library(shiny)
  library(shinydashboard)
}

### 構成要素 ###
# header #
header <- dashboardHeader(title = "多変量時系列データの外れ値検出ツール", titleWidth = 500) ### headerの最終部分

# sidebar #
sidebar <- dashboardSidebar(
  # サイドバーメニュー
  sidebarMenu(
    menuItem("初期データセット", tabName = "initDF", icon = icon("table")),
    menuItem("外れ値の所在リスト", tabName = "OutlierList", icon = icon("list")),
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
    tabItem(tabName = "Dataset",
            h2("外れ値を欠損値に置き換えたデータセットをダウンロードすることができます"),
            dataTableOutput("dt_conv"), 
            downloadButton("downloadData", "データセットのダウンロード")),
    tabItem(tabName = "Summary", h2("初期データの基本統計量"), dataTableOutput("summary_ori"), 
            downloadButton("downloadSummaryB", "基本統計量(元データ)のダウンロード"),
            h2("外れ値を欠損値に置き換えた後の基本統計量"), dataTableOutput("summary_con"),
            downloadButton("downloadSummaryA", "基本統計量(変換データ)のダウンロード")),
    tabItem(tabName = "Scatter", plotOutput("scatterPlot"),
            downloadButton("downloadSplot", "散布図のダウンロード")),
    tabItem(tabName = "Trend", plotOutput("Trendgragh"),
            downloadButton("downloadTgragh", "トレンドグラフのダウンロード"))
  )
) ### bodyの最終部分

## 組み立て ##
dashboardPage(header, sidebar, body)