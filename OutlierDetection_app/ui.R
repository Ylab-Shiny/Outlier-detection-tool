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
header <- dashboardHeader(title = "スマートBEMSの外れ値検出ツール", titleWidth = 500) ### headerの最終部分

# sidebar #
sidebar <- dashboardSidebar(
  # サイドバーメニュー
  sidebarMenu(
    menuItem("外れ値の所在リスト", tabName = "OutlierList", icon = icon("list")),
    menuItem("外れ値の時刻と外れ値", tabName = "Outliers", icon = icon("crosshairs"))
  ),
  
  # ファイルのアップロードUI
  fileInput("file", "csvファイルをアップロードしてください",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  uiOutput("columns_out")
) ### sidebarの最終部分

# body #
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "OutlierList", dataTableOutput("DataTable")),
    tabItem(tabName = "Outliers", dataTableOutput("dt_each"))
  )
) ### bodyの最終部分

## 組み立て ##
dashboardPage(header, sidebar, body)