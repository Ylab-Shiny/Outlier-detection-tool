##############################################################################################################################
#### 中部大電力消費実績描画アプリ -- ui.R ####################################################################################
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
    menuItem("外れ値リスト", tabName = "OutlierList", icon = icon("table"))
  ),
  
  # ファイルのアップロードUI
  fileInput("file", "csvファイルをアップロードしてください",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
) ### sidebarの最終部分

# body #
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "OutlierList",
            dataTableOutput("DataTable"))
  )
) ### bodyの最終部分

## 組み立て ##
dashboardPage(header, sidebar, body)