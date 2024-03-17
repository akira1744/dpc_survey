################################################################################

# date: 2023-11-28

# author: nuajbec

################################################################################
# read packages
pacman::p_load(
  lubridate, # 日付操作 
  janitor, # 集計表 tabyl(),adorn_totals()
  openxlsx,
  readxl,
  writexl, # Excel出力 write_xlsx()
  ggrepel, # ggplotのテキスト位置自動調整 geom_text_repel() 
  ggridges, # リッジプロット 変数の分布をグループ別に出力 geom_density()
  gghighlight, # ggplotでにhighlight機能を追加 gghighlight()
  patchwork, # ggplotのグラフを結合 
  scales, # ggplotのメモリをパーセントにするのに使用 percent
  export, # パワポ出力 graph2ppt() table2ppt()
  RSQLite,
  tidyverse, # packageセット(ggplot2 tibble tidyr readr purrr dplyr stringr forcats)
  tidylog # tidyverseにlog出力機能追加
)

# read my function
source('/home/rstudio/srv/function/index.R')

# clear global variables
clean_vars()

# shiny用のデータベースに接続
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

# con_shinyからすべてのtableをtibbleで読み込み
dbListTables(con_shiny) %>% 
  map(~dbReadTable(con_shiny, .x)) %>% 
  set_names(dbListTables(con_shiny)) %>% 
  list2env(envir = .GlobalEnv)

################################################################################

dpcmst_mdc2 <- tibble(dpcmst_mdc2) %>% print()
dpcmst_mdc6 <- tibble(dpcmst_mdc6) %>% print()
dpcmst_ope <- tibble(dpcmst_ope) %>% print()
dpcmst_kname <- tibble(dpcmst_kname) %>% print()
