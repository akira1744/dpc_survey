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
  RSQLite, # SQLite操作
  tidyverse, # packageセット(ggplot2 tibble tidyr readr purrr dplyr stringr forcats)
  tidylog # tidyverseにlog出力機能追加
)

# read my function
source('/home/rstudio/srv/function/index.R')

# clear global variables
clean_vars()

################################################################################

# データベースのpath
db <- 'output/db.sqlite'

# connect
con = dbConnect(SQLite(), db, synchronous="off")

# dbのtable一覧を確認
dbListTables(con) %>% print()

# dbのtableをすべて読み込みして、同名の変数に格納
dbListTables(con) %>% 
  map(~dbReadTable(con, .x)) %>% 
  set_names(dbListTables(con)) %>% 
  list2env(envir = .GlobalEnv)


# shiny用のdbを作成
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

# con_shinyのtableをすべて読み込みして、同名の変数に格納
dbListTables(con_shiny) %>% 
  map(~dbReadTable(con_shiny, .x)) %>% 
  set_names(dbListTables(con_shiny)) %>% 
  list2env(envir = .GlobalEnv)

################################################################################

# data source
# https://community.qlik.com/t5/Japan-Healthcare/%E4%BB%A4%E5%92%8C%EF%BC%93%E5%B9%B4%E5%BA%A6-R3-DPC%E8%AA%BF%E6%9F%BB%E3%81%AE-quot-%E6%96%BD%E8%A8%AD%E6%A6%82%E8%A6%81%E8%A1%A8-quot-%E3%81%AB%E4%BD%8F%E6%89%80%E3%81%A8%E7%B7%AF%E5%BA%A6%E7%B5%8C%E5%BA%A6%E3%82%92%E4%BB%98%E3%81%91%E3%81%BE%E3%81%97%E3%81%9F/td-p/2053221

# read data
df <- readxl::read_xlsx('input/緯度経度/R3施設概要表_DPC退院患者調査.xlsx') %>% 
  glimpse()

fy <- '2021'

df <- df %>% 
  select(no=`告示番号\r\n※1`,
         lat=Latitude,
         long=Longitude
         ) %>% 
  mutate(fy=fy,.before=no) %>% 
  glimpse() 

# mstno結合用のdata
mst_hp <- tibble(mst_hp) %>%
  select(mstno,fy,no) %>% 
  glimpse()

# mstnoを結合
mst_latest_latlon <- mst_hp %>% 
  filter(fy=='2021') %>% 
  left_join(df,by=c('fy','no')) %>% 
  glimpse()


# dbに書き出し
dbWriteTable(con_shiny, 'mst_latest_latlon', mst_latest_latlon, overwrite = T, row.names = F)
