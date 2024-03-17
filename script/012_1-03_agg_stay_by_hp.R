################################################################################

# date: 2023-11-28

# author: nuajbec

# 複数年分のデータが1sheetにまとまっているので、最新年だけ処理すればOK

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

# データベースのpath
db <- 'output/db.sqlite'

# connect
con = dbConnect(SQLite(), db, synchronous="off")

# dbのtable一覧を確認
dbListTables(con) %>% print()

################################################################################

# setting

################################################################################

dir <- 'input/dpc_survey'

# file一覧の取得
files <- list.files(dir,full.names = T,recursive = T) %>%
  print()

# 'aggregation_by_surgery_by_disease'を含むfileに絞り込み
files <- files %>% 
  str_subset('1-03在院日数の状況') %>% 
  print()

# 最新年度を指定
nend <- '2021'

# mstnoを結合するためにmst_hpを取得しておく  
mst_hp <- tbl(con,'mst_hp') %>% 
  filter(fy==nend) %>% 
  select(mstno,no) %>% 
  collect() %>% 
  glimpse()

################################################################################

filepath <- files[1]
print(filepath)

sheet=1
skip=0
nheader=3

colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=skip) 
colname
  
# clean_colnameの列名を使って、データを読み込み
df <- readxl::read_xlsx(
  filepath,
  sheet = sheet,
  col_names = colname,
  skip=nheader+skip
) %>% 
  glimpse()
  
df <- df %>% 
  select(告示番号,contains('平均値')) %>%
  glimpse()

# 告示番号が欠損しているデータを削除
df <- df %>% 
  filter(!is.na(告示番号)) %>% 
  glimpse()
  
# fyとnoとoverallをkeyにしてその他の行をlongにする
df <- df %>% 
  pivot_longer(
    cols = -告示番号,
    names_to = 'nend_str',
    values_to = 'stay'
  ) %>%
  glimpse()

df <- df %>% 
  mutate(nend_num = str_extract(nend_str,'[0-9]+')) %>%
  mutate(nend_num = if_else(str_detect(nend_str,'元年度'),'1',nend_num)) %>%
  mutate(nend_num = as.numeric(nend_num)) %>%
  glimpse()
  
df <- df %>% 
  mutate(nend_base = case_when(
    str_detect(nend_str,'平成') ~ 1988,
    str_detect(nend_str,'令和') ~ 2018,
  )) %>%
  glimpse()

df <- df %>% 
  mutate(nend= nend_base + nend_num) %>%
  mutate(nend=as.character(nend)) %>%
  glimpse()

df <- df %>% 
  select(告示番号,nend,stay) %>% 
  print()

# 2018年度以前のデータは削除
df <- df %>% 
  filter(nend>='2018') 
  
df <- df %>% 
  left_join(mst_hp,by=c('告示番号'='no')) %>%
  glimpse()

df <- df %>% 
  select(mstno,fy=nend,stay) %>%
  print()

# daysがNAの行を削除
df <- df %>% 
  filter(!is.na(stay))

# dbに書き込み
dbWriteTable(con, 'agg_stay_by_hp', df, overwrite = T,row.names = F)

# dbのtable一覧を確認
dbListTables(con) %>% print()
