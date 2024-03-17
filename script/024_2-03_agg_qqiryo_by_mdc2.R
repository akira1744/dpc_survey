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

# データベースのpath
db <- 'output/db.sqlite'

# connect
con = dbConnect(SQLite(), db, synchronous="off")

################################################################################

# setting

################################################################################

dir <- 'input/dpc_survey'

# file一覧の取得
files <- list.files(dir,full.names = T,recursive = T) %>%
  print()

# 'aggregation_by_surgery_by_disease'を含むfileに絞り込み
files <- files %>% 
  str_subset('2-03予定緊急医療入院医療機関別MDC別集計') %>% 
  print()

################################################################################

filepath <- files[1]
filepath

# 前処理用の関数
cleanup_kinkyu_mdc02 <- function(filepath){
  
  print(filepath)
  fy <- str_extract(filepath,'[0-9]{4}') 
  fy
  sheet=1
  skip=0
  nheader=3
  
  colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=skip) %>% 
    print()
  
  # clean_colnameの列名を使って、データを読み込み
  df <- readxl::read_xlsx(
    filepath,
    sheet = sheet,
    col_names = colname,
    skip=nheader+skip
  ) 
  
  df <- df %>% 
    rename(no=告示番号) %>% 
    select(no,contains('予定外入院'),contains('救急医療入院')) %>% 
    print()
  
  df <- df %>% 
    mutate(fy=fy,.before=no) %>% 
    print()
  
  
  # fyとnoとoverallをkeyにしてその他の行をlongにする
  df <- df %>% 
    pivot_longer(
      cols = -c(fy,no),
      names_to = 'name',
      values_to = 'value'
    ) %>% 
    print()
  
  # mdc02を抜粋
  df <- df %>% 
    mutate(mdc2cd = str_sub(name,5,6)) %>%
    glimpse()
  
  # valueが-の行は削除
  df <- df %>% 
    filter(value!='-') %>% 
    print()
  
  # 出力用に整形
  df <- df %>% 
    select(fy,no,mdc2cd,name,value)
  
  return(df)
}

# テスト
filepath <- files[1]
test <- cleanup_kinkyu_mdc02(filepath) %>%
  print()

################################################################################

# filesのすべてのxlsxファイルに対して、cleanup_mdc10dataを実行して、縦に結合する
df <- files %>% 
  map_df(cleanup_kinkyu_mdc02) %>% 
  print()

df <- df %>% 
  mutate(value=as.numeric(value)) %>%
  print()

# name列をもとにkb列を作り、yoteigaiとqqiryoに分ける
df <- df %>% 
  mutate(kb = if_else(str_detect(name,'予定外入院'), 'yoteigai','qqiryo')) %>%
  print()

# kbでpivot_wide
df <- df %>% 
  select(-name) %>% 
  pivot_wider(
    names_from = kb,
    values_from = value
  ) %>% 
  print()

################################################################################

# mst_mstnoを読み込み
mst_hp <- dbReadTable(con,'mst_hp') %>% 
  tibble() %>% 
  select(mstno,fy,no) %>% 
  print()

# mst_mstnoと結合
df <- df %>% 
  left_join(mst_hp,by=c('fy','no')) %>% 
  print()

# dbに書き込み
dbWriteTable(con, 'agg_qqiryo_by_mdc2', df, overwrite = T,row.names = F)

# dbのtable一覧を確認
dbListTables(con) %>% print()
