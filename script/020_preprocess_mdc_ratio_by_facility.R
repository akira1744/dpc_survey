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
  str_subset('mdc_ratio_by_facility') %>% 
  print()

################################################################################

# 前処理用の関数
cleanup_mdc02data <- function(filepath){
  
  print(filepath)
  fy <- str_extract(filepath,'[0-9]{4}') 
  sheet=1
  skip=0
  nheader=2
  
  colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=skip) 
  
  colname <- colname %>% 
    str_replace_all('比率_','')
  
  # clean_colnameの列名を使って、データを読み込み
  df <- readxl::read_xlsx(
    filepath,
    sheet = sheet,
    col_names = colname,
    skip=nheader+skip
  ) 
  
  df <- df %>% 
    select(-全体,-通番,-施設名) %>% 
    rename(overall=件数_全体,no=告示番号)
  
  
  df <- df %>% 
    mutate(fy=fy,.before=no)
  
  # fyとnoとoverallをkeyにしてその他の行をlongにする
  df <- df %>% 
    pivot_longer(
      cols = -c(fy,no,overall),
      names_to = 'mdc2cd',
      values_to = 'ratio'
    ) 
  
  df <- df %>% 
    mutate(value=overall*ratio)
  
  df <- df %>% 
    mutate(value=round(value)) 
  
  df <- df %>% 
    select(fy,no,mdc2cd,value) %>% 
    mutate(mdc2cd = str_sub(mdc2cd,4,5)) 
  
  return(df)
}

# テスト
filepath <- files[1]
test <- cleanup_mdc02data(filepath) %>%
  print()


################################################################################

# filesのすべてのxlsxファイルに対して、cleanup_mdc10dataを実行して、縦に結合する
df <- files %>% 
  map_df(cleanup_mdc02data) %>% 
  print()

################################################################################

# noが0もしくはNAの行を削除
df <- df %>% 
  filter(!is.na(no)) %>% 
  filter(no!=0) %>%
  print()

# valueがNA・0の行を削除
df <- df %>% 
  filter(!is.na(value)) %>% 
  filter(value!=0) %>% 
  glimpse()

# mst_mstnoを読み込み
mst_hp <- dbReadTable(con,'mst_hp') %>% 
  tibble() %>% 
  select(mstno,fy,no) %>% 
  print()

# mst_mstnoと結合
agg_mdc2 <- df %>% 
  left_join(mst_hp,by=c('fy','no')) %>% 
  print()

# dbに書き込み
dbWriteTable(con, 'agg_mdc2', agg_mdc2, overwrite = T,row.names = F)

# dbのtable一覧を確認
dbListTables(con) %>% print()
