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
  RSQLite, # SQLite
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
  str_subset('aggregation_by_surgery_by_disease') %>% 
  print()

# テスト
filepath <- files[1]

# ###############################################################################

# 前処理の関数を作成
cleanup_mdc10data <- function(filepath){
  
  # ファイル名から年度を抽出
  fy <- str_replace(filepath,'input/dpc_survey/','') %>% 
    str_extract('^[0-9]{4}') 
  
  sheet=1
  skip=0
  nheader=4
  
  colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=skip) 
  
  # clean_colnameの列名を使って、データを読み込み
  df <- readxl::read_xlsx(
    filepath,
    sheet = sheet,
    col_names = colname,
    skip=nheader+skip
  ) 
  
  df <- df %>% 
    select(-通番,-施設名) %>% 
    rename(no = 告示番号)
  
  # 列名に"輸血以外の再掲列"を削除
  df <- df %>% 
    select(-contains('輸血以外の再掲')) 

  # 告示番号と通番と施設名をkeyにしてそれ以外の列をlongにする
  df <- df %>% 
    pivot_longer(
      cols = -c(no),
      names_to = 'key',
      values_to = 'value'
    ) 
  
  # value列が0のデータは削除
  df <- df %>% 
    filter(value!='-')

  # keyの最初の_までの文字列をmdc6cdにする
  df <- df %>% 
    mutate(mdc6cd = str_extract(key, "^[^_]+"))
  

  # key列の最後の2桁をopecdという列にする
  df <- df %>% 
    mutate(
      opecd = str_sub(key,-2,-1)
    ) 
  
  # by列を作り、keyに件数が含まれる場合はn、そうでない場合はstayを入れる
  df <- df %>% 
    mutate(by = if_else(str_detect(key,'件数'),'n','stay'))
  
  # key列を削除
  df <- df %>% 
    select(-key)
  
  # pivot_widetrで、軸はnoとmdc6cdとopecd、byで横に展開、値はvalue
  df <- df %>% 
    pivot_wider(
      id_cols = c(no,mdc6cd,opecd),
      names_from = by,
      values_from = value
    )
  
  # nはinteger型にする。valueにrename
  df <- df %>% 
    mutate(n = as.integer(n)) %>% 
    rename(value=n)
  
  # stayはdouble型にする
  df <- df %>% 
    mutate(stay = as.double(stay))
  

  # 年度の列を作成
  df <- df %>% 
    mutate(fy = fy)
  
  # 列順整理
  df <- df %>% 
    select(fy,no,mdc6cd,opecd,value,stay) 
  
  return(df)
}


################################################################################ 

# テスト
df <- cleanup_mdc10data(filepath) %>% 
  print()

################################################################################

# filesのすべてのxlsxファイルに対して、cleanup_mdc10dataを実行して、縦に結合する
df <- files %>% 
  map_dfr(cleanup_mdc10data) %>%
  print()

################################################################################

# mst_mstnoを読み込み
mst_hp <- dbReadTable(con,'mst_hp') %>% 
  tibble() %>% 
  select(mstno,fy,no) %>% 
  print()

# mst_mstnoと結合
agg_mdc10 <- df %>% 
  left_join(mst_hp,by=c('fy','no')) %>% 
  print()

# dbに書き込み
dbWriteTable(con, 'agg_mdc10', agg_mdc10, overwrite = T,row.names = F)

# dbのtable一覧を確認
dbListTables(con) %>% print()
