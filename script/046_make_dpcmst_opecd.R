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

# connect
con = dbConnect(SQLite(), 'output/db.sqlite', synchronous="off")

# shiny用のデータベースに接続
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

# con_shinyからdpcmst_mdc6を取得
dpcmst_mdc6 <- dbReadTable(con_shiny,'dpcmst_mdc6') %>% tibble() %>% print()

################################################################################

# setting

################################################################################

dir <- 'input/dpc_survey'

# file一覧の取得
files <- list.files(dir,full.names = T,recursive = T) %>%
  print()

# 'aggregation_by_surgery_by_disease'を含むfileに絞り込み
files <- files %>% 
  str_subset('agg_by_dpc') %>% 
  print()

filepath <- files[2]

################################################################################

# 前処理の関数を作成
make_dpcmst_ope<- function(filepath){
  
  # filepathからfyを取得
  fy <- str_extract(filepath,'[0-9]{4}') %>% print()
  
  # 数値にする
  fy <- as.integer(fy) %>% print()
  
  # fyが奇数だったら-1して偶数にしてdpcfyを取得
  dpcfy <- ifelse(fy %% 2 == 1,fy-1,fy) %>%
    print()
  
  # 2列だけ取り込み
  colname <- c('dpc','dpcname')
  
  # colnameの列名を使って、データを読み込み。読み込むのは最初の2列だけにする
  df <- readxl::read_xlsx(
    filepath,
    sheet = 3,
    col_names = colname,
    range=cell_cols("A:B"),
  ) 
  
  # 最初の3行を削除
  df <- df[-c(1:3),] %>% print()
  
  # dpcの最初の6桁からmdc6cdを作成
  df <- df %>% 
    mutate(mdc6cd = str_sub(dpc,1,6)) %>% 
    print()
  
  # dpcの9桁目10桁目からopecdを作成
  df <- df %>% 
    mutate(opecd = str_sub(dpc,9,10)) %>% 
    print()
  
  # fy列とdpcfy列を作成
  df <- df %>% 
    mutate(fy = fy) %>% 
    mutate(dpcfy = dpcfy) %>% 
    print()
  
  # mst_mdc6cdとfyをkeyにしてmdc6を結合
  df <- df %>% 
    left_join(dpcmst_mdc6,by=c('mdc6cd','dpcfy'='dpcfy')) %>% 
    print()
  
  # mdc6の文字数を取得
  df <- df %>% 
    mutate(mdc6_len = str_length(mdc6)) %>% 
    print()
  
  # dpcnameをmdc6_len+1文字分だけ先頭から削除
  df <- df %>% 
    mutate(dpcname = str_sub(dpcname,mdc6_len+1)) %>% 
    print()
  
  # 文頭の全角の括弧+全角スペースを削除
  df <- df %>% 
    mutate(tmp = str_remove(dpcname,'^（.*）　')) %>% 
    print()
  
  # other列で"　手術・処置等"よりも後ろを削除
  df <- df %>% 
    mutate(tmp = str_remove(tmp,'手術・処置等.*')) %>% 
    mutate(tmp = str_remove(tmp,'定義副傷病.*')) %>% 
    mutate(tmp = str_remove(tmp,'発症前Rankin.*')) %>%
    print()
  
  # 文頭と文末の全角スペースを削除
  df <- df %>% 
    mutate(tmp = str_remove(tmp,'^　')) %>% 
    mutate(tmp = str_remove(tmp,'　$')) %>% 
    print()
  
  # opecdがxxの場合はopeを定義なしにする
  df <- df %>% 
    mutate(tmp = ifelse(opecd == 'xx','定義なし',tmp)) %>% 
    print()
  
  # 列の整理
  dpcmst_ope <- df %>% 
    rename(ope=tmp) %>% 
    arrange(fy,dpcfy,mdc6cd,opecd,ope) %>% 
    distinct(fy,dpcfy,mdc6cd,opecd,ope) %>% 
    print()
  
  return(dpcmst_ope)
  
}

################################################################################

# 全filesに対して前処理の関数を適用して縦に結合
dpcmst_ope <- files %>% 
  map_df(make_dpcmst_ope) %>% 
  print()

################################################################################

# fyが不要かどうかを確認
dpcmst_ope <- dpcmst_ope %>% 
  group_by(dpcfy,mdc6cd,opecd,ope) %>% 
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(n) %>% 
  print()

# nが1のdataがないので、fyは不要と判断。
dpcmst_ope <- dpcmst_ope %>% 
  distinct(dpcfy,mdc6cd,opecd,ope) %>%
  print()

# ダブっているものを確認
dpcmst_ope <- dpcmst_ope %>% 
  group_by(mdc6cd,opecd,dpcfy) %>%
  mutate(dpl=n()) %>% 
  ungroup() %>%
  arrange(-dpl) %>% 
  print()

# dplが2以上かつ、opeが両眼もしくは片眼のものはope名を変更
dpcmst_ope <- dpcmst_ope %>% 
  mutate(ope = case_when(
    dpl ==1 ~ ope,
    str_detect(ope,'　両眼') ~ str_replace(ope,'　両眼',''),
    str_detect(ope,'　片眼') ~ str_replace(ope,'　片眼',''),
    TRUE ~ ope
  ))

# nが1のdataがないので、fyは不要と判断。
dpcmst_ope <- dpcmst_ope %>% 
  distinct(dpcfy,mdc6cd,opecd,ope) %>%
  print()

# ダブっているものを再確認
dpcmst_ope <- dpcmst_ope %>% 
  group_by(mdc6cd,opecd,dpcfy) %>%
  mutate(dpl=n()) %>% 
  ungroup() %>%
  arrange(-dpl) %>% 
  print()


# dpl列を削除
dpcmst_ope <- dpcmst_ope %>% 
  select(-dpl) %>% 
  arrange(dpcfy,mdc6cd,opecd) %>% 
  print()

dpcmst_ope

# dpcmst_opeをshiny.sqliteに書き込み
dbWriteTable(con_shiny, 'dpcmst_ope', dpcmst_ope, overwrite = TRUE,row_names = FALSE)

################################################################################

