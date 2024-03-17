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

# file一覧の取得
files <- list.files('input/dpc_survey',full.names = T,recursive = T) %>% print()

# filesの中からfacility_summary_table.xlsxを含むファイルを抽出
files <- files %>% 
  str_subset('facility_summary_table.xls') %>% 
  print()

filepath <- files[1]


# 前処理の関数を作成
get_facility_mst <- function(filepath){
  
  # filepathから年度を取得
  fy <- filepath %>% 
    str_extract('[0-9]{4}') 

  # 1つ目のファイルを読み込み
  df <- readxl::read_xlsx(filepath,sheet=1) 
  
  # 列名を取得
  colname <- df %>% colnames() 
  
  # 列名を整形
  colname <- colname %>% 
    str_replace_all('\r\n','') %>%
    str_replace_all('※[0-9]','') %>% 
    str_replace('.*[0-9元]年度','') 
  
  # 列名を変更
  colnames(df) <- colname
  
  # 年度列を追加
  df <- df %>% 
    mutate(fy = fy,.before = 1) 
  
  # 通番列が欠損しているデータを削除
  df <- df %>% 
    filter(!is.na(通番))

  return(df)
  
}

# filesの全データに前処理の関数を適用してから縦に結合
mst_hp <- files %>% 
  map_dfr(get_facility_mst) %>% 
  print()

# データの確認
mst_hp %>% glimpse()

# 通番の告示番号の扱い方を確認
mst_hp %>% 
  filter(str_detect(施設名,'埼玉石心会病院')) %>% 
  print()

# 通番が数字以外の列を確認
mst_hp %>% 
  filter(!str_detect(通番,'[0-9]')) %>% 
  print()

################################################################################

# 作業用のデータを作成
mst_no <- mst_hp %>% 
  select(fy,告示番号,通番) %>%
  print()

# 2018年のデータを抽出
m2018 <- mst_no %>% 
  filter(fy=='2018') %>% 
  select('no2018'=告示番号) %>% 
  print()

# 2019年のデータを抽出
m2019 <- mst_no %>% 
  filter(fy=='2019') %>% 
  select('no2019'=告示番号,'no2018'=通番) %>% 
  print()

# 2020年のデータを抽出
m2020 <- mst_no %>% 
  filter(fy=='2020') %>% 
  select('no2020'=告示番号,'no2019'=通番) %>% 
  print()

# 2021年のデータを抽出
m2021 <- mst_no %>% 
  filter(fy=='2021') %>% 
  select('no2021'=告示番号,'no2020'=通番) %>% 
  print()

# 2018年のデータに2019年のデータを結合
mst <- m2018 %>% 
  full_join(m2019,by = 'no2018') %>% 
  print()

# 2018年のデータに2020年のデータを結合
mst <- mst %>% 
  full_join(m2020,by = 'no2019') %>% 
  print()

# 2018年のデータに2021年のデータを結合
mst <- mst %>% 
  full_join(m2021,by = 'no2020') %>% 
  print()

# すべての列のNA列を-に変換
mst <- mst %>% 
  mutate_all(replace_na,'-') %>% 
  print()

# すべての列を.でつないでmstnoの列を追加。
mst <- mst %>% 
  mutate(mstno = str_c(no2018,no2019,no2020,no2021,sep = '.'),.before=no2018) %>% 
  print()

# mstnoをkeyにしてlongにする
mst_long <- mst %>% 
  pivot_longer(cols = c(-mstno),names_to = 'fy',values_to = 'no') %>% 
  print()

# fy列からnoを削除
mst_long <- mst_long %>% 
  mutate(fy = str_remove(fy,'no')) %>% 
  print()

# 

################################################################################

# 作業用のデータを作成
mst_no <- mst_hp %>% 
  select(fy,告示番号,都道府県,施設名) %>%
  print()

# 2018年のデータを抽出
m2018 <- mst_no %>% 
  filter(fy=='2018') %>% 
  select('no2018'=告示番号,都道府県,施設名) %>% 
  print()

# 2019年のデータを抽出
m2019 <- mst_no %>% 
  filter(fy=='2019') %>% 
  select('no2019'=告示番号,都道府県,施設名) %>% 
  print()

# 2020年のデータを抽出
m2020 <- mst_no %>% 
  filter(fy=='2020') %>% 
  select('no2020'=告示番号,都道府県,施設名) %>% 
  print()

# 2021年のデータを抽出
m2021 <- mst_no %>% 
  filter(fy=='2021') %>% 
  select('no2021'=告示番号,都道府県,施設名) %>% 
  print()

# 2018年のデータに2019年のデータを結合
mst <- m2018 %>% 
  full_join(m2019,by =c('都道府県','施設名')) %>% 
  print()

# 2018年のデータに2020年のデータを結合
mst <- mst %>% 
  full_join(m2020,by =c('都道府県','施設名')) %>% 
  print()

# 2018年のデータに2021年のデータを結合
mst <- mst %>% 
  full_join(m2021,by = c('都道府県','施設名')) %>% 
  print()

# すべての列のNA列を-に変換
mst <- mst %>% 
  mutate_all(replace_na,'-') %>% 
  print()

# すべての列を.でつないでmstnoの列を追加。
mst <- mst %>% 
  mutate(mstno = str_c(no2018,no2019,no2020,no2021,sep = '.'),.before=no2018) %>% 
  print()

# mstnoをkeyにしてlongにする
mst_long2 <- mst %>% 
  pivot_longer(cols = c(-都道府県,-施設名,-mstno),names_to = 'fy',values_to = 'no') %>% 
  print()

# fy列からnoを削除
mst_long2 <- mst_long2 %>% 
  mutate(fy = str_remove(fy,'no')) %>% 
  print()

################################################################################


# mst_longとmst_hpを結合
mst_long_hp <- mst_long %>% 
  full_join(mst_hp,by = c('fy','no'='告示番号')) %>% 
  print()

# 市町村番号と施設名が一緒なのに、mstnoがuniqueでないデータを確認
for_modify_list <- mst_long_hp %>% 
  filter(no!='-') %>% 
  group_by(都道府県,施設名) %>%
  mutate(n = n(),
         mstno_distinct= n_distinct(mstno)) %>%
  filter(mstno_distinct!=1) %>%
  select(mstno:病院類型) %>% 
  arrange(施設名,fy) %>% 
  print()

# 
for_modify_list　<- distinct(for_modify_list,都道府県,施設名)

for_modify_list_mst2 <- for_modify_list %>% 
  left_join(mst_long2,by=c('都道府県','施設名')) %>%
  print()

for_modify_list_mst2 <- for_modify_list_mst2 %>% 
  distinct(mstno,都道府県,施設名) %>% 
  print()

for_modify_list_mst2 <- for_modify_list_mst2 %>% 
  rename(mstno2=mstno) %>%
  print()

mst_long_hp <- mst_long_hp %>%
  left_join(for_modify_list_mst2,by=c('都道府県','施設名')) %>%
  print()

# mstno2に値があれば、その値でmstnoを書き換える
mst_long_hp <- mst_long_hp %>% 
  mutate(mstno = if_else(is.na(mstno2),mstno,mstno2)) %>% 
  print()

# mstno2を削除
mst_long_hp <- mst_long_hp %>% 
  select(-mstno2) %>% 
  print()

mst_hp <- mst_long_hp %>% 
  filter(!is.na(施設名))

# dbに書き出し
DBI::dbWriteTable(con,'mst_hp',mst_hp,overwrite = T,row.names = F)

# dbのtable一覧を確認
dbListTables(con) %>% print()
