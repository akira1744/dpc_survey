################################################################################

# date:20231129

# author:nujabec

################################################################################
# read packages
pacman::p_load(
  lubridate, # 日付操作 
  janitor, # 集計表 tabyl(),adorn_totals()
  openxlsx,
  readxl,
  writexl, # Excel出力 write_xlsx()
  flextable, # 見やすい表作成
  gtsummary, # 見やすい統計表作成
  esquisse, # マウスでグラフ作成 esquisser()
  ggrepel, # ggplotのテキスト位置自動調整 geom_text_repel() 
  ggridges, # リッジプロット 変数の分布をグループ別に出力 geom_density()
  gghighlight, # ggplotでにhighlight機能を追加 gghighlight()
  patchwork, # ggplotのグラフを結合 
  scales, # ggplotのメモリをパーセントにするのに使用 percent
  export, # パワポ出力 graph2ppt() table2ppt()
  officer, # 汎用オフィス系出力
  summarytools, # 探索的データ分析(EDA) dfSummary()
  DataExplorer, # 探索的データ分析(EDA)　create_report()
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

# setpath
file <- 'input/00450021_医療施設動態調査/2022/n0001.csv'

# cp932のcsvを読み込み。skip=9。範囲は1列名だけ読み込み
df <- read_csv(file,locale = locale(encoding = "CP932"),skip = 9,col_names = F)

# 1列目だけに絞り込み
df <- df %>% select(x=X1) %>% print()

# 末尾のスペースを削除
df <- df %>% mutate(x = str_trim(x)) %>% print()

# 全角スペースを半角スペースに変換
df <- df %>% mutate(x = str_replace_all(x, "　", " ")) %>% print()

# 数字だけを抽出
df <- df %>% mutate(cd = str_extract(x, "[0-9]+")) %>% print()

# スペース以降の文字だけを抽出。
df <- df %>% mutate(name = str_extract(x, "(?<=\\s).*$")) %>% print()

# nameのスペースを削除
df <- df %>% mutate(name = str_trim(name)) %>% print()

# cdの桁数を調べる
df <- df %>% mutate(nchar = nchar(cd)) %>% print()

df %>% count(nchar)


# 2桁の場合にprefcdとpref列を作ってfillで下方向に穴埋め
df <- df %>%
  mutate(prefcd = if_else(nchar == 2, cd, NA)) %>%
  mutate(pref = if_else(nchar == 2, name, NA)) %>%
  fill(prefcd) %>% 
  fill(pref) %>%
  print()

# 4桁の場合にiryocdとiryo列を作ってfillで下方向に穴埋め
df <- df %>%
  mutate(iryocd = if_else(nchar == 4, cd, NA)) %>%
  mutate(iryo = if_else(nchar == 4, name, NA)) %>%
  fill(iryocd) %>% 
  fill(iryo) %>%
  print()

# 5桁の場合にcitycdとcity列を作ってfillで下方向に穴埋め
df <- df %>%
  mutate(citycd = if_else(nchar == 5, cd, NA)) %>%
  mutate(city = if_else(nchar == 5, name, NA)) %>%
  fill(citycd) %>% 
  fill(city) %>%
  print()

# 5桁の列だけを抜き出してmstにする
mst_district <- df %>% filter(nchar == 5) %>%
  select(-x,-cd,-name,-nchar) %>% 
  print()

################################################################################

# 施設マスタを読み込んで市町村コードとマッチするか確認

# dbから読み込み
mst_hp <- dbReadTable(con, 'mst_hp') %>% 
  rename(citycd=市町村番号) %>% 
  tibble() %>% 
  print()

# 結合
mst_hp_district <- mst_hp %>% 
  left_join(mst_district, by = c('citycd')) %>% 
  print()

# 都道府県の列を削除
mst_hp_district <- mst_hp_district %>% select(-都道府県) %>% glimpse()

################################################################################

# mst_districtをdbに書き込み
dbWriteTable(con, 'mst_district', mst_district, overwrite = T,row_name=F)

# mst_hp_districtをdbに書き込み
dbWriteTable(con, 'mst_hp_district', mst_hp_district, overwrite = T,row_name=F)

# dbのtable一覧を確認
dbListTables(con) %>% print()

