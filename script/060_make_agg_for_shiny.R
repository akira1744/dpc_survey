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

################################################################################

# dpcのdataを読み込み

################################################################################

# データベースのpath
db_dpc <- 'output/dpcmst.sqlite'

# connect
con_dpc = dbConnect(SQLite(), db_dpc, synchronous="off")

# con_dpcのtable一覧を確認
dbListTables(con_dpc) %>% print()

# con_dpcのtableをすべて読み込みして、同名の変数に格納
dbListTables(con_dpc) %>% 
  map(~dbReadTable(con_dpc, .x)) %>% 
  set_names(dbListTables(con_dpc)) %>% 
  list2env(envir = .GlobalEnv)

################################################################################

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

# mstnoをkeyにpref_iryoを結合するための準備
mst_latest_pref_iryo <- mst_latest %>% 
  tibble() %>% 
  select(mstno,pref_iryo) %>% 
  print()

# 2次医療圏のdpcbed数を集計
mst_latest_dpcbed <- mst_latest %>% 
  group_by(pref_iryo) %>% 
  summarise(pref_iryo_dpcbed = sum(dpcbed)) %>%
  print()

################################################################################

# agg by mstno by fy

################################################################################

# 2次医療圏ごとの集計

agg_mdc2

# pref_iryoを結合
agg_long_pref_iryo <- tibble(agg_mdc2) %>% 
  left_join(mst_latest_pref_iryo, by = 'mstno') %>% 
  print()

# pref_iryoの集計
agg_long_pref_iryo <- agg_long_pref_iryo %>% 
  group_by(pref_iryo,fy) %>% 
  summarise(pref_iryo_value = sum(value)) %>% 
  ungroup() %>% 
  arrange(desc(pref_iryo_value)) %>% 
  print()

# pref_iryo_bedの結合
agg_long_pref_iryo_bed <- agg_long_pref_iryo %>% 
  left_join(mst_latest_dpcbed, by = 'pref_iryo') %>% 
  print()

################################################################################

# 施設ごとの集計

# agg_hpの作成
agg_long_hp <- agg_mdc2 %>% 
  group_by(mstno,fy) %>%
  summarise(value = sum(value)) %>% 
  ungroup() %>%
  arrange(desc(fy),desc(value)) %>% 
  print()

# pref_iryoの結合
agg_long_hp <- agg_long_hp %>% 
  left_join(mst_latest_pref_iryo, by = 'mstno') %>% 
  left_join(agg_long_pref_iryo, by = c('pref_iryo','fy')) %>% 
  print()

# 2次医療圏内のシェア率を計算
agg_long_hp <- agg_long_hp %>% 
  mutate(pref_iryo_share = value / pref_iryo_value) %>% 
  print()

# pref_iryoのdataを削除
agg_long_hp <- agg_long_hp %>% 
  select(-pref_iryo,-pref_iryo_value) %>% 
  print()

# stayを追加
agg_long_hp <- agg_long_hp %>% 
  left_join(agg_stay_by_hp, by = c('mstno','fy')) %>% 
  print()

# qqを追加
agg_long_hp <- agg_long_hp %>% 
  left_join(agg_qq_by_hp, by = c('mstno','fy')) %>% 
  print()

# yoteigaiとqqiryoを追加
agg_long_hp <- agg_long_hp %>% 
  left_join(agg_qqiryo_by_hp, by = c('mstno','fy')) %>% 
  print()

agg_long_hp

# dbに書き出し
dbWriteTable(con_shiny, 'agg_long_hp', agg_long_hp, overwrite = T, row.names = F)

################################################################################

# mdc2ごとの集計

# agg_mdc2の整形
agg_long_mdc2 <- agg_mdc2 %>% 
  select(mstno,fy,mdc2cd,value) %>% 
  arrange(desc(fy),desc(value)) %>% 
  tibble() %>% 
  print()

# pref_iryoの結合
agg_long_mdc2 <- agg_long_mdc2 %>% 
  left_join(mst_latest_pref_iryo, by = 'mstno') %>% 
  left_join(agg_long_pref_iryo, by = 'pref_iryo') %>% 
  print()

# fyごとmdc2ごとpref_iryoごとのpref_iryo_valueを計算
agg_long_mdc2 <- agg_long_mdc2 %>% 
  group_by(fy,mdc2cd,pref_iryo) %>% 
  mutate(pref_iryo_value = sum(value)) %>%
  ungroup() %>% 
  print()

# 2次医療圏内のシェア率を計算
agg_long_mdc2 <- agg_long_mdc2 %>% 
  mutate(pref_iryo_share = value / pref_iryo_value) %>% 
  print()

# pref_iryoのdataを削除
agg_long_mdc2 <- agg_long_mdc2 %>% 
  select(-pref_iryo,-pref_iryo_value) %>% 
  print()

# qqを追加
agg_qq_by_mdc2 <- agg_qq_by_mdc2 %>% select(-no) %>% tibble() %>% print()

agg_long_mdc2 <- agg_long_mdc2 %>% 
  left_join(agg_qq_by_mdc2, by = c('mstno','fy','mdc2cd')) %>% 
  print()

# yoteigaiとkinkyuを追加
agg_qqiryo_by_mdc2 <- agg_qqiryo_by_mdc2 %>% select(-no) %>% tibble() %>% print()

agg_long_mdc2 <- agg_long_mdc2 %>% 
  left_join(agg_qqiryo_by_mdc2, by = c('mstno','fy','mdc2cd')) %>% 
  print()

# qqを率に直す
agg_long_mdc2 <- agg_long_mdc2 %>% 
  mutate(qq = qq / value) %>% 
  print()

# yoteigaiとqqiryoを率に直す
agg_long_mdc2 <- agg_long_mdc2 %>% 
  mutate(qqiryo = qqiryo / value) %>% 
  mutate(yoteigai = yoteigai / value) %>%
  print()


# dbに書き出し
dbWriteTable(con_shiny, 'agg_long_mdc2', agg_long_mdc2, overwrite = T, row.names = F)

################################################################################

# agg_mdc6の作成

agg_mdc10 <- agg_mdc10 %>% tibble()

agg_long_mdc6 <- agg_mdc10 %>% 
  mutate(stay_sum=stay * value) %>% 
  group_by(mstno,fy,mdc6cd) %>%
  summarise(value = sum(value),
            stay_sum = sum(stay_sum)) %>%
  ungroup() %>%
  arrange(desc(fy),desc(value)) %>% 
  print()

agg_long_mdc6 <- agg_long_mdc6 %>% 
  mutate(stay = stay_sum/value) %>% 
  select(-stay_sum) %>% 
  print()

# pref_iryoの結合
agg_long_mdc6 <- agg_long_mdc6 %>% 
  left_join(mst_latest_pref_iryo, by = 'mstno') %>% 
  left_join(agg_long_pref_iryo, by = 'pref_iryo') %>% 
  print()

# fyごとmdc6ごとpref_iryoごとのpref_iryo_valueを計算
agg_long_mdc6 <- agg_long_mdc6 %>% 
  group_by(fy,mdc6cd,pref_iryo) %>% 
  mutate(pref_iryo_value = sum(value)) %>%
  ungroup() %>% 
  print()

# 2次医療圏内のシェア率を計算
agg_long_mdc6 <- agg_long_mdc6 %>% 
  mutate(pref_iryo_share = value / pref_iryo_value,.after=value) %>% 
  print()

# pref_iryoのdataを削除
agg_long_mdc6 <- agg_long_mdc6 %>% 
  select(-pref_iryo,-pref_iryo_value) %>% 
  print()

# dbに書き出し
dbWriteTable(con_shiny, 'agg_long_mdc6', agg_long_mdc6, overwrite = T, row.names = F)


################################################################################

# agg_mdc10の整形
agg_long_mdc10 <- agg_mdc10 %>% 
  select(mstno,fy,mdc6cd,opecd,value,stay) %>% 
  tibble() %>% 
  arrange(desc(fy),desc(value)) %>% 
  print()

# pref_iryoの結合
agg_long_mdc10 <- agg_long_mdc10 %>% 
  left_join(mst_latest_pref_iryo, by = 'mstno') %>% 
  left_join(agg_long_pref_iryo, by = 'pref_iryo') %>% 
  print()

# fyごとmdc10ごとpref_iryoごとのpref_iryo_valueを計算
agg_long_mdc10 <- agg_long_mdc10 %>% 
  group_by(fy,mdc6cd,opecd,pref_iryo) %>% 
  mutate(pref_iryo_value = sum(value)) %>%
  ungroup() %>% 
  print()

# 2次医療圏内のシェア率を計算
agg_long_mdc10 <- agg_long_mdc10 %>% 
  mutate(pref_iryo_share = value / pref_iryo_value) %>% 
  print()

# pref_iryoのdataを削除
agg_long_mdc10 <- agg_long_mdc10 %>% 
  select(-pref_iryo,-pref_iryo_value) %>% 
  print()

# stayを最後に
agg_long_mdc10 <- agg_long_mdc10 %>% 
  relocate(stay,.after=pref_iryo_share) %>%
  print()

# dbに書き出し
dbWriteTable(con_shiny, 'agg_long_mdc10', agg_long_mdc10, overwrite = T, row.names = F)

################################################################################
