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
# shiny用のdbを作成
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

################################################################################

# mstの作成

################################################################################

# 必要列を抽出、列名変更
mst_hp_district_tmp <- mst_hp_district %>% 
  tibble() %>% 
  select(mstno,fy,hpname=施設名,prefcd,pref,iryocd,iryo,citycd,city,bed=病床総数,dpcbed=DPC算定病床数) %>% 
  glimpse()

################################################################################

# 病床数は年度ごとの数字を残すので別のdbとして保持
mst_bed <- mst_hp_district_tmp %>% 
  select(mstno,fy,bed,dpcbed) %>% 
  print()

# 書き出し
dbWriteTable(con_shiny, 'mst_bed', mst_bed, overwrite = T, row.names = F)

################################################################################

# 医療機関ごとに最新年度の行に絞ってマスタを作る

# mstnoごとに最もfyが大きい行を抽出
mst_latest <- mst_hp_district_tmp %>% 
  arrange(desc(fy)) %>%
  distinct(mstno,.keep_all=T) %>%
  print()

################################################################################

# hpnameでuniqueにするための処理

# hpname_pref_city列を作成
mst_latest <- mst_latest %>% 
  mutate(hpname_pref_city=str_c(hpname,'_',pref,'_',city)) %>% 
  glimpse()

# hpnameの重複数をdpl列に入れる
mst_latest <- mst_latest %>% 
  group_by(hpname) %>% 
  mutate(dpl=n()) %>% 
  ungroup() %>%
  print()

# dpc>1の時はhpnameをhpname_pref_cityに書き換え
mst_latest <- mst_latest %>% 
  mutate(hpname=if_else(dpl>1,hpname_pref_city,hpname)) %>% 
  select(-dpl,-hpname_pref_city) %>% 
  print()

################################################################################

# iryoとcityを名前でuniqueにするための処理

# pref_iryo列を作成
mst_latest <- mst_latest %>% 
  mutate(pref_iryo=str_c(pref,'_',iryo)) %>% 
  glimpse()

# pref_city列を作成
mst_latest <- mst_latest %>% 
  mutate(pref_city=str_c(pref,'_',city)) %>% 
  glimpse()

################################################################################

# mst_bed_rangeを作成
mst_bed_range <- mst_latest %>% 
  summarise(bed_min=min(bed),bed_max=max(bed),dpcbed_min=min(dpcbed),dpcbed_max=max(dpcbed)) %>%
  print()

# 書き出し
dbWriteTable(con_shiny, 'mst_bed_range', mst_bed_range, overwrite = T, row.names = F)

# mst_prefを作成
mst_pref <- mst_latest %>% 
  distinct(prefcd,.keep_all=T) %>% 
  arrange(prefcd) %>% 
  select(pref) %>% 
  print()

# 書き出し
dbWriteTable(con_shiny, 'mst_pref', mst_pref, overwrite = T, row.names = F)

# mst_pref_iryo_cityを作成
mst_pref_iryo_city <- mst_latest %>% 
  arrange(prefcd,iryocd,citycd) %>% 
  distinct(prefcd,iryocd,citycd,.keep_all=T) %>% 
  select(pref,pref_iryo,pref_city) %>% 
  print()

# 書き出し
dbWriteTable(con_shiny, 'mst_pref_iryo_city', mst_pref_iryo_city, overwrite = T, row.names = F)

################################################################################

mst_latest

# 必要列に絞り込み
mst_latest <- mst_latest %>% 
  select("mstno", "hpname", "pref",iryocd,iryo,citycd,city, "pref_iryo", "pref_city", "bed", "dpcbed") %>% 
  print()

# 書き出し
dbWriteTable(con_shiny, 'mst_latest', mst_latest, overwrite = T, row.names = F)

################################################################################


