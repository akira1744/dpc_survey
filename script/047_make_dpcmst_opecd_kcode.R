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

con_dpc = dbConnect(SQLite(), 'output/dpcmst.sqlite', synchronous="off")

# con_dpcのtable一覧を確認
dbListTables(con_dpc) %>% print()

# con_dpcのtableをすべて読み込みして、同名の変数に格納
dbListTables(con_dpc) %>% 
  map(~dbReadTable(con_dpc, .x)) %>% 
  set_names(dbListTables(con_dpc)) %>% 
  list2env(envir = .GlobalEnv)

# shiny用のデータベースに接続
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

dpcmst_ope <- dbReadTable(con_shiny,'dpcmst_ope') %>% tibble() %>% print()

################################################################################

# dpcmst_mdc10を作成
dpcmst_mdc10 <- dpcmst_mdc10 %>% 
  tibble() %>% 
  mutate(mdc6cd = str_c(MDCコード,分類コード)) %>% 
  rename(mdc2cd=MDCコード,opecd=対応コード,stymd=有効期間_開始日,enymd=有効期間_終了日) %>%
  print()

# 手術○_点数表名称を確認 → 手術4と手術5はなにも入っていなかった
dpcmst_mdc10 %>% 
  distinct(手術4_点数表名称) %>% 
  print()

# 先にreplace_naで空文字にする
dpcmst_mdc10 <- dpcmst_mdc10 %>% 
  mutate(手術1_点数表名称 = replace_na(手術1_点数表名称,''),
         手術2_点数表名称 = replace_na(手術2_点数表名称,''),
         手術3_点数表名称 = replace_na(手術3_点数表名称,''),
         手術4_点数表名称 = replace_na(手術4_点数表名称,''),
         手術5_点数表名称 = replace_na(手術5_点数表名称,''),
         手術1_Kコード = replace_na(手術1_Kコード,''),
         手術2_Kコード = replace_na(手術2_Kコード,''),
         手術3_Kコード = replace_na(手術3_Kコード,''),
         手術4_Kコード = replace_na(手術4_Kコード,''),
         手術5_Kコード = replace_na(手術5_Kコード,'')) %>% 
  glimpse()

# 手術1_点数表名称～手術5_Kコードまでを+で結合してkname と　kcodeを作成する。
dpcmst_mdc10 <- dpcmst_mdc10 %>% 
  mutate(kname = str_c(手術1_点数表名称,手術2_点数表名称,手術3_点数表名称,手術4_点数表名称,手術5_点数表名称,sep = '+'),
         kcode = str_c(手術1_Kコード,手術2_Kコード,手術3_Kコード,手術4_Kコード,手術5_Kコード,sep = '+')) %>% 
  glimpse()

# ++++$のように+で終わっているものは+を削除
dpcmst_mdc10 <- dpcmst_mdc10 %>% 
  mutate(kname = str_remove(kname,'\\++$'),
         kcode = str_remove(kcode,'\\++$')) %>% 
  glimpse()

# +が入っているデータを確認する
dpcmst_mdc10 %>% 
  filter(str_detect(kname,'\\+')) %>% 
  glimpse()

# stymdからyear,monthを作成
dpcmst_mdc10 <- dpcmst_mdc10 %>% 
  mutate(stymd=ymd(stymd)) %>% 
  mutate(year = year(stymd),
         month = month(stymd)) %>% 
  glimpse()

# fyを作成
dpcmst_mdc10 <- dpcmst_mdc10 %>% 
  mutate(fy = if_else(month >= 4,year,year-1)) %>% 
  glimpse()

# fyが奇数の時は-1してdpcfyを作成
dpcmst_mdc10 <- dpcmst_mdc10 %>% 
  mutate(dpcfy = if_else(fy %% 2 == 1,fy-1,fy)) %>% 
  glimpse()

# 必要列に絞り込み
dpcmst_kname <- dpcmst_mdc10 %>% 
  distinct(dpcfy,mdc6cd,opecd,kcode,kname) %>% 
  glimpse()

# 書き出し
dbWriteTable(con_shiny,'dpcmst_kname',dpcmst_kname,overwrite = T)



