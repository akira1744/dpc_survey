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

# shiny用のデータベースに接続
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

################################################################################

# dpcmst_mdc2を作成
dpcmst_mdc2 <- dpcmst_mdc02 %>% 
  tibble() %>% 
  select(mdc2cd=MDCコード,mdc2=MDC名称) %>% 
  distinct(mdc2cd,mdc2) %>% 
  mutate(mdc2 = case_when(
    mdc2cd=='01' ~ '神経系',
    mdc2cd=='02' ~ '眼科系',
    mdc2cd=='03' ~ '耳鼻咽喉科系',
    mdc2cd=='04' ~ '呼吸器系',
    mdc2cd=='05' ~ '循環器系',
    mdc2cd=='06' ~ '消化器系',
    mdc2cd=='07' ~ '筋骨格系',
    mdc2cd=='08' ~ '皮膚系',
    mdc2cd=='09' ~ '乳房系',
    mdc2cd=='10' ~ '内分泌系',
    mdc2cd=='11' ~ '腎尿路系',
    mdc2cd=='12' ~ '女性生殖器系',
    mdc2cd=='13' ~ '血液系',
    mdc2cd=='14' ~ '新生児系',
    mdc2cd=='15' ~ '小児系',
    mdc2cd=='16' ~ '外傷系',
    mdc2cd=='17' ~ '精神系',
    mdc2cd=='18' ~ 'その他',
    TRUE ~ 'error'
  )) %>% 
  print()

# dpcmst_mdc2をshiny.sqliteに書き込み
dbWriteTable(con_shiny, 'dpcmst_mdc2', dpcmst_mdc2, overwrite = TRUE,row_names = FALSE)

################################################################################

# dpcmst_mdc6を作成
dpcmst_mdc6 <- dpcmst_mdc06 %>% 
  tibble() %>% 
  mutate(mdc6cd = str_c(MDCコード,分類コード)) %>% 
  select(mdc2cd=MDCコード,mdc6cd,mdc6=名称,stymd=有効期間_開始日,enymd=有効期間_終了日) %>%
  print()

# stymdからyear列とmonth列を作成
dpcmst_mdc6 <- dpcmst_mdc6 %>% 
  mutate(stymd = ymd(stymd)) %>% 
  mutate(year = year(stymd),month = month(stymd)) %>% 
  print()

# yearとmonthからdpcfyの列を作成
dpcmst_mdc6 <- dpcmst_mdc6 %>% 
  mutate(dpcfy = case_when(
    month >= 4 ~ year,
    month < 4 ~ year - 1
  )) %>% 
  print()

# dpcfyの確認
dpcmst_mdc6 %>% 
  count(stymd,year,month,dpcfy)

# 必要列だけに整理
dpcmst_mdc6 <- dpcmst_mdc6 %>% 
  select(dpcfy,mdc2cd,mdc6cd,mdc6) %>% 
  print()

# dpcmst_mdc6をshiny.sqliteに書き込み

dbWriteTable(con_shiny, 'dpcmst_mdc6', dpcmst_mdc6, overwrite = TRUE,row_names = FALSE)

################################################################################
