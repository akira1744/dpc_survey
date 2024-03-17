
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

################################################################################

# shiny用のデータベースに接続
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

# con_shinyからdpcmst_mdc6を読み込み
dpcmst_mdc6 <- dbReadTable(con_shiny, 'dpcmst_mdc6') %>% tibble() %>% print()

################################################################################

# データベースのpath
db_dpc <- 'output/dpcmst.sqlite'

# connect
con_dpc = dbConnect(SQLite(), db_dpc, synchronous="off")

# con_dpcのtable一覧を確認
dbListTables(con_dpc) %>% print()

# con_dpcからdpcmst_mdc06_meisyo_icdcordを読み込み
dpcmst_icd <- dbReadTable(con_dpc, 'dpcmst_mdc06_meisyo_icdcord') %>% tibble() %>% print()

# 列名変更
dpcmst_icd <- dpcmst_icd %>% 
  mutate(mdc6cd = str_c(MDCコード,分類コード)) %>% 
  select(
    mdc6cd,
    icd = ICDコード,
    icdname=ICD名称,
    stymd=有効期間_開始日
  ) %>% print()

# stymdからyear,monthを作成
dpcmst_icd <- dpcmst_icd %>% 
  mutate(
    stymd = ymd(stymd),
    year = year(stymd),
    month = month(stymd),
  ) %>% print()

# 年度としてdpcfy列を作成
dpcmst_icd <- dpcmst_icd %>% 
  mutate(
    dpcfy = case_when(
      month >= 4 ~ year,
      month < 4 ~ year - 1
    )
  ) %>% print()

# 確認
dpcmst_icd %>% 
  count(stymd,dpcfy)


# 列順を調整
dpcmst_icd <- dpcmst_icd %>% 
  select(
    dpcfy,
    mdc6cd,
    icd,
    icdname,
  ) %>% print()

# 結合が問題ないか確認→OK
dpcmst_mdc6 %>% 
  left_join(dpcmst_icd, by = 'mdc6cd') %>%
  print()

# dpc_icdをcon_shinyに書き込み
dbWriteTable(con_shiny, 'dpc_icd', dpcmst_icd, overwrite = TRUE)

