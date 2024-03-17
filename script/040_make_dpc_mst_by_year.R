################################################################################

# 作成日:20231128

# 作成者:nujabec

################################################################################
# パッケージの読み込み
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
  tidyverse, # packageセット(ggplot2 tibble tidyr readr purrr dplyr stringr forcats)
  tidylog # tidyverseにlog出力機能追加
  )

# 自作関数の読み込み
source('/home/rstudio/srv/function/index.R')

# function以外の変数を削除
clean_vars()

################################################################################

# 診断群分類(DPC)電子点数表の読込み
filename <- list.files('input/dpc_mst',pattern = '.xlsx$',full.names = T) %>% print()

# setting
filepath <- 'input/dpc_mst/診断群分類_DPC_電子点数表_20190522.xlsx'
fy <- '2018'
outputdir <- str_c('dpcmst/',fy,'/')


################################################################################

# １）ＭＤＣ名称の前処理
sheet <- '１）ＭＤＣ名称'
nheader <- 2
nskip <- 0

# ヘッダを取得
colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=nskip) %>%
  print()

# データの読み込み
dpcmst_mdc02 <- readxl::read_excel(filepath,
                                   sheet = sheet,
                                   skip = nheader+nskip, 
                                   col_names = colname,
                                   col_types = "text") %>% 
  glimpse()

# rds保存
dpcmst_mdc02 %>% 
  saveRDS(str_c(outputdir,'dpcmst_mdc02.rds'))

################################################################################

# ２）分類名称の前処理
sheet <- '２）分類名称'
nheader <- 2
nskip <- 0

# ヘッダを取得
colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=nskip) %>%
  print()


# データの読み込み
dpcmst_mdc06 <- readxl::read_excel(filepath,
                                   sheet = sheet,
                                   skip = nheader+nskip, 
                                   col_names = colname,
                                   col_types = "text") %>% 
  glimpse()

# rds保存
dpcmst_mdc06 %>% 
  saveRDS(str_c(outputdir,'dpcmst_mdc06.rds'))

################################################################################

# ４）ＩＣＤの前処理
sheet <- '４）ＩＣＤ'
nheader <- 2
nskip <- 0

# ヘッダを取得
colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=nskip) %>%
  print()


# データの読み込み
dpcmst_mdc06_meisyo_icdcord <- readxl::read_excel(filepath,
                                   sheet = sheet,
                                   skip = nheader+nskip, 
                                   col_names = colname,
                                   col_types = "text") %>% 
  glimpse()

# rds保存
dpcmst_mdc06_meisyo_icdcord %>% 
  saveRDS(str_c(outputdir,'dpcmst_mdc06_meisyo_icdcord.rds'))

################################################################################

# ６）手術の前処理
sheet <- '６）手術 '
nheader <- 2
nskip <- 0

# ヘッダを取得
colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=nskip) %>%
  print()

# データの読み込み
dpcmst_mdc10 <- readxl::read_excel(filepath,
                              sheet = sheet,
                              skip = nheader+nskip, 
                              col_names = colname,
                              col_types = "text") %>% 
  glimpse()

# rds保存
dpcmst_mdc10 %>% 
  saveRDS(str_c(outputdir,'dpcmst_mdc10.rds'))

################################################################################

# ７）手術・処置等１
sheet <- '７）手術・処置等１'
nheader <- 2
nskip <- 0

# ヘッダを取得
colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=nskip) %>%
  print()

# データの読み込み
dpcmst_ope_syoti1 <- readxl::read_excel(filepath,
                                   sheet = sheet,
                                   skip = nheader+nskip, 
                                   col_names = colname,
                                   col_types = "text") %>% 
  glimpse()

# rds保存
dpcmst_ope_syoti1 %>% 
  saveRDS(str_c(outputdir,'dpcmst_ope_syoti1.rds'))

################################################################################

# ８）手術・処置等２
filepath <- filename[1]
sheet <- '８）手術・処置等２'
nheader <- 2
nskip <- 0

# ヘッダを取得
colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=nskip) %>%
  print()

# データの読み込み
dpcmst_ope_syoti2 <- readxl::read_excel(filepath,
                                        sheet = sheet,
                                        skip = nheader+nskip, 
                                        col_names = colname,
                                        col_types = "text") %>% 
  glimpse()

# rds保存
dpcmst_ope_syoti2 %>% 
  saveRDS(str_c(outputdir,'dpcmst_ope_syoti2.rds'))

################################################################################

# ９）定義副傷病名
filepath <- filename[1]
sheet <- '９）定義副傷病名'
nheader <- 2
nskip <- 0

# ヘッダを取得
colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=nskip) %>%
  print()

# データの読み込み
dpcmst_hukusyobyo <- readxl::read_excel(filepath,
                                        sheet = sheet,
                                        skip = nheader+nskip, 
                                        col_names = colname,
                                        col_types = "text") %>% 
  glimpse()

# rds保存
dpcmst_hukusyobyo %>% 
  saveRDS(str_c(outputdir,'dpcmst_hukusyobyo.rds'))

################################################################################

# 11）診断群分類点数表
filepath <- filename[1]
sheet <- '11）診断群分類点数表'
nheader <- 2
nskip <- 2

# ヘッダを取得
colname <- get_colname_from_multiline_data(filepath,sheet=sheet,nheader=nheader,skip=nskip) %>%
    print()
  
# データの読み込み
dpcmst_mdc14 <- readxl::read_excel(filepath,
                                     sheet = sheet,
                                     skip = nheader+nskip, 
                                     col_names = colname,
                                     col_types = "text") %>% 
  glimpse()

# rds保存
dpcmst_mdc14 %>% 
  saveRDS(str_c(outputdir,'dpcmst_mdc14.rds'))

################################################################################

# 厚労省のマスタをRDBへ書き出し

################################################################################

# print_table_list(RDB)
# 
# # DB connect
# RDB <- connect_RDB()
# 
# # dpcmst_mdc02をRDBに書き出し
# dplyr::copy_to(RDB,
#                df = dpcmst_mdc02,
#                name = 'dpcmst_mdc02',
#                overwrite = TRUE,
#                temporary = FALSE)
#                
# 
# ################################################################################
# 
# # dpcmst_mdc06をRDBに書き出し
# dplyr::copy_to(RDB,
#                df = dpcmst_mdc06,
#                name = 'dpcmst_mdc06',
#                overwrite = TRUE,
#                temporary = FALSE)
# 
# ################################################################################
# 
# # dpcmst_mdc06_meisyo_icdcordをRDBに書き出し
# dplyr::copy_to(RDB,
#                df = dpcmst_mdc06_meisyo_icdcord,
#                name = 'dpcmst_mdc06_meisyo_icdcord',
#                overwrite = TRUE,
#                temporary = FALSE)
# 
# ################################################################################
# 
# # dpcmst_mdc10をRDBに書き出し
# dplyr::copy_to(RDB,
#                df = dpcmst_mdc10,
#                name = 'dpcmst_mdc10',
#                overwrite = TRUE,
#                temporary = FALSE)
# 
# ################################################################################
# 
# # dpcmst_ope_syoti1をRDBに書き出し
# dplyr::copy_to(RDB,
#                df = dpcmst_ope_syoti1,
#                name = 'dpcmst_ope_syoti1',
#                overwrite = TRUE,
#                temporary = FALSE)
# 
# ################################################################################
# 
# # dpcmst_ope_syoti2をRDBに書き出し
# dplyr::copy_to(RDB,
#                df = dpcmst_ope_syoti2,
#                name = 'dpcmst_ope_syoti2',
#                overwrite = TRUE,
#                temporary = FALSE)
# 
# ################################################################################
# 
# # dpcmst_hukusyobyoをRDBに書き出し
# dplyr::copy_to(RDB,
#                df = dpcmst_hukusyobyo,
#                name = 'dpcmst_hukusyobyo',
#                overwrite = TRUE,
#                temporary = FALSE)
# 
# tbl(RDB,'dpcmst_mdc14') %>% glimpse()
# 
# ################################################################################
# 
# # dpcmst_mdc14をRDBに書き出し
# dplyr::copy_to(RDB,
#                df = dpcmst_mdc14,
#                name = 'dpcmst_mdc14',
#                overwrite = TRUE,
#                temporary = FALSE)

















