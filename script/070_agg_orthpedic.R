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

# shiny用のdbを作成
con_shiny = dbConnect(SQLite(), 'output/shiny.sqlite', synchronous="off")

# table一覧を確認
dbListTables(con_shiny) %>% print()

# con_shinyのtableをすべて読み込みして、同名の変数に格納
dbListTables(con_shiny) %>% 
  map(~dbReadTable(con_shiny, .x)) %>% 
  set_names(dbListTables(con_shiny)) %>% 
  list2env(envir = .GlobalEnv)

##################################################################################

# 施設別集計
agg_long_hp <- tibble(agg_long_hp) %>% print()

# 施設別mdc2別集計
agg_long_mdc2 <- tibble(agg_long_mdc2) %>% print()

# 施設別mdc6別集計
agg_long_mdc6 <- tibble(agg_long_mdc6) %>% print()

# 施設別mdc10別集計
agg_long_mdc10 <- tibble(agg_long_mdc10) %>% print()

# mdc2のマスタ
dpcmst_mdc2 <- tibble(dpcmst_mdc2) %>% print()

# mdc6のマスタ
dpcmst_mdc6 <- tibble(dpcmst_mdc6) %>% print()

# mdc6とicdの対応表
dpc_icd <- tibble(dpc_icd) %>% print()

# opeのマスタ
dpcmst_ope <- tibble(dpcmst_ope) %>% print()

# knameのマスタ
dpcmst_kname <- tibble(dpcmst_kname) %>% print()

# 病床数マスタ(年度別推移あり)
mst_bed <- tibble(mst_bed) %>% print()

# 医療機関マスタ(医療機関ごとの最新年度のデータ)
mst_latest <- tibble(mst_latest) %>% print()

# 都道府県,2次医療圏,市町村マスタ
mst_pref_iryo_city <- tibble(mst_pref_iryo_city) %>% print()

##################################################################################

# 対象範囲の病院を抽出
hp <- mst_latest %>% 
  filter(pref=='埼玉県') %>% 
  select(mstno,hpname,pref,city) %>% 
  glimpse()

# 実績と結合
df <- agg_long_mdc10 %>% 
  inner_join(hp, by='mstno') %>%
  glimpse()

# dpcmstと結合するためにdpcfyを作成
df <- df %>% 
  mutate(fy=as.numeric(fy)) %>% 
  mutate(dpcfy=if_else(fy%%2==0,fy,fy-1),.after=fy) %>% 
  print()

# dpcmst_mdc6を結合
df <- df %>% 
  left_join(select(dpcmst_mdc6,mdc6cd,dpcfy,mdc6),by=c('mdc6cd','dpcfy')) %>% 
  glimpse()

dpcmst_ope %>% 
  group_by(mdc6cd,opecd,dpcfy) %>%
  filter(n()>1) %>% 
  print()

# dpcmst_opeを結合
df <- df %>% 
  left_join(dpcmst_ope,by=c('mdc6cd','opecd','dpcfy')) %>% 
  glimpse()


# 指定された病院に絞り込み
hpnames <- '埼玉石心会病院|豊岡第一|所沢中央|埼玉医科大学病院|赤心堂|防衛'

df %>% 
  count(hpname) %>% 
  filter(str_detect(hpname,hpnames)) %>% 
  print()  

df <- df %>% 
  filter(str_detect(mdc6cd,'^07|^16')) %>%
  filter(str_detect(hpname,hpnames)) %>%
  print()

# mdc6を絞り込むために一旦集計表を作って出力
df %>% 
  count(mdc6cd,mdc6,opecd,ope) %>%
  write_xlsx('output/tmp_mdc6_ope.xlsx')

mdc6cds <- '070080|070160|160610|160700|160720|160740|160760|160780'

df %>% 
  filter(str_detect(mdc6cd,mdc6cds)) %>%
  count(mdc6cd,mdc6,opecd,ope) %>%
  write_xlsx('output/tmp_mdc6_ope_filter.xlsx')

# 絞り込みと手術名変更
df <- df %>% 
  filter(str_detect(mdc6cd,mdc6cds)) %>%
  mutate(ope = if_else(str_detect(ope,'骨折観血的手術'),'骨折観血的手術',ope)) %>%
  glimpse()

# 集計列だけに絞り込み
df <- df %>% 
  select(hpname,fy,mdc6cd,mdc6,opecd,ope,value)


# 病院名整形
df <- df %>% 
  mutate(hpname=if_else(str_detect(hpname,'石心会'),'埼玉石心会病院',hpname)) %>%
  mutate(hpname=if_else(str_detect(hpname,'所沢中央病院'),'所沢中央病院',hpname)) %>%
  mutate(hpname=if_else(str_detect(hpname,'赤心堂病院'),'赤心堂病院',hpname)) %>%
  print()

# dpc病名の表記ブレを確認
df %>% 
  distinct(mdc6cd,mdc6) %>% 
  group_by(mdc6cd) %>%
  filter(n()>1)

# opeの表記ブレを確認
df %>% 
  distinct(mdc6cd,opecd,ope) %>% 
  group_by(mdc6cd,opecd) %>%
  filter(n()>1)

df <- df %>% 
  mutate(fy=str_c(fy,'年度')) %>% 
  glimpse()

# 手術なしを除外
df %>% count(ope)
df <- df %>% 
  filter(!str_detect(ope,'なし'))

# 赤心堂を除外
df <- df %>% 
  filter(!str_detect(hpname,'赤心堂'))

# 病院ごと年度別の集計表を作成する
agg_by_hp <- df %>% 
  group_by(hpname,fy) %>% 
  summarise(value=sum(value),.groups='drop') %>% 
  pivot_wider(names_from = fy, values_from = value) %>% 
  arrange(desc(`2021年度`)) %>% 
  print()

agg_by_hp %>% 
  rename(病院=hpname) %>% 
  write_xlsx('output/近隣病院DPC実績_病院別.xlsx')

# fyで横に広げて提出用の表を作成
agg <- df %>% 
  arrange(fy,mdc6cd,opecd,hpname) %>% 
  pivot_wider(names_from = fy, values_from = value) %>% 
  select(mdc6cd,mdc6,opecd,ope,hpname,everything()) %>%
  arrange(mdc6cd,opecd,desc(`2021年度`)) %>% 
  print()

agg <- agg %>% 
  rename(疾患code=mdc6cd,疾患=mdc6,手術code=opecd,手術=ope,病院=hpname) %>%
  print()

# 出力
agg %>% 
  write_xlsx('output/近隣病院DPC実績.xlsx')
