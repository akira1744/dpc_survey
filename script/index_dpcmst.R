# scriptフォルダの.Rファイル一覧を取得
# list.files('script',pattern = '04',full.names = T,recursive = T) %>% dput() 

source("script/040_make_dpc_mst_by_year.R",echo = T)

source("script/042_integrate_dpcmst.R",echo = T)

source("script/044_make_dpcmst_mdc2_mdc6.R",echo = T)

source("script/045_make_dpcmst_icd.R",echo = T)

source("script/046_make_dpcmst_opecd.R",echo = T)

source("script/047_make_dpcmst_opecd_kcode.R",echo = T)

source("script/048_confilm_dpcmst.R",echo = T)
