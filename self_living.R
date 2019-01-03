path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',')
info$response = c(1:68323)
colnames(info)
agg_self_living = info[ info$n3_self_living > 0, c(68:73,282)] 
#只選self_living>0的row ; 68:73=只選self_living相關的風險
basic_info = info[,c(270:272,282:287)] #只選填答者基本資料
colnames(agg_self_living)
agg_self_living = agg_self_living %>% gather(key, value, -response) %>% arrange(response)
agg_self_living = filter(agg_self_living,agg_self_living$value == 1) %>% subset(,select = -value)
names(agg_self_living)[names(agg_self_living) == "key"] <- "answered_field"
agg_self_living = merge.data.frame(agg_self_living,basic_info,by='response')
