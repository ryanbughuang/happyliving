path = '/Users/ryanhuang/Desktop/107-1/business statistics/happyliving/happylivingdata_allvalid.csv'
info = read.table(path, header = TRUE, sep = ',')
info$response = c(1:68323)
colnames(info)

self_living = info[ info$n3_self_living > 0, c(16,23:28,270:272,282:287)]
View(self_living)
basic_info = info[,269:279] #只選填答者基本資料
#apply(basic_info[,c(4:8)],1,'sum') #living status detail是複選^^
basic_info = subset(basic_info, select = c(1:3,11)) #只選性別、年齡、住況、住地


