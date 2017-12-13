tbl<-matrix(c(7,11,10,5,4,29,9,11,13,6,6,39),6,2,dimnames=list(distance=c("1-3","4-6","7-9","10-12","13-15","15+"),
                                                               Evacuation=c("yes","no")))

chisq.test(tbl)
fisher.test(tbl)

tbl<-matrix(c(7,11,10,9,29,9,11,13,12,39),5,2,dimnames=list(distance=c("1-3","4-6","7-9","10-15","15+"),
                                                               Evacuation=c("yes","no")))

chisq.test(t(tbl))
t(tbl)

birth.freq = as.array(c(66,63,64,48,64,74,70,59,54,51,45,42))
birth.prob <- as.array(rep(1,12))
for(i in 1:12){
  birth.prob[i] = 1/12
}
chisq.test(birth.freq,p=birth.prob)
