prop.test(x=c(47,135),n=c(1443,1781),alternative ="two.sided",correct=F)
prop.test(x=c(18,5),n=c(171+18,99+5),correct=F)
fisher.test(matrix(c(18,171,5,99),ncol=2))

prop.test(x=c(18+171,5+99),n=c(171+18+10845,99+5+10993),correct=F)
