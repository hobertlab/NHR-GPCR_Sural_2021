tot.gene <- 18797 ## Total no. of genes in analysis

sample.A <- 267 ## No. of genes in sample A (used as reference)

not.sample.A <- tot.gene - sample.A ## No. of genes not in sample A

sample.B <- 432 ## No. of genes in sample B (used as test)

overlap <- 30 ## No. of genes in both samples A & B

hyper.p.exact <- dhyper(overlap, sample.A, not.sample.A, sample.B)
## calculates hypergeometric p value for obtaining exactly 'overlap' number of genes in
## both samples A & B

hyper.p.exact

hyper.p.cumul.more <- sum(dhyper(overlap:sample.B, sample.A, not.sample.A, sample.B))
## calculates hypergeometric p value for obtaining at least 'overlap' number of genes in 
## both samples A & B

hyper.p.cumul.more

hyper.p.cumul.less <- sum(dhyper(0:overlap, sample.A, not.sample.A, sample.B))
## calculates hypergeometric p value for obtaining at most 'overlap' number of genes in 
## both samples A & B

hyper.p.cumul.less

rm(list = ls())
