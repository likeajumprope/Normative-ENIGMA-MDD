#load tables with z-scores
library(ggplot2)
library(reshape2)
library(emmeans)

load("~/Dropbox/ENIGMA/test_tables.RData")

BMI <-
  read.csv("~/Dropbox/ENIGMA/raw_data/ENIGMA_MDD_BMI_Lifespan_11082021.csv")
#CTQ_old<-read.csv("~/Dropbox/ENIGMA/ENIGMA_MDD_CTQ_Lifespan_11082021.csv")
CTQ <- read.csv("~/Dropbox/ENIGMA/raw_data/ENIGMA_CTQ_16Dec2021.csv")

#load("out_variables.RData")

#MDD
MDD_positive <- (as.matrix((MDD_zscore[, c(2:36)] > 1.96)) * 1)
MDD_negative <- (as.matrix((MDD_zscore[, c(2:36)] < (-1.96))) * -1)


#cont
cont_positive <- (as.matrix((cont_zscore[, c(2:36)] > 1.96)) * 1)
cont_negative <- (as.matrix((cont_zscore[, c(2:36)] < (-1.96))) * -1)

# percentage of indivdiuals with  extreme deviations

percOfExtremesTable <- list()

percOfExtremesTable[[1]] <- colSums(MDD_positive) / nrow(MDD_positive)
percOfExtremesTable[[2]] <- colSums(MDD_negative) / nrow(MDD_negative) * (-1)
percOfExtremesTable[[3]] <- colSums(cont_positive) / nrow(cont_positive)
percOfExtremesTable[[4]] <- colSums(cont_negative) / nrow(cont_negative) * (-1)
percOfExtremesTable[[5]]<- colnames(MDD_positive)

percOfExtremesTable <-
  data.frame(matrix(
    unlist(percOfExtremesTable),
    nrow = 35,
    byrow = FALSE
  ))
colnames(percOfExtremesTable) <-
  c("MDD_positive",
    "MDD_negative",
    "cont_positive",
    "cont_negtaive",
    "region")

write.csv(percOfExtremesTable, file = "41_percOfExtremesTable.csv")
#
# #plots
# setwd("~/Dropbox/ENIGMA/hist_ecdf")
# for (i in 2:36){
# png(file=paste0(colnames(MDD_zscore[i]), "_hist.png"))
# p1 <- hist(MDD_zscore[,i], breaks=40, xlab=NULL)                     # centered at 4
# p2 <- hist(cont_zscore[,i],  breaks=40,  xlab=NULL)                     # centered at 6
# plot( p1, col='skyblue',border=F, xlab=NULL, main=NULL)  # first histogram
# plot( p2, add=T, col=scales::alpha('red',.5), border=F, xlab=NULL, main=NULL)
# title(colnames(MDD_zscore[i]))
# abline(v=1.96)
# abline(v=-1.96)
# dev.off()
#
# png(file=paste0(colnames(MDD_zscore[i]), "_ecdf.png"))
# p1 <- ecdf(MDD_zscore[,i])                     # centered at 4
# p2 <- ecdf(cont_zscore[,i])
# plot(p1, main=NULL, col='skyblue')
# plot(p2, main=NULL, add=T, col=scales::alpha('red',.5))# centered at 6
# title(colnames(MDD_zscore[i]))
# dev.off()
# }

# chis square tests for testing whether the frequencies of extreme deviaitions between MDD and healthy controls are significant

#prepare chi square table
countOfExtremesTable_pos <- list()
countOfExtremesTable_neg <- list()
countOfExtremesTable_norm <- list()
countOfExtremesTable_pos[[1]] <- colSums(MDD_positive)
countOfExtremesTable_pos[[2]] <- colSums(cont_positive)
countOfExtremesTable_neg[[1]] <- colSums(MDD_negative) * (-1)
countOfExtremesTable_neg[[2]] <- colSums(cont_negative) * (-1)
countOfExtremesTable_norm[[1]] <-
  3645 - countOfExtremes_pos$MDD - countOfExtremes_neg$MDD
countOfExtremesTable_norm[[2]] <-
  2119 - countOfExtremes_pos$cont - countOfExtremes_neg$cont


countOfExtremes_pos <-
  as.data.frame(t(do.call(
    rbind.data.frame, countOfExtremesTable_pos
  )))
colnames(countOfExtremes_pos) <- c("MDD", "cont")
rownames(countOfExtremes_pos) <- colnames(cont_negative)

countOfExtremes_neg <-
  as.data.frame(t(do.call(
    rbind.data.frame, countOfExtremesTable_neg
  )))
colnames(countOfExtremes_neg) <- c("MDD", "cont")
rownames(countOfExtremes_neg) <- colnames(cont_negative)

countOfExtremes_norm <-
  as.data.frame(t(do.call(
    rbind.data.frame, countOfExtremesTable_norm
  )))
colnames(countOfExtremes_norm) <- c("MDD", "cont")
rownames(countOfExtremes_norm) <- colnames(cont_negative)

#chisquare test

#positive

p <- list()
chi_pos <- list()
for (i in 1:35) {
  tab <- rbind(countOfExtremes_pos[i, ], countOfExtremes_norm[i, ])
  print(tab)
  res <- chisq.test(tab)
  chi_pos[[i]] <- res$p.value
  print(res$p.value)
  p[i] <- res$p.value
}

p_pos <- do.call(rbind.data.frame, p)

p <- list()
chi_neg <- list()
for (i in 1:35) {
  tab <- rbind(countOfExtremes_neg[i, ], countOfExtremes_norm[i, ])
  print(tab)
  res <- chisq.test(tab)
  chi_neg[[i]] <- res$p.value
  print(res$p.value)
  p[i] <- res$p.value
}

p_neg <- do.call(rbind.data.frame, p)

pos <- do.call(rbind.data.frame, chi_pos)
neg <- do.call(rbind.data.frame, chi_neg)
chi_values <- cbind(colnames(cont_negative), pos, neg)
colnames(chi_values) <- c("regions", "positive-norm", "negative-norm")

tmp.fdr.p = p.adjust(p_neg[c(1:35), 1], method = "fdr")

write.csv(chi_values, file = "41_chi_values.csv")

# table with one or more extreme deviations

MDD_no_positive_deviation <- rowSums(MDD_positive) == 0
MDD_no_negative_deviation <- rowSums(MDD_negative) == 0

MDD_no_deviation <-
  MDD_no_positive_deviation & MDD_no_negative_deviation
sum(MDD_no_deviation)

positive_deviation <- rowSums(positive) != 0
negative_deviation <- rowSums(negative) != 0

only_negative_deviation <- negative_deviation &
  no_positive_deviation
sum(only_negative_deviation)

only_positive_deviation <- no_negative_deviation &
  positive_deviation
sum(only_positive_deviation)


both_deviations <- positive_deviation & negative_deviation
sum(both_deviations)