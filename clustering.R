library(flacco)
library(data.table)
library(ggplot2)
library(doParallel)
library(xtable)

setwd('~/Downloads/Bas')

dt_NN <- fread('data.csv')
dt_BBOB <- fread('all_flacco_bbob.csv')
colnames(dt_BBOB) <- sapply(colnames(dt_BBOB), . %>% gsub('ela_(meta.)?', '', .))

dt_NN_ <- dcast(dt_NN[rep <= 10], data.set + rep ~ feature)[
  , f := paste0(data.set, rep)
][, -c('data.set', 'rep')]
dt_NN_ <- as.data.frame(dt_NN_[, -c('f')]) %>% 
  set_rownames(dt_NN_$f) %>% 
  scale

hc <- hclust(dist(dt_NN_), method = 'complete')

cairo_pdf('hclust_NN.pdf', width = 9, height = 9)
plot(hc, xlab = '', cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, main = NULL)
rect.hclust(hc, k = 3, border = 2:5)
dev.off()

dt_NN <- dt_NN[, median(value), by = .(data.set, feature)] %>% 
  dcast(data.set ~ feature, value.var = 'V1')
dt_NN <- as.data.frame(dt_NN[, -c('data.set')]) %>% 
  set_rownames(dt_NN$data.set)

col <- c('f', colnames(dt_NN))
dt_BBOB <- dt_BBOB[, ..col]

dt_BBOB <- melt(dt_BBOB, id.var = 'f', variable.name = 'feature')
dt_BBOB <- dt_BBOB[, median(value), by = .(f, feature)] %>%
  dcast(f ~ feature, value.var = 'V1')

dt_BBOB <- as.data.frame(dt_BBOB[, -c('f')]) %>% 
  set_rownames(paste0('f', dt_BBOB$f))

dt <- rbind(dt_NN, dt_BBOB) %>% scale
hc <- hclust(dist(dt), method = 'ward.D2')

cairo_pdf('hclust_all.pdf', width = 9, height = 9)
plot(hc, xlab = '', cex = 1.5, cex.axis = 1.5, cex.lab = 1.5, main = NULL)
rect.hclust(hc, k = 4, border = 2:4)
dev.off()
plot(hc)