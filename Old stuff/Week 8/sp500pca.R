# PCA demo for energy stocks
rm(list=ls())
library(tidyr)
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 8")
sp500_px <- read.csv("sp500_data.csv", header=TRUE)
library(tidyverse)
names(sp500_px)
oil <- sp500_px[, c("CVX", "XOM")]
names(oil)
dim(oil)
pca <- princomp(oil)
names(pca)
pca$loadings
loadings <- pca$loadings
ggplot(data = oil, aes(x=CVX, y=XOM)) +
  geom_point(alpha = 0.3) +
  stat_ellipse(type="norm", level = 0.99) +
  geom_abline(intercept = 0, slope = loadings[2,1]/loadings[1,1]) +
  geom_abline(intercept = 0, slope = loadings[2,2]/loadings[1,2])
#
#
syms <- c( 'AAPL', 'MSFT', 'CSCO', 'INTC', 'CVX', 'XOM', 'SLB', 'COP','JPM', 'WFC', 'USB', 'AXP', 'WMT', 'TGT', 'HD', 'COST')
top_cons <- sp500_px[row.names(sp500_px)>='2011-01-01', syms]
sp_pca <- princomp(top_cons)
par(mar=c(6,3,0,0)+.1, las=2)
screeplot(sp_pca, main='')

## Loadings for stock data
loadings = sp_pca$loadings[,1:5]
loadings <- as.data.frame(loadings)
loadings$Symbol <- row.names(loadings)
loadings <- gather(loadings, "Component", "Weight", -Symbol)
head(loadings)
loadings$Color = loadings$Weight > 0
ggplot(loadings, aes(x=Symbol, y=Weight, fill=Color)) +
  geom_bar(stat='identity', position = "identity", width=.75) + 
  facet_grid(Component ~ ., scales='free_y') +
  guides(fill=FALSE)  +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5))