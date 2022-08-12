# load packages
library(fplscrapR)
library(dplyr)
library(doParallel)
library(data.table)
library(reshape2)
library(ggplot2)
library(magrittr)
library(worldfootballR)
library(corrplot)

registerDoParallel(cores=8)

