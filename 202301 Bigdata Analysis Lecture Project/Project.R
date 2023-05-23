#교차 검증을 위한 분석

source("package_library.R")
library(openxlsx)
library(readxl)

data <- read.xlsx("final_data.xlsx")

colnames(data)

data <- data[2:length(colnames(data))]

non_numeric_columns <- names(data)[!sapply(data, is.numeric)]
non_numeric_columns

pca_region <- prcomp(data, rank = 5)

pca_region$rotation

pca_region$x

pca_region2 <- princomp(data)

pca_region2$loadings

data_for_FA <- as.data.frame(read_excel("data_for_FA.xlsx"))
data_for_FA <- data_for_FA[2:length(colnames(data_for_FA))]

pca_region3 <- prcomp(data_for_FA, rank=5)
pca_region3$scores
pca_region3$rotation

pca_region4 <- fa(data_for_FA)

install.packages("psych")
library(psych)

fa <- fa(data_for_FA, nfactors = 5, rotate = "varimax", fm = "pa")

print(fa$loadings)

data_final <- read.xlsx("data_for_FA_non65.xlsx")[,2:ncol(read.xlsx("data_for_FA_non65.xlsx"))]

head(data_final)

fa2 <- factanal(data_final, 2, "varimax")

fa2$loadings
