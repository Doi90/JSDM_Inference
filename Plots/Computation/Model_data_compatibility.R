library(grid)
library(gridExtra)

data <- read.table("Compatibility.csv", header = TRUE, sep = ",")
rownames(data) <- data[,1]
data <- data[,-1]
class(data)
grid.table(data)


library(xtable)
data <- read.table("Compatibility.csv", header = TRUE, sep = ",")
rownames(data) <- data[,1]
data <- data[,-1]
print.xtable(xtable(data), type = "latex")
?xtable