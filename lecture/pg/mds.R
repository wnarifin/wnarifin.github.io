# ========================
# Multidimensional scaling
# Wan Nor Arifin
# ========================

# data: fruit & motor vehicle
# ---------------------------
matrix = read.csv("matrix.csv")  # similarity matrix
matrix = 10 - as.matrix(matrix)  # turn into dissimilarity matrix

# analysis
matrix = as.dist(matrix)  # set as the dissimilarity ratings as the distances
# 1 dimension
loc = cmdscale(matrix, k = 1)
x = loc[, 1]
stripchart(x ~ names(x), pch = 19, col = "blue", frame = F)
stripchart(x, pch = "")
text(x, 1, names(x), srt = 90, col = "blue")
# 2 dimensions
loc = cmdscale(matrix, k = 2)
loc  # locations in space
x = loc[, 1]
y = loc[, 2]
plot(x, y, xlab = "", ylab = "", ylim = c(-3, 3), pch = 19, col = "blue")
text(x, y + 0.2, rownames(loc), col = "blue")
# details
loc = cmdscale(matrix, k = 2, eig = T)
round(loc$eig, digit = 3)
plot(loc$eig, type = "o")
round(loc$GOF, digit = 3)
# 3 dimensions
loc = cmdscale(matrix, k = 3)
x = loc[, 1]
y = loc[, 2]
z = loc[, 3]
library(scatterplot3d)
s3d = scatterplot3d(x, y, z, type = "h", pch = 19, color = "blue")
text(s3d$xyz.convert(x, y, z), labels = rownames(loc), pos = 3, col = "blue") 

# data: iris
# ----------
flower = iris
matrix = dist(iris[1:4])  # turn into Euclidean distances from the 4 variables
# 1 dimension
loc = cmdscale(matrix, k = 1)
x = loc[, 1]
x = data.frame(x, species = flower$Species)
stripchart(x ~ species, data = x, col = 1:3, frame = F)
# 2 dimensions
loc = cmdscale(matrix, k = 2)
x = loc[, 1]
y = loc[, 2]
plot(x, y, xlab = "", ylab = "", col = flower$Species, pch = 19)
legend(x = "top", legend = levels(flower$Species), col = 1:nlevels(flower$Species), pch = 19)
# details
loc = cmdscale(matrix, k = 2, eig = T)
round(loc$eig, digit = 3)
plot(loc$eig, type = "o")
round(loc$GOF, digit = 3)
# 3 dimensions
loc = cmdscale(matrix, k = 3)
x = loc[, 1]
y = loc[, 2]
z = loc[, 3]
loc1 = data.frame(x, y, z, spe = as.factor(flower$Species))
as.numeric(loc1$spe)
library(scatterplot3d)
s3d = scatterplot3d(x, z, y, type = "h", color = as.numeric(loc1$spe), pch = 19)
legend(x = "top", legend = levels(flower$Species), col = 1:nlevels(flower$Species), pch = 19)
text(s3d$xyz.convert(x, z, y), labels = loc1$spe, pos = 3, col = as.numeric(loc1$spe))
