data(iris)
head(iris, 3)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 
print(ir.pca)
plot(ir.pca, type = "l")
summary(ir.pca)
# Predict PCs
predict(ir.pca, 
        newdata=tail(log.ir, 2))


