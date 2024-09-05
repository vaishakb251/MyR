#Load the built-in Iris dataset
data(iris)

# View the first few rows of the dataset
head(iris)

# Summary statistics for the dataset
summary(iris)

# Standard deviation for numeric columns
sapply(iris[, 1:4], sd)

# Scatter plot: Sepal.Length vs Sepal.Width, colored by species
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, 
     main="Sepal Length vs Sepal Width", xlab="Sepal Length", ylab="Sepal Width")

# Boxplot for Sepal.Length by species
boxplot(Sepal.Length ~ Species, data=iris, main="Sepal Length by Species",
        xlab="Species", ylab="Sepal Length")

# Perform ANOVA to see if Sepal.Length differs between species
anova_result <- aov(Sepal.Length ~ Species, data=iris)
summary(anova_result)

# Correlation matrix of the numeric variables
cor(iris[, 1:4])