---
title: "S&DS 363 Final Project"
author: "Michael and George"
date: "April 2020"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
rm(list = ls()) 
knitr::opts_chunk$set(echo = TRUE)
```

# Introduction 

The World Happiness Report is a leading survey in assessing the state of global happiness. There has been a report ever since the inception of the survey in 2012. In recent years, the report has gained global recognition as governments, organizations and civil societies increasingly use happiness indicators to inform their policy-making decisions. Leading experts across various fields such as economics, psychology, survey analysis, national statistics, health and public policy describe how measurements of well-being can be used effectively to assess the progress of nations. The reports review the state of happiness in the world today and show how the new science of happiness explains personal and national variations in happiness. 

# Design and Primary Questions 

As there are now more than 7 reports that have been released, we chose to focus our analysis on the one published in 2015, as it included the most variables than any other report. 

Given that dataset we will try to answer three different questions in this analysis using three different multivariate statistical techniques.

Firstly, along what lines do countries tend to be different when we examine factors that may contribute to their happiness score? With the help of Principal Component Analysis (PCA) we will be able to reduce all of the variables found in the model to a lower dimensional space that will allow us to make concrete comparisons. 

Secondly, which countries are like other countries when it comes to considering the factors that affect their happiness scores? We believe that this grouping of countries will be best done through Cluster Analysis. 

And finally, are there any latent variables that can best explain the differences between happiness scores among countries? In order to achieve this we will be using Factor Analysis which can tell us how many latent factors can explain the data and from there we will be able to deduce what exactly these factors are. 

# Data

The content of the report consists of happiness scores for each country along with all the explanatory variables that may affect the value of that score. The scores of the report use data from the Gallup World Poll and are based on answers to the main life evaluation question asked in the poll. This question, known as the Cantril ladder, asks respondents to think of a ladder with the best possible life for them being a 10 and the worst possible life being a 0 and to rate their own current lives on that scale. The scores are from nationally representative samples for the years 2013-2014 and use the Gallup weights to make the estimates representative. 

The columns following the happiness score estimate the extent to which each of six factors – economic production, social support, life expectancy, freedom, absence of corruption, and generosity – contribute to making life evaluations higher in each country than they are in Dystopia, a hypothetical country that has values equal to the world’s lowest national averages for each of the six factors. They have no impact on the total score reported for each country, but they do explain why some countries rank higher than others.

Dystopia is an imaginary country that has the world’s unhappiest people. The purpose in establishing Dystopia is to have a benchmark against which all countries can be favorably compared (no country performs more poorly than Dystopia) in terms of each of the six key variables, thus allowing each sub-bar to be of positive width. The lowest scores observed for the six key variables, therefore, characterize Dystopia. 

More specifically for each of the variables: 

-**Country**: Name of the country. Factor Variable. 

-**Region**: Region that a country belongs to. Factor Variable. 

-**Happiness Rank**: Rank of the country based on the Happiness Score. Factor Variable. 

-**Happiness Score**: A metric measured in 2015 by asking the sampled people the question: "How would you rate your happiness on a scale of 0 to 10 where 10 is the happiest." Continuous Variable.  

-**Standard Error**: The standard error of the happiness score. Continuous Variable. 

-**Economy (GDP per Capita)**: The extent to which GDP contributes to the calculation of the Happiness Score. Continuous Variable. 

-**Family**: The extent to which Family contributes to the calculation of the Happiness Score. Continuous Variable. 

-**Health (Life Expectancy)**: The extent to which Life expectancy contributed to the calculation of the Happiness Score. Continuous Variable.

-**Freedom**: The extent to which Freedom contributed to the calculation of the Happiness Score. Continuous Variable. 

-**Trust (Government Corruption)**: The extent to which Perception of Corruption contributes to Happiness Score. Continuous Variable. 

-**Generosity**: The extent to which Generosity contributed to the calculation of the Happiness Score. Contunuous Variable. 

-**Dystopia Residual**: The extent to which Dystopia Residual contributed to the calculation of the Happiness Score. The residuals, or unexplained components, differ for each country, reflecting the extent to which the six variables either over- or under-explain average 2014-2015 life evaluations. These residuals have an average value of approximately zero over the whole set of countries. Continuous Variable. 

By adding all these factors listed above we get the happiness score for each of the countries. 

```{r, echo = FALSE, include=FALSE}
library(corrplot)
library(PerformanceAnalytics)
library(aplpack)
library(fpc)
library(cluster)
library(ape)

data <- read.csv("2015.csv")
newdata <- data[-c(1,2,3,5)]
```

# Multivariate Analysis 

## Principal Components Analysis
```{r, echo = FALSE}
source("http://www.reuningscherer.net/STAT660/R/CSQPlot.r.txt")
CSQPlot(newdata,label="Happiness Data")
```

Looking at our chi-square quantile plot, we observe that our data is not perfectly normal as some of the data points are outside of the 95% confidence limits. Despite this, we feel that our data is still good enough for us to perform a PCA analysis on it.

```{r}
plot(newdata, pch=19, cex=.7, col='red', main="Matrix plot of Happiness data")
chart.Correlation(newdata, histogram=TRUE, pch=19)
```

While checking for linearity in our data, we make a series of bivariate scatterplots to check for any non-linear relationships. Looking at our scatterplots, we can see that all of our variables have a linearly correlation. Because of the lack of any non-linear relationships, we can proceed with our PCA analysis.

```{r, echo = FALSE}
corrplot.mixed(cor(newdata), lower.col="black", upper = "ellipse", tl.col = "black", number.cex=.7, order = "hclust",
               tl.pos = "lt", tl.cex=.7, main="Correlations for Happiness Data")
```

Lastly, we decide to look into the correlation between our variables by creating a correlation table. From this, we determine that most of our variables are either strongly or moderately correlated with each other. Ideally, for a PCA analysis, we would want most of our variables to be moderately correlated with each other and our findings support this. Therefore, we can proceed with our PCA analysis. 

```{r, echo = FALSE}
pc1 <- princomp(newdata, cor = TRUE)
print(summary(pc1), digits = 2, loadings = pc1$loadings, cutoff=0)
round(pc1$sdev^2,2)
```

Because we want to explain 80% of the variability in our data, we observe that we should keep 4 components, which explains about 86.6% of the total variance. If we use an eigenvalue > 1 rule, we see that we should keep 4 components since the fourth component has an eigenvalue that is less than 1. 

```{r, echo = FALSE}
screeplot(pc1,type="lines",col="red",lwd=2,pch=19,cex=1.2,main="Scree Plot of Happiness Data")
```

The scree plot indeed confirms our belief from earlier that the first 2-3 components explains most of the variability in our data. Between the 2nd and 3rd component, we see an elbow in our scree plot, which indicates that we should keep 2 components.

```{r, echo = FALSE}
source("http://www.reuningscherer.net/STAT660/R/parallel.r.txt")
parallelplot(pc1)
```

Finally, we decide to conduct a parallel analysis using the Longman and Allen method to determine the number of components we should keep. Looking at our parallel plot, we observe that only our first 2 eigenvalues are above the parallel threshold. Overall, we decide to keep 2 principal components from our analysis because the scree plot and parallel analysis are generally considered to be more reliable and desirable in determining the number of components to keep. 

Our loading coefficients table shows that component 1 has a moderate correlation for all of the variables involved. We can then say that component 1 likely represents overall happiness levels in a country because of its consistent correlation with all of the variables. Looking at component 2, we see a strong negative correlation value for the variables 'Generosity', 'Freedom' and 'Trust in Government'. We can then say that component 2 is picking up on variation relating to democracy and government trust.  

```{r, echo = FALSE}
source("http://reuningscherer.net/stat660/r/ciscoreplot.R.txt")

ciscoreplot(pc1,c(1,2),data[,1])
biplot(pc1,choices=c(1,2),pc.biplot=T)
```

Looking at our PC score plot, we see that most of the data points are within the 95% confidence interval ellipse. However, we do have a few outliers, particularly in the direction of the 2nd component. Recall that the 2nd component is related to democracy and government trust. Interestingly enough, we observe that 3 out of 4 of these countries outside the ellipse have, in the past decades, experienced some form of violence or genocide that largely impacted the country. This could explain why the happiness score for 'Syria', 'Rwanda', and 'Myanmar' is largely dependent on democracy and government trust. 

From the biplot, we can observe that all of the variables are pointing in the positive direction for component 1, which means that all of them are positively affecting happiness. However, for component 2, we see a divergence between variables related to life expectancy/wealth and variables relating to generosity and government trust. This divergence means that countries with higher GDP per capita and life expectancy depend less on generosity and government trust when it comes to their happiness levels. The same can be said vice versa for countries that largely depend on generosity and government trust for their happiness. 

## Cluster Analysis

Since our data is continuous, we decided to do a cluster analysis using two methods to group the data. Our first method uses the euclidean distance and ward.D as our agglomeration method. In our second method, we instead use the manhattan distance metric and centroid as our agglomeration method.

```{r}
newdata <-  scale(na.omit(newdata))
```

To start off, we decide to scale our data to make sure that no variable is scale dependent, meaning that some variables could have more influence over others due to their units.

```{r, echo = FALSE}
dist1 <- dist(newdata, method="euclidean")
clust1 <- hclust(dist1, method = "ward.D")

clust1$labels <- as.character(data[,1])
plot(clust1, xlab="",ylab="Distance",main="Clustering for Countries")
rect.hclust(clust1,k=5)
```

Next, we decide to cluster in 5 groups using the euclidean distance and ward.D agglomeration method. We can see that, although it is difficult to identify each individual country, our dendrogram tree is broken down pretty evenly in 5 separate clusters of countries.

```{r, echo = FALSE}
dist2 <- dist(newdata, method="manhattan")
clust2 <- hclust(dist2, method = "centroid")
plot(clust2,labels= rownames(data), cex=0.6, xlab="",ylab="Distance",main="Clustering of Countries")
rect.hclust(clust2,k=5)
```

On the other hand, when using the manhattan distance and centroid agglomeration method, we see very different results. We encounter an uneven clustering of countries where a large majority of countries are clustered into one group leaving the other 4 cluster groups with only 1 or 2 countries. In addition, our dendrogram tree branches are all broken down into many separate sub-branches in a uneven and complex way.

```{r, echo=FALSE,results='hide',fig.keep='all'}
source("http://reuningscherer.net/stat660/R/HClusEval.R.txt")
#Call the function
hclus_eval(newdata, dist_m = 'euclidean', clus_m = 'ward.D', plot_op = T)
```

In the above plot, we use the euclidean distance and ward.D agglomeration method to plot the R^2, which is a measure of how much variability in the data our model explains. We see that R^2 slows to significantly slow down in its increase after about 8-10 cluster groups. The Semi-Partial R Squared, which measures the relative change in within-clusters Sum of Squares, seems to as well become near zero after 8-9 groups with significantly smaller marginal gains by the addition of one more group after that point.

```{r, echo = FALSE}
#kdata is just normalized input dataset
kdata <- newdata
n.lev <- 15  #set max value for number of clusters k

# Calculate the within groups sum of squared error (SSE) for the number of cluster solutions selected by the user
wss <- rnorm(10)
while (prod(wss==sort(wss,decreasing=T))==0) {
  wss <- (nrow(kdata)-1)*sum(apply(kdata,2,var))
  for (i in 2:n.lev) wss[i] <- sum(kmeans(kdata, centers=i)$withinss)}

# Calculate the within groups SSE for 250 randomized data sets (based on the original input data)
k.rand <- function(x){
  km.rand <- matrix(sample(x),dim(x)[1],dim(x)[2])
  rand.wss <- as.matrix(dim(x)[1]-1)*sum(apply(km.rand,2,var))
  for (i in 2:n.lev) rand.wss[i] <- sum(kmeans(km.rand, centers=i)$withinss)
  rand.wss <- as.matrix(rand.wss)
  return(rand.wss)
}

rand.mat <- matrix(0,n.lev,250)

k.1 <- function(x) { 
  for (i in 1:250) {
    r.mat <- as.matrix(suppressWarnings(k.rand(kdata)))
    rand.mat[,i] <- r.mat}
  return(rand.mat)
}

# Same function as above for data with < 3 column variables
k.2.rand <- function(x){
  rand.mat <- matrix(0,n.lev,250)
  km.rand <- matrix(sample(x),dim(x)[1],dim(x)[2])
  rand.wss <- as.matrix(dim(x)[1]-1)*sum(apply(km.rand,2,var))
  for (i in 2:n.lev) rand.wss[i] <- sum(kmeans(km.rand, centers=i)$withinss)
  rand.wss <- as.matrix(rand.wss)
  return(rand.wss)
}

k.2 <- function(x){
  for (i in 1:250) {
    r.1 <- k.2.rand(kdata)
    rand.mat[,i] <- r.1}
  return(rand.mat)
}

# Determine if the data data table has > or < 3 variables and call appropriate function above
if (dim(kdata)[2] == 2) { rand.mat <- k.2(kdata) } else { rand.mat <- k.1(kdata) }

# Plot within groups SSE against all tested cluster solutions for actual and randomized data - 1st: Log scale, 2nd: Normal scale

xrange <- range(1:n.lev)
yrange <- range(log(rand.mat),log(wss))
plot(xrange,yrange, type='n', xlab='Cluster Solution', ylab='Log of Within Group SSE', main='Cluster Solutions against Log of SSE')
for (i in 1:250) lines(log(rand.mat[,i]),type='l',col='red')
lines(log(wss), type="b", col='blue')
legend('topright',c('Actual Data', '250 Random Runs'), col=c('blue', 'red'), lty=1)

yrange <- range(rand.mat,wss)
plot(xrange,yrange, type='n', xlab="Cluster Solution", ylab="Within Groups SSE", main="Cluster Solutions against SSE")
for (i in 1:250) lines(rand.mat[,i],type='l',col='red')
lines(1:n.lev, wss, type="b", col='blue')
legend('topright',c('Actual Data', '250 Random Runs'), col=c('blue', 'red'), lty=1)
```

In the graph above, which represents the log of within group sum of squares versus the number groups, we see two major elbows. One at groups 3-4 and another one at groups 7-8. This is in agreement with our conclusion from the R^2 plot above, where after looking at various clustering metrics, we concluded that number of groups should be around 8-10. Using both of these metrics, we ultimately decide to go with 8 clustering groups for our cluster analysis. 

```{r, echo = FALSE}
dist1 <- dist(newdata, method="euclidean")
clust1 <- hclust(dist1, method = "ward.D")

clust1$labels <- as.character(data[,1])
plot(clust1, xlab="",ylab="Distance",main="Clustering for Countries")
rect.hclust(clust1,k=8)
```

```{r, echo=FALSE}
cuts <- cutree(clust1,k=8)

rownames(data) <- data[,1]

#get list of countries in each group ??? note that group numbering is arbitrary ??? there is no ???ordering??? of groups
for (i in 1:8){
  print(paste("Countries in Cluster ",i))
  print(rownames(data)[cuts==i])
  print (" ")
}
```

Looking at our cluster groups, we see that a majority of major Western/European countries are grouped together in cluster 1, which is no surprise given that these countries are considered to be highly developed. We also observe that countries that are generally regarded as undeveloped are grouped together in cluster 8, with a large majority of them from the African continent. Interestingly enough, in cluster 7, we see a group of countries that have all had wars/genocides in the past few decades or recently. This upholds our findings from earlier where in our PCA analysis, we observed that countries, which were recently ravaged by violent conflict, had a higher dependency on democracy and government trust for their happiness. We also observe that some countries that are located in the same region (Ex. East Europe or Middle East) are often clustered together. Overall, we conclude that these cluster grouping of countries largely follows what we would expect to see from a cluster analysis that relies on variables relating to happiness, health, freedom, corruption, and GDP per capita.

## Factor Analysis 

We start our factor analysis by first looking at the correlation matrix of all the indicator variables in the data used to calculate the happines score of a country. As such, we remove columns refering to the happiness score directly as well as any categorical variables that have to do with the name of a specific country as well as its geographic location. 

```{r, echo = FALSE}
data <- na.omit(data)
CorMatrix <- cor(data[,-c(1:5)])
round(CorMatrix, 2)
```

Most of the indicator variables have moderate correlation with each other (less than an absolute value of 60%), but there are some strong exceptions. For example, we see that the amount that family contributes in overall happiness has a relativelt strong correlation of 0.65 with the degree that the economy contributes to happiness. Even stronger correlation is found between the level that health contributes to happiness and the level that is contributed by the economy. This is the highest between any two indicators and it is found to be equal to 0.82. Finally, we can say that we see some degree of moderate correlation between family and health (0.53) as well as freedom and family (0.44).  

We continue by performing a Kaiser-Meyer-Olkin (KMO) measure of adequacy test, in order to see if our data are actually appropriate to be used in a factor analysis scheme. 

```{r, echo = FALSE}
library(rela)
KMO <- paf(as.matrix(data[,-c(1:5)]))
summary(KMO)
```

Performing a KMO test on our data yields a result of .667, which would put it near the "middling" range for whether factor analysis is appropriate for our data. Even though a value of .667 is lower than what we would like, we can still perform factor analysis on our data as its KMO value doesn't fall below .50, which is the unacceptable range.

Now, in order to determine the number of latent factors in our model we will be using the results from the principle components analysis that we performed earlier in our report. Specifically, we will be using the same scree plot that we produced earlier in order to determine the number of factors. 

```{r, echo = FALSE}
pc1 <- princomp(data[,-c(1:5)], cor = TRUE)
names(pc1)
print(summary(pc1), digits = 2, loadings = pc1$loadings, cutoff = 0)

screeplot(pc1, type = "lines", col = "red", lwd = 2, pch = 19, cex = 1.2, main = "Scree Plot")

source("http://www.reuningscherer.net/STAT660/R/parallel.r.txt")
parallelplot(pc1)
```

Looking at the scree plot, we observe an elbow at 2 factors. Meanwhile on the parallel plot, we see the red eigenvalue line dips below the Longman and Allen method lines between 2 and 3 factors. Ultimately, we decide to use 2 latent factors for our factor analysis.

As we end up having only 2 latent factors in our factor analysis the extraction method we will be using when using orthogonal models will be that of maximum likelihood. 

```{r, echo = FALSE}
#Maximum likelihood
fact1 <- factanal(data[,-c(1:5)],factors=2,rotation="varimax")
fact1

#get reproduced correlation matrix
repro1 <- fact1$loadings%*%t(fact1$loadings)
#residual correlation matrix
resid1 <- fact1$cor-repro1
round(resid1,2)

#get root-mean squared residuals
len <- length(resid1[upper.tri(resid1)])
RMSR1 <- sqrt(sum(resid1[upper.tri(resid1)]^2)/len)
RMSR1

#get proportion of residuals greater than 0.05 in absolute value
sum(rep(1,len)[abs(resid1[upper.tri(resid1)])>0.05])/len
```

We calculate a Root Mean Square Residual of 0.050934. When we instead look at the proportion of residuals greater than .05, we find a value equal to 0.28571, which is relatively good number.

Next, we will be testing the hypothesis that there are no latent factors in our data. 

```{r, echo = FALSE}
fact1 <- factanal(data[,-c(1:5)],factors=2,rotation="varimax")
fact1
```

Looking at our p-value of 0.0189, which is less than .05, we can reject the null hypothesis that there are no latent factors in our data. From what we observed in previous steps, we decide to go with 2 latent factors in our factor analysis. 

```{r, echo = FALSE}
plot(fact1$loadings, pch=18, col='red')
abline(h=0)
abline(v=0)
text(fact1$loadings, labels=names(data[,-c(1:5)]),cex=0.8)
```

In our loading plot of two factors, we observe that variables such as Economy and Health have particularly high values in factor 1. As both of them are related to a degree with the quality of the instutions in a country, we can say that indicators that are high in value in factor 1, relate to the infrastructure and instutions of a particular country.  Meanwhile indicators such as Freedom, Trust and Government Corruption and Generosity have high values in the second factor. Hence, we can say that this factor relates to things such as personal freedoms and quality of interactions among people in a country. 

Finally, we can say that the two latent factors that we found relate very closely to the PCA components that we found in the previous part, meaning that the direction towards which the variability of the data increases resembles very closely the latent factorts that describe them. 

# Conclusions and Discussion 

## Principal Components Analysis 

In our Principal Components Analysis, after using various methods, we decided to keep 2 principal components in our analysis. Using a biplot, our main finding was that countries that are largely dependent on variables like GDP per capita and life expectancy for their happiness were less reliant on variables like generosity and freedom. The same can be said vice versa. Interestingly in our PC score plot, we found that countries that have experienced recent violent events like Syria, Rwanda, and Myanmar were negative outliers towards the 2nd component. This means that these countries are heavily reliant on freedom and generosity for their happiness levels. 

## Cluster Analysis 

Using a euclidean distance and ward.D agglomeration method, we grouped countries into 8 clusters. We largely saw what we expected to see from a cluster analysis where countries were grouped by variables related to happiness, health, freedom, corruption, and GDP per capita. For example, we had a cluster of Western countries that many would consider as highly developed countries. Additionally, we also had a cluster of countries that would be considered as undeveloped and low-income, largely in the African continent. Suprisingly, we also observed a cluster that consisted of countries that have experienced war and genocide in the past decades, supporting our findings earlier in our PCA analysis.

## Factor Analysis 

Factor analysis gave us some interesting insights that would not be possible with the other two multivariate methods. Through PCA we showed that there are indeed two factors towards which the variability in our data increases and along these axes our variables differ. Here we built on top of that and were able to get an insight on what the latent variables are. These were able to describe the happiness score of each country in a lower dimensional space. Public infrastructure and personal freedoms are two things that someone could easily associate with happiness of people in a country. Hence, it would be interesting to see future happiness reports expand upon the variables that they already include or group them into fewer categories as we showed. 

# References 

The introductory part of this project as well as the descriptions for the variables were adapted from this page, which features and in-depth overview of the contents of the dataset: https://www.kaggle.com/unsdsn/world-happiness/data#
