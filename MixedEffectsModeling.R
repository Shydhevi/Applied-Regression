#' ---
#' title: "Mixed Effects Models"
#' author: "Margo Bergman"
#' date: "March 27, 2020"
#' output: word_document
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' Example
#' 
#' Using a classic data set, the Grunfeld data set, we explore the R package plm. This is modified from Chapter 12 of the book "Panel Econometrics" edited by Mike Tsionas. 
#' 
#' This data set, based on a 10 firm data set from 1935 - 1954 relates investment levels (defined here in the economics sense of investment as infrastructure, plus maintenance and repairs) to the market value of the firms and their capital stock. 
#' 
## ----------------------------------------------------------------------------------
library(plm)
data("Grunfeld", package="plm")

#' 
#' We need to know whether the dataset is balanced. This is one where observations from all possible combinations of the cross-sectional dimension (i = 1, ., n) and the time dimension (t = 1, ., T) so that its total number of observations is N = n x T.  Those datasets that don't meet these requirements are unbalanced, and will not work with all tests and estimators in the plm package. To determine if we have a balanced dataset, we can explore it in the following way:
#' 
#' What are the names of the variables in the data set?
#' 
## ----------------------------------------------------------------------------------
names(Grunfeld)

#' 
#' Display the first six observations:
#' 
## ----------------------------------------------------------------------------------
head(Grunfeld)

#' 
#' What is the number of observations?
#' 
## ----------------------------------------------------------------------------------
nrow(Grunfeld)

#' 
#' What are the identifiers of the firms in the data set?
#' 
## ----------------------------------------------------------------------------------
unique(Grunfeld$firm)

#' 
#' What is the number of firms in the data set?
#' 
## ----------------------------------------------------------------------------------
length(unique(Grunfeld$firm))

#' 
#' Which years are included in the data set?
#' 
## ----------------------------------------------------------------------------------
unique(Grunfeld$year)

#' 
#' What is the number of years that are included in the data set?
#' 
## ----------------------------------------------------------------------------------
length(unique(Grunfeld$year))

#' 
#' How many duplicate firm-year combinations are in the data set?
#' 
## ----------------------------------------------------------------------------------
sum(duplicated(Grunfeld[ , c( "firm", "year" ) ] ) )

#' 
#' There are no duplicate firm-year combinations. Therefore, the number of unique firm-year combinations is equal to the number of observations (200).
#' 
#' How many observations are included in each of the 20 years?
#' 
## ----------------------------------------------------------------------------------
table( Grunfeld$year )

#' 
#' Illustrate this graphically:
#' 
## ----------------------------------------------------------------------------------
barplot( table( Grunfeld$year ) )

#'  We have values for all the years, so we do not have to drop any years of our dataset. 
#'  
#'  Let's look now at the data.
#' 
#' How did investment (unweighted average investment of the firms included in the data set) change over time?
#' 
## ----------------------------------------------------------------------------------
aggregate( inv ~ year, Grunfeld, mean )

#' 
#' How did the distribution of the investment change over time?
#' 
## ----------------------------------------------------------------------------------
    boxplot( inv ~ year, data = Grunfeld )

    lines( aggregate( inv ~ year, Grunfeld, mean )$inv, col = "blue", lwd = 2 )

#' 
#' The data has a wide spread, especially as it gets towards the end of the dataset. The means within each year are sometimes even spread and other times very uneven, with a strong rightward, or positive, skew. This implies that several of the years, some of the firms had much higher levels of investment than others.
#' 
#' 
## ----------------------------------------------------------------------------------
boxplot( log( capital ) ~ year, data = Grunfeld )

#' 
#' In this graph, we look at the capital investment. While it is even in 1935, it becomes skewed leftward, or negative, from 1936 - 1946, where it evens back out. This indicates a much lower level of capital investment for some firms than other firms. 
#' 
#' Now we turn to the model we will be estimating:
#' 
#' $$
#' inv_{it}=\beta_{0} + \beta_{1} value_{it} + \beta_{2} capital_{it} + u_{it} $$
#' 
#' $inv_{it}$ is the firm's gross investment
#' $value_{it}$ is the firm's market value
#' $capital_{it}$ is the firm's capital stock
#' $u_{it}$ is the error term
#' 
#' 
#' 
#' 
#' If you ignore the panel aspects of the dataset, and simply estimate the model using OLS, you will get this result:
#' 
## ----------------------------------------------------------------------------------
invOLS <-lm( inv ~ value + capital, data=Grunfeld )
summary(invOLS)

#' While this will look perfectly reasonable, the underlying issues with it are as follows: 
#' 
#' 1) the assumptions of OLS are that the observations are independent and identically distributed (i.i.d). Time series, and especially panel data, often violate this assumption
#' 
#' 2) OLS also assumes the dependent variable is conditional on a vector of covariates where
#' 
#' $$
#' E(y_{it}|\textbf{x}_{it})=\textbf{x}'_{it}\beta \; \; \forall \, i,t $$
#' 
#' $$
#' Var(y_{it}|\textbf{x}_{it})=\sigma^{2} \; \; \forall \, i,t $$
#' 
#' With panel data, we may not have a common conditional probability density function as assumed above. This will lead to higher significance levels than would potentially exist in the properly estimated panel data set. However, it will also lead to inefficient, and potentially biased estimators, if there are correlations among individual fixed and random effects with the covariates.
#' 
#' Solution #1:
#' 
#' Add a dummy variable
#' 
#' $$
#' E(y_{it}|\textbf{x}_{it}, z_{it})$$
#' 
#' In the Grunfeld data set, this would result in this specification of the model:
#' 
#' $$
#' inv_{it}=\beta_{0} + \beta_{1} value_{it} + \beta_{2} capital_{it} + \sum_{j=2}^{n} \alpha_{j}\textbf{I}(i=j) + \sum_{j=2}^{T} \gamma_{j}\textbf{I}(t=j)u_{it} $$
#' 
#' which adds firm specific ($\alpha_{j}$) and time specific ($\gamma_{j}$) values to the model.   
#' 
#' Here we can have R create dummy variables for us, for different models:
#' 
#' Firm-specific effects:
#' 
## ----------------------------------------------------------------------------------
invLSDVi <- lm( inv ~ value + capital + factor( firm ),data=Grunfeld )
summary(invLSDVi)

#' 
#' 
#' Time-specific effects:
#' 
## ----------------------------------------------------------------------------------
invLSDVt <- lm( inv ~ value + capital + factor( year ),data=Grunfeld )
summary(invLSDVt)

#' 
#' 
#' Both types of effects:
#' 
## ----------------------------------------------------------------------------------
invLSDV2 <- lm( inv ~ value + capital + factor( firm ) + factor( year ), data=Grunfeld )
summary(invLSDV2)

#' 
#' Here is a visual comparison of fixed effect regression to Pooled OLS (using just one regressor)
#' 
## ----------------------------------------------------------------------------------
invLSDVi1 <- lm( inv ~ capital + factor( firm ),
data=Grunfeld )
invHat <- fitted( invLSDVi1 )
library( "car" )
scatterplot( invHat ~ capital | firm, data=Grunfeld,
legend=list( coords="bottomright" ) )
abline( lm( inv ~ capital, data=Grunfeld ), lwd=3,
col ="black" )
legend( "topleft", "Pooled OLS regression line",
col ="black", lwd =3 )

#' 
#' 
#' The black line is the OLS regression line. The other lines are the fixed effects lines. You should be able to see that both their slopes, and their intercepts are different. 
#' 
#' Let's move on from these methods, and prepare the data set for analysis. 
#' 
#' YOu typically want to sort your data by year, and then by the index. Here that would be the firm. 
#' 
## ----------------------------------------------------------------------------------
GrunfeldSortYear <- Grunfeld[order(Grunfeld$year, Grunfeld$firm ), ]
GrunfeldSortYear[ c( 1:3, 198:200 ), 1:2 ]

#' 
#' We are now ready to estimate the data. Panel estimation requires data to be in the "long" format. If your data is in the "wide" format, there are methods that can change this. 
#' 
## ----------------------------------------------------------------------------------
library(plm)

#' 
#' This next set of code converts the data.frame to a pdata.frame. 
#' 
## ----convert_to_panel--------------------------------------------------------------
GrunfeldPdata <- pdata.frame( Grunfeld,index=c( "firm", "year" ) )

#' 
#' Changing the data to a panel does the following to the dataset:
#' 
#' 1) It sets the class to "pdata.frame" 
#' 2) It adds an "index" to the data set and to each variable in the data
#' set. This is a data.frame with two variables: the individual identifier and the time identifier: 
#' 
#' You can see this new attribute here:
#' 
## ----------------------------------------------------------------------------------
attr(GrunfeldPdata, "index")[c(1:3, 198:200),]


#' 3) It sets the class of each individual variable to "pseries"
#' 
#' 4) It modifies the row names of the data set so that they indicate the individual identifier and the time identifier.
#' 
## ----------------------------------------------------------------------------------
rownames( GrunfeldPdata ) [ c( 1:3, 198:200 ) ]


#' 
#' 5) It converts the variables that identify the individuals and the time periods to categorical (factor) variables.
#' 
#' If the first variable of the data set is the individual identifier and the second variable is the time identifier, the explicit conversion of the data set with pdata.frame() is not necessary. In this case, the data set can be used as is.
#' 
#' We are ready to estimate.
#' 
#' The main idea behind panel data sets is that you want to estimate this model:
#' 
#' 
#' $$
#' E(y_{it}|\textbf{x}_{it}) = \textbf{x}_{it}\beta_{it}$$
#' 
#' Any heterogeneity not captured by $\textbf{x}$ can be estimated by $\theta_{it}=(\beta_{it},\sigma_{it}^2)$, a parameter vector that varies over all time and individuals. 
#' 
#' This is not estimable. Therefore, you decompose $\theta_{it}$ into $\theta_{it}=(\beta,\lambda_{it},\sigma^2)$, where $\beta$ and $\sigma^2$ are fixed, and $\lambda_{it}$ can vary, capturing the fixed ($\alpha_{it}$) or time ($\gamma_{it}$) effects.
#' 
#' Fixed-Effects Estimation Using "Within" Transformation
#' 
#' This model uses the following estimation:
#' 
#' $$
#' inv_{it}=\beta_{0} + \beta_{1} value_{it} + \beta_{2} capital_{it} + u_{it} $$
#' 
#' $$u_{it} = \alpha_{i} + \gamma_{t} + \epsilon_{it}$$
#' Here, $\alpha_{i}$ is the firm-specific effects, $\gamma_{t}$ gives the time-specific effects, and $\epsilon_{it}$ gathers all the rest of the error. 
#' 
#' YOu can also set either of the parameters $\alpha_{i}$ or $\gamma_{t}$ to 0 to estimate just the firm-or-time specific effects. 
#' 
#' Firm-Specific Effects
## ----------------------------------------------------------------------------------
invFEi <-plm( inv~value + capital, data=Grunfeld )
summary(invFEi)
fixef(invFEi)
summary(fixef(invFEi))

#' Time-Specific Effects
## ----------------------------------------------------------------------------------
invFEt <-plm( inv~value + capital, effect = "time",
data = Grunfeld )
summary(invFEt)
fixef(invFEt)
summary(fixef(invFEt))

#' Both Effects
## ----------------------------------------------------------------------------------
invFE2 <-plm( inv ~ value + capital, effect="twoways",
data = Grunfeld )
summary(invFE2)
fixef( invFE2, effect="individual" )
fixef( invFE2, effect="time" )
summary(fixef( invFE2, effect="individual" ))
summary(fixef( invFE2, effect="time" ))

#' 
#' 
#' Random Effects model
#' 
#' we will estimate time-invariant random effects models. You will explore individual-invariant random effects models on your own. We will not be exploring two-way fixed and random effects. This model uses the default procedure, developed in this paper: Swamy and Arora (1972)
#' 
## ----------------------------------------------------------------------------------
invRE2a <- plm( inv ~ value + capital, data=Grunfeld,effect="twoways", model="random" )
summary(invRE2a)

#' 
#' This model uses the random method developed in this paper: Amemiya (1971)
#' 
## ----------------------------------------------------------------------------------
invRE2b <- plm( inv ~ value + capital, data=Grunfeld,effect="twoways", model="random", random.method="amemiya" )
summary(invRE2b)

#' 
#' Note the presence of a time effect in the second model, that is absent in the first. 
#' 
#' The questions we now ask are, is the random effects model more accurate than the fixed-effect model? The Durbin-Wu-Hausman test can compare them and tell us. If you reject the null hypothesis (statistically significant result) then the estimators in random effects model are assumumed to be correlated with the covariates, and we reject them.
#' 
## ----------------------------------------------------------------------------------
phtest( invFE2, invRE2b )

#' 
#' We therefore conclude that there are not random effects in the time-invariant model. 
#' 
#' Therefore this is our final model:
#' 
## ----------------------------------------------------------------------------------
invFE2 <-plm( inv ~ value + capital, effect="twoways",
data = Grunfeld )
summary(invFE2)
summary(fixef( invFE2, effect="individual" ))
summary(fixef( invFE2, effect="time" ))

#' 
#' 
#' 
