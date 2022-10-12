rm ( list=ls ())
setwd( '~/Data')

dfData <- read.table( 'brain.dat', sep= '\t', header = TRUE)
dfData

library ( foreign)
library ( psych )
library (car)
library (GGally)
library (ppcor )
library (knitr)

#describe data, mean sd, median, rang
knitr:: kable ( describe( dfData), digits=2)

#Plot the Data
ggpairs (dfData [ , 2:7])
plot(df_Data$FSIQ)

#zero-order correlation
kable (zero_order <- cor ( dfData ), digits =4 )
cor (dfData[ , 1:7])

#see the VIF, the missing values
diag( solve( cor( dfData[ , c('Female','VIQ', 'PIQ', 'MRI_Count')]) ) )

#see the lineal model
summary( Fit <- lm (FSIQ ~ Female + VIQ + PIQ + MRI_Count, data=dfData))

Fit <- lm (FSIQ ~ Female + VIQ + PIQ + MRI_Count, data=dfData)
residual <- resid(Fit)
plot(residual)
abline(0,0)

#see the confidence interval
CI <- confint( Fit )
CI

#Putting things together
regression_table <- summary ( Fit )$coefficients
colnames( CI )

#visualize data between QQ, 4plots, two variables FSIQ | Others
avPlots(Fit)
plot(Fit)

#Residuals vs Fitted
hist(Fit$residuals)

