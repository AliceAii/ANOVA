rm( list=ls() )

df_corr <- read.table( header=T, sep='|', text='
Hours|Performance
4|8
3|5
1|3
2|6
5|8' )

install.packages("ggplot2")

library ( ggplot2 )

fig <- ggplot( df_corr, aes( x=Hours, y=Performance ) ) +
  geom_point( size=3 ) +
  theme_bw()

#Get the means
mean_x <- mean( df_corr$Hours)
mean_y <- mean( df_corr$Performance)

# We know that the regression line must go
# through the intersection of the two means
fig + geom_vline( xintercept=mean_x ) +
  geom_hline( yintercept=mean_y )

#Deviates and Squared Deviates
df_corr$deviate_x <- df_corr$Hours - mean_x
df_corr$deviate_x2 <- df_corr$deviate_x^2
df_corr$deviate_y <- df_corr$Performance - mean_y
df_corr$deviate_y2 <- df_corr$deviate_y^2

sum( df_corr$deviate_x)
sum( df_corr$deviate_y)
sum( df_corr$deviate_x2)
sum( df_corr$deviate_y2)

#Product of the Deviates
df_corr$Products <- df_corr$deviate_x * df_corr$deviate_y

#sum of squares
SS_x <- sum (df_corr$deviate_x2)
SS_y <- sum (df_corr$deviate_y2)

#sum of Products
SP <- sum( df_corr$Products)

#Correlation
r_xy <- SP / sqrt( SS_x * SS_y)

#Coefficient of Determination
r_xy2 <- r_xy2^2

#Coefficient of Alienation
1 - r_xy2

#Slope
b1 <- SP /SS_x

#Intercept 
b0 <- mean_y - (b1* mean_x)

#Let's add the regression line to the plot 
fig + geom_smooth( method='lm', se=F ) 

# Does the line of best fit go through the means?
fig + geom_smooth( method='lm', se=F ) +
  geom_vline( xintercept=mean_x ) +
  geom_hline( yintercept=mean_y )
#! error

#The product values, y hat
df_corr$yhat <- b0 +b1 * df_corr$Hours 

#Residuals
df_corr$Residuals <- df_corr$Performance - df_corr$yhat

#Square the Residuals
df_corr$Residuals2 <- df_corr$Residuals^2
SS_residuals <- sum ( df_corr$Residuals2)

#See the residuals
fig + geom_smooth( method='lm', se=F, formula='y~x' ) +
  geom_segment( data=df_corr, aes( x=Hours, xend=Hours, y=Performance, yend=yhat )
  )

#coefficient of Alienation
SS_residuals / SS_y

#Examine the residuals of the yhat values around the grand mean
df_corr$mean_residual <- df_corr$yhat - mean_y
sum( df_corr$mean_residual)

#Square them
df_corr$mean_residual2 <- df_corr$mean_residual^2
SS_model <- sum( df_corr$mean_residual2)

#Coefficient of Determination
SS_model/SS_y

fig + geom_hline( yintercept=mean_y ) +
  geom_segment( aes( x=Hours, xend=Hours, y=Performance, yend=mean_y ) )
SS_model + SS_residuals
SS_y

# the new stuff, using the tools available to us in R

# Notice that the t statistic and p-values for the 
# correlation and regression are exactly the same...
with( df_corr, cor.test( Hours, Performance))
summary( lm( Performance ~ Hours, data=df_corr))

summary( lm( Residuals ~ Hours, data=df_corr))
with( df_corr, cor.test( Hours, Residuals))

# Visualization
ggplot( df_corr,  aes( x=Hours, y=Residuals ) ) +
  geom_point() +
  theme_bw() +
  geom_smooth( method='lm', se=F, formula='y ~ x ' )
#!error


