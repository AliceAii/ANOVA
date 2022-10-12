rm( list=ls())
setwd("~/Data")

library(dplyr)
library(reshape2)
library(ggplot2)
library( psych)
library(ppcor)
library(knitr)
library(GGally)
library(agricolae)
library(car)
library(sjstats)
library(pwr)

df_final = read.csv("HSLSdata.csv")
df_final$f_X1PAR1EDU <- factor (df_final$X1PAR1EDU)
df_final <- na.omit(df_final)

STEM_Credits <- df_final$X3TCREDSTEM
Bachelor_Higher <- df_final$Bachelor.Higher
TotalGPA <- df_final$X3TGPATOT
f_Bachelor <- factor(df_final$Bachelor.Higher)
f_edu <- factor(df_final$X1PAR1EDU)

#Descriptive Statistics
knitr::kable (describe (df_final ), digits = 2)
summary(df_final$f_X1PAR1EDU)
educount <- dplyr::count(df_final, f_X1PAR1EDU)

#parents education plot
pdf("hist.pdf", width=10.0,height=7.0 )
plot_hist <- ggplot(df_final, aes(x=factor(f_X1PAR1EDU)))+
  geom_bar(width=0.7, fill = "navy")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)
theme_minimal()
print(plot_hist +
        labs(y = "count", x =" Parent's Highest Level of Education"))
dev.off()

#plot the data, regression
pdf("ggpairs.pdf", width=7.0,height=7.0 )
ggpairs(df_final, columns = 1:2)
dev.off()
ggpairs(df_final, columns = 1:2, ggplot2::aes(colour = f_X1PAR1EDU) )

#classify by parents' education
pdf("lm.pdf", width=10.0,height=7.0 )
plot2 <- ggplot(data= df_final)+
  geom_point(mapping = aes(x =X3TCREDSTEM, y = X3TGPATOT, color = f_X1PAR1EDU))+
  scale_color_discrete(name = "Parent's Highest Education")+
  geom_smooth(method = 'lm', mapping = aes(x = X3TCREDSTEM, y=X3TGPATOT))
print(plot2 + 
        ggtitle("The Correlation between Student Total GPA And Credits Earned in STEM Grouped by Parent's Highest Education")+
        labs(y= "Student Total GPA", x= "Credits Earned in STEM"))
dev.off()

#boxplots and lm
pdf("box.pdf", width=14.0,height=6.0 )
boxplot(df_final$X3TGPATOT ~ df_final$f_X1PAR1EDU,
        main = "Student Total GPA by Parent's Highest Education",
        xlab = "Parent's Highest Education", ylab = "Student Total GPA",
        col = "red", border = "black")
dev.off()

pdf("reg.pdf", width=10.0,height=7.0 )
plotedu <- ggplot( df_final, aes(X3TCREDSTEM, X3TGPATOT, color=X1PAR1EDU)) + 
  geom_smooth(method =lm, se = F, fullrange =T) +
  scale_color_discrete(name = "Parent's Highest Education") +
  theme(legend.key.size = unit(1, "cm")) 
print(plotedu + 
        ggtitle("The Regression of Student Total GPA And Credits Earned in STEM Grouped by Parent's Highest Education")+
        labs(y= "Student Total GPA", x= "Credits Earned in STEM"))
dev.off()

#ANOVA between GPA and parent education
anova <- aov ( X3TGPATOT ~ f_X1PAR1EDU, data=df_final)
summary(anova)
anova_stats(anova)

#Tukey's HSE poc-hoc test
TukeyHSD(anova, conf.level=.95)
result <- HSD.test(anova, 'f_X1PAR1EDU')
result
# plot the multiple comparisons
datameans <- result$means
datagroups <- result$groups
ind <- match( row.names(datameans), row.names(datagroups))
datameans$groups <- datagroups$groups[ind]

pdf("CI.pdf", width=10.0,height=7.0 )
plotdata <- as.data.frame( cbind(row.names(datameans), datameans[,1],
                                 datameans$std, as.character(datameans$groups)))
names(plotdata) <- c("Treat", "y", "std", "groups")
plotdata$y <- as.numeric(as.character(plotdata$y))
plotdata$std <- as.numeric(as.character(plotdata$std))

ggplot(plotdata, aes(x= Treat, y=y)) +
  geom_col()+
  ggtitle("95% CI for the Mean")+
  labs(y= "Total GPA", x= "Parent's Highest Education")+
  geom_errorbar(aes(ymin = y - 2*std, ymax = y +std), width =0.2,)+
  geom_text(mapping = aes(y = y + 2*std, label = groups, vjust = -1))
dev.off()


#correlation
with(df_final, cor.test(X3TGPATOT, X3TCREDSTEM)) 
plot(df_final$X3TGPATOT, df_final$X3TCREDSTEM)

#要用ANCOVA的假定模型检验是否有交互效应
anova2 <- aov ( X3TCREDSTEM ~ f_X1PAR1EDU, data=df_final)
summary(anova2)
anova_stats(anova2)#delete this part

##Anova test for the model
ancova <- aov(X3TGPATOT ~ X3TCREDSTEM*f_X1PAR1EDU, data=df_final)
summary(ancova) 
anova_stats(ancova)
Anova(ancova, type="2")

#X3TCREDSTEM:f_X1PAR1EDU p=0.0123 有显著性的交互关系，所以就不能用ancova

#boxplot
boxplot(df_final$X3TCREDSTEM ~ df_final$f_X1PAR1EDU,
        main = "Credits Earned in STEM by Parents' Highest Education",
        xlab = "Parents' Highest Education", ylab = "Credits Earned in STEM",
        col = "red", border = "black")

#multiple correlation with interaction variables
#anova results shows there is a correlation between stem and parentedu
class(df_final$f_X1PAR1EDU)
levels(df_final$f_X1PAR1EDU)# there is an empty level, how can we delete it?
str(df_final)
#bachelor as a comparison group
model_interact <- lm(X3TGPATOT ~ X3TCREDSTEM*Bachelor.Higher, data=df_final)
summary(model_interact)
#the same as: lm(X3TGPATOT ~ X3TCREDSTEM + Bachelor.Higher + X3TCREDSTEM:Bachelor.Higher, data=df_final)

# as a factor
model_interact2 <- lm(X3TGPATOT ~ X3TCREDSTEM*f_Bachelor, data=df_final)
summary(model_interact2)

#for less than bachelor y= 1.189+0.197*STEM
#for bachelor and higher y = 2.364+0.104*STEM
#(12.63, 3.68)

#plot the regression data
STEM_Credits <- df_final$X3TCREDSTEM
Bachelor_Higher <- df_final$Bachelor.Higher
TotalGPA <- df_final$X3TGPATOT


plot(STEM_Credits[Bachelor_Higher==0], TotalGPA[Bachelor_Higher==0], col="blue", ylim=c(0,6),
     xlim=c(0,20), xlab="STEM Credits", ylab="Total GPA",
     main="Total GPA VS. STEM Credits, Parent's Education") 
points(STEM_Credits[Bachelor_Higher==1], TotalGPA[Bachelor_Higher==1], col="red",pch=16)
#add a legend
legend("topleft", legend = c("less than Bachelor", "Bachelor/Higher"),
       col = c("blue", "red"), lty = 1:2, cex = 0.8)
#add lines
abline(a=1.189, b=0.197, col= "blue", lwd=1.5)
abline(a=2.364, b=0.104, col= "red", lwd=1.5)
abline(v=10.37, lwd=2, lty=2 )


# Johnson-Neyman method to find "regions" of significance
library("reghelper")

sig_regions(model_interact2, alpha = 0.05, precision = 3)

#the lower is 10.37, but the uper is NA, don't know how to fix it
# do some research of how to interpret the lower level.
# 这一部分是不显著的差异，在这个区域范围外是显著的

#plot the model residual
plot(model_interact)

#plot the sig_region

plot(STEM_Credits[Bachelor_Higher==0], TotalGPA[Bachelor_Higher==0], col="blue", ylim=c(0,6),
     xlim=c(0,20), xlab="STEM Credits", ylab="Total GPA",
     main="Total GPA VS. STEM Credits, Parent's Education") 
points(STEM_Credits[Bachelor_Higher==1], TotalGPA[Bachelor_Higher==1], col="red",pch=16)
#add a legend
legend("topleft", legend = c("less than Bachelor", "Bachelor/Higher"),
       col = c("blue", "red"), lty = 1:2, cex = 0.8)
#add lines
abline(a=1.189, b=0.197, col= "blue", lwd=1.5)
abline(a=2.364, b=0.104, col= "red", lwd=1.5)
abline(v=10.37, lwd=2, lty=2 )
