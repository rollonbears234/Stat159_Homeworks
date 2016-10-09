#Read in Advertising and compute regression object via lm(), summary of this object using summary()
#Also produces a scatter with regression line - save in PNG
#Predict Relationship between the variables

adData = read.csv(file = "../../data/Advertising.csv")

tv = adData$TV
radio = adData$Radio
newspaper = adData$Newspaper
sales = adData$Sales


rel_tv_sales = lm(formula = sales ~ tv)
rel_radio_sales = lm(formula = sales ~ radio)
rel_newspaper_sales = lm(formula = sales ~ newspaper)

summary_tv_sales = summary(rel_tv_sales)
summary_radio_sales = summary(rel_radio_sales)
summary_newspaper_sales = summary(summary_radio_sales)

save(rel_tv_sales,rel_radio_sales,rel_newspaper_sales, file = "../../data/regression.RData")



#Making scatter plots
png('../../images/scatterplot-tv-sales.png')
plot(tv, sales)
abline(rel_tv_sales)
dev.off()

png('../../images/scatterplot-radio-sales.png')
plot(radio, sales)
abline(rel_radio_sales)
dev.off()

png('../../images/scatterplot-newspaper-sales.png')
plot(newspaper, sales)
abline(rel_newspaper_sales)
dev.off()

#Making plots from lm
lm.collective <- lm(Sales ~ TV + Newspaper + Radio, data = adData)
png('../../images/residual-plot.png')
plot(lm.collective, which = 1, main = "Residual Plot")
dev.off()

png('../../images/scale-location-plot.png')
plot(lm.collective, which = 3, main = "Scale Location")
dev.off()

png('../../images/normal-qq-plot.png')
plot(lm.collective, which = 2, main = "Normal QQ")
dev.off()
