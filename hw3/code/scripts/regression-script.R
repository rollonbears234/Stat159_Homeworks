#Read in Advertising and compute regression object via lm(), summary of this object using summary()
#Also produces a scatter with regression line - save in PNG
#Predict Relationship between the variables

adData = read.csv(file = "../data/Advertising.csv")

tv = adData$TV
radio = adData$Radio
newspaper = adData$Newspaper
sales = adData$Sales


rel_tv_sales = lm(formula = sales ~ tv)
rel_radio_sales = lm(formula = sales ~ radio)
rel_newspaper_sales = lm(formula = sales ~ newspaper)

summary_tv_sales = summary(rel_tv_sales)
summary_radio_sales = summary(rel_radio_sales)
summary_newspaper_sales = summary(rel_newspaper_sales)

save(rel_tv_sales,rel_radio_sales,rel_newspaper_sales, file = "../data/regression.RData")



png('../images/scatterplot-tv-sales.png')
plot(TV, Sales)
abline(adModel)
dev.off()
