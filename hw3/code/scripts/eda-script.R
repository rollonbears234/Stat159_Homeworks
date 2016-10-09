#Reads in data and computes summary statistic

adData = read.csv(file = "../data/Advertising.csv")

tv = adData$TV
radio = adData$Radio
newspaper = adData$Newspaper
sales = adData$Sales


#Saving Summary Statistics to eda-output.txt
sink(file = "../data/eda-output.txt")

#Summary Statistics

#TV
summary(tv)
#Radio
summary(radio)
#Newspaper
summary(newspaper)
#Sales
summary(sales)

#Correlation Matrix
together = data.frame(tv,radio,newspaper,sales)
cor_matrix <- cor(together)
cor_matrix

sink()

#Saving Correlation Matrix
save(cor_matrix, file = "../data/correlation-matrix.RData")



#Adding Histograms to Images
png('../images/histogram-sales.png')
hist(sales, main = "Sales")
dev.off()

png('../images/histogram-tv.png')
hist(tv, main = "TV")
dev.off()

png('../images/histogram-radio.png')
hist(radio, main = "Radio")
dev.off()

png('../images/histogram-newspaper.png')
hist(sales, main = "Newspaper")
dev.off()

#Scatterplot Matrix
png('../images/scatterplot-matrix.png')
pairs(~TV+Radio+Newspaper+Sales,data=adData,
   main="Scatterplot Matrix")
dev.off()
