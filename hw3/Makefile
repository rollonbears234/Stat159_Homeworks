.PHONY: all

# all: data/eda-output.txt data/regression.RData report/report.pdf
#
# data/regression.RData:
# 	cd code; Rscript regression-script.R
#
# data/eda-output.txt:
# 	cd code; Rscript eda-script.R
#
# report/report.pdf: data/regression.RData data/eda-output.txt
# 	cd report; R -e "rmarkdown::render('report.Rmd')"
#
#
.PHONY: data

data:
	cd data; curl -o Advertising.csv http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv


.PHONY: clean

clean:
	rm report/report.pdf
