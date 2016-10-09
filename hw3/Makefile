.PHONY: all
.PHONY: data
.PHONY: eda
.PHONY: regression
.PHONY: report
.PHONY: tests
.PHONY: clean
.PHONY: session

#Prony Targets
all: eda regression report
eda: data/eda-output.txt
regression: data/regression.RData
report: report/report.pdf
session: session-info.txt

data:
	cd data; curl -o Advertising.csv http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv

clean:
	rm report/report.pdf

tests:
	cd code: Rscript test-that.R

session-info.txt:
	cd code/scripts; Rscript session-info-script.R


#Now creating files once called from the phony targets
data/regression.RData:
	cd code/scripts; Rscript regression-script.R

data/eda-output.txt:
	cd code/scripts; Rscript eda-script.R

report/report.pdf: data/regression.RData data/eda-output.txt
	cd report; R -e "rmarkdown::render('report.Rmd')"
