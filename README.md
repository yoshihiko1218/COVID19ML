# Classification of COVID19 and influenza patients

Use XgBoost_COVIDandFLUWithoutNan.ipynb to run classification between COVID-19 and influenza paitents. <br />
Raw input file required is UsedCombined.txt  <br />
COVID19_PatientTable_WithCitations.txt includes literature references to all source data for COVID-19 patients. 

Use sombrero.R to run Self-Organizaing Map (SOM) for clustering COVID-19 patients into subgroups. <br />
Raw input file required is LiteratureSearchDataindividualpatients.tsv <br />

Use covidRF.R and covidRidgeLAsso.R for classification between COVID-19 and influenza patients using random forest, RIDGE, or LASSO regression. <br />
Raw input file required is COVIDandFLUdata.csv <br />

Dependencies for  XGBoost include: <br />
xgboost <br />
pandas <br />
sklearn <br />
bayes_opt <br />
numpy <br />
matplotlib <br />
scikitplot <br />
graphviz <br />

Dependencies for SOM include: <br />
SOMbrero <br />

Dependencies for RIDGE, LASSO, and random forest include: <br />
glmnet <br />
pROC <br />
randomForest <br />
