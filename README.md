#PTA_dataPlant

Transform to dataPlant for different models to show.

# Step one:

DataUpdate.bat: update daily data and write the new data table into Mysql database.

In details:

1) Update Wind data 

Rscript --vanilla D:/code/PTA_dataPlant/getUpdateData.R

2) Update weather data

python D:/code/PTA_dataPlant/getWeather.py

3) Update Choice data

python D:/code/PTA_dataPlant/ChoiceCode/bat/main.py

4) Update clear

Rscript D:/code/PTA_dataPlant/DayUpdateClear.R

5) add to old data and write to Mysql database

Rscript D:/code/PTA_dataPlant/data_updating.R


# Step two: 

PTA_prediction.R: load new data and run all the related mdoels.


