#PTA_dataPlant

Transform to dataPlant for different models to show.

# File lists

getWeather.py: get today weather data

getWeatherData.R: write today weather data into database table (not upload with password)

OldWeather2Database.R: write history weather data into database table (not upload with password)

getWindData.R: write today wind data into database table (not upload with password)

OldWind2Database.R: write history Wind data into database table (not upload with password)

getChoiceData.R: write today choice data into database table (not upload with password)

OldChoice2Database.R: write history choice data into database table (not upload with password)

getUpdateALL.R: read all related data into R space (not upload with password)

misc.R: misc functions for PTA models

Models.R: all models

PTA_prediction.R: main interface function to run (not upload with password)

ChoiceWeatherDataUpdate.bat: windows task file to update data everyday

log.txt: python log file

README: this file



# Old Version

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


