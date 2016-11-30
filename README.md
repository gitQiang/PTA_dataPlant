# PTA price prediction

Here, we combined different information and tried to predict PTA price in different dimensions.

# Data update in daily and write into Mysql database.

1) Update weather data

ChoiceWeatherDataUpdate.bat

2) Update Choice data

ChoiceDataUpdate.bat

3) Update Wind data 

DataUpdate.bat and DataUpdatePTA.bat in "Wind".

# Main functions

train_predict.R: main interface function to run 

DataClean.R: data preparing functions

# Other useful files:

getWeather.py: get today weather data

getWeatherData.R: write today weather data into database table (not upload with password)

getWindData.R: write today wind data into database table (not upload with password)

getChoiceData.R: write today choice data into database table (not upload with password)

miscNew.R: misc functions for PTA models of new branch (fill1)

Models.R: all models for training and predicting

log.txt: python log file

README: this file

other files will beremoved from the final online version.

# Contact information

Qiang Huang (email: huangqiang@3golden.com.cn)

Copyright ©2016 All rights reserved.




