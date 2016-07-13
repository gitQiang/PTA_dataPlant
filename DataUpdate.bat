:: work flow
:: Update Wind data 
Rscript --vanilla D:/code/PTA_dataPlant/getUpdateData.R
:: Update weather data
python D:/code/PTA_dataPlant/getWeather.py
:: Update Choice data
python D:/code/PTA_dataPlant/ChoiceCode/bat/main.py
:: Update clear
Rscript --vanilla D:/code/PTA_dataPlant/DayUpdateClear.R
