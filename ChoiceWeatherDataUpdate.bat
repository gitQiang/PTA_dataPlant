:: work flow
:: Update weather data
python D:/code/PTA_dataPlant/getWeather.py
:: write weather to database
Rscript D:/code/PTA_dataPlant/getWeatherData.R
:: Update Choice data
python D:/code/PTA_dataPlant/ChoiceCode-YangMing/bat/main.py
:: Update clear
Rscript D:/code/PTA_dataPlant/getChoiceData.R
