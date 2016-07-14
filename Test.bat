@echo off 
if "%1" == "h" goto begin 
mshta vbscript:createobject("wscript.shell").run("%~nx0 h",0)(window.close)&&exit 
:begin
:: 下面是我的代码   
:ag   
set t=22:22:30
@REM t为你设置的时间  
if %t%==%time:~0,8% goto word   
goto ag   
:word   
:: work flow
:: Update Wind data 
Rscript --vanilla D:/code/PTA_dataPlant/getUpdateData.R
:: Update weather data
python D:/code/PTA_dataPlant/getWeather.py
:: Update Choice data
python D:/code/PTA_dataPlant/ChoiceCode/bat/main.py
:: Update clear
Rscript D:/code/PTA_dataPlant/DayUpdateClear.R
goto ag
