# -*- coding: utf-8 -*-
from __future__ import print_function

__author__ = 'ym'
"""
    Date        : '2016/7/12 0012'
    Description :
    
"""
import urllib2
import time

def download():
    now_string = time.strftime('%Y/%m/%d', time.localtime(time.time()))
    #print (now_string)
    url =  "https://www.wunderground.com/history/airport/ZSHC/"+ now_string +"/DailyHistory.html?req_city=%E6%9D%AD%E5%B7%9E&req_state=ZJ&req_statename=Zhejiang&reqdb.zip=00000&reqdb.magic=1&reqdb.wmo=58457&format=1"
    #print(url)
    f = urllib2.urlopen(url)
    file = open("D:/code/PTA_dataPlant/UpdateData/Weather" + now_string.replace("/","-") + ".csv" , mode='w')

    for line in  f.readlines():
        if len(line.strip()) == 0 : continue
        #print(line.replace("<br />",""), end="")
        file.write(line.replace("<br />",""))
    file.close()

if __name__ == "__main__":
    download()