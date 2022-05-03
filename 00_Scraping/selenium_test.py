import pandas as pd
import numpy as np
import multiprocessing as mp
import re
import sys
import requests
import time
from selenium import webdriver
from bs4 import BeautifulSoup as bs
from selenium.webdriver.common.by import By


driver = webdriver.Chrome(executable_path='C:\\Users\\Joshualevy\\AppData\\Local\\Programs\\Chromedriver\\chromedriver.exe')
driver.maximize_window()

driver.get('https://www.proxymonitor.org/reports/prtproposal.aspx?pid=700')


## FIRST WE NEED TO CHANGE THE SAVE TYPE 
driver.find_element(By.ID, "ReportToolbar1_Menu_ITCNT3_SaveFormat_I").click()
driver.find_element(By.ID, "ReportToolbar1_Menu_ITCNT3_SaveFormat_DDD_L_LBI6T0")



driver.find_element(By.ID, "ReportToolbar1_Menu_DXI2_I").click()
time.sleep(5)



content = bs(driver.page_source, parser='html_parser')
print(content)




driver.quit()