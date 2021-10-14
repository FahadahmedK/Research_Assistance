#!/usr/bin/env python
# coding: utf-8

# In[1]:


from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from time import sleep
import selenium
from urllib.request import urlopen
import requests
import time


# In[2]:


url = 'https://nl.sub.uni-goettingen.de/collection/nlh-fta'
options = Options()
prefs = {'download.prompt_for_download': False, 'plugins.always_open_pdf_externally': True}
options.add_experimental_option('prefs', prefs)
options.headless = False
driver = webdriver.Chrome(options = options)
driver.get(url)


# In[24]:


for year in range(1930, 1940):
    showmore =  WebDriverWait(driver, 15).until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[3]/section/div[3]/article[3]/button[1]/i')))
    showmore.click()
    year = driver.find_element_by_partial_link_text(str(year))
    year.click()
    for page in range(2):
            try:
                content = driver.find_elements_by_partial_link_text('Financial times historical archive')
                for i in range((len(content))):
                        content = driver.find_elements_by_partial_link_text('Financial times historical archive')
                        content[i].click()
                        item = WebDriverWait(driver, 15).until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/section/header/div[2]/div/div/button[5]')))
                        item.click()
                        time.sleep(3)
                        try:
                            pdf = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/section/div[1]/div[5]/section/p[1]/a')))
                            pdf.click()
                            go_back = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH, '/html/body/header/ul/li[1]/a')))
                            go_back.click()
                        except:
                            time.sleep(7)
                            driver.refresh()
                            pdf = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/a')))
                            pdf.click()
                            driver.back()
                            go_back = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH, '/html/body/header/ul/li[1]/a')))
                            go_back.click()
                navigate = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[2]/div[2]/ol/li[10]/a')))
                navigate.click()
            except:
                break 
    retrieve_showmore = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH, '/html/body/div[1]/div[3]/section/div[3]/article[3]/ol/li/a')))
    retrieve_showmore.click()    


# In[ ]:




