# -*- coding: utf-8 -*-
"""
Created on Fri Nov  2 16:05:51 2018

@author: kxu
"""

from pandas.io.excel import ExcelWriter
import pandas
import os
import datetime
import shutil


def csvtoxlsx(fullpath): ## use full file path
    with ExcelWriter('Data.xlsx') as ew:
        test =pandas.read_csv(fullpath, encoding='cp1252', header = None )
        test.to_excel(ew, sheet_name="Data", index = None, header = False)
        
        
def rename(newpath): ## use full path for renaming - if run in the wrong place it could easily rename the wrong files
                ## also code will always be store away from data ## not good practice
    for k in os.listdir(newpath):
        # start, ext = os.path.splitext(k)
        # if ext == "xlsx"
        ext = os.path.splitext(k)[1][1:]
        if ext == "xlsx":
            newfilename = "Data.xlsx"
            os.rename(k, newfilename)
        elif ext == "csv":
            csvtoxlsx(k)
            newfilename = "Data.csv"
            os.rename(k, newfilename)        
                
        
def submit(source, dest):
    sublist = os.listdir(source)
    count = 0
    os.chdir(source) ## do not like this
    for k in sublist:
        count +=1
        ## create runid function
        ## also just concatenate year,month....
        now = str(datetime.datetime.now())
        out = "".join(c for c in now if c not in ('!','.',':', ' ', '-', ':'))[:14]
        ## adding sequence to runid
        form = str(format(count, "04"))
        foldname = (out+"%s") % (form)
        print(foldname)
        ## creating runid folder
        newpath = dest + foldname
        ## create function of check exists and create if not
        if not os.path.exists(newpath):
            os.makedirs(newpath)
            origfile = source + k
            ## break the below out, for some reason the folder may exist and file still needs to be moved
            ## also we may need to log all eventualities
            shutil.move(origfile,newpath)
            os.chdir(newpath) ## this maybe ok, just not comfortable with it
            rename(newpath) ## prefer this to be a rename of an absolute path
        
s = 'G:\\Data_Dev\\04_Magic\\002_CellExtraction\\04_CellDBUsingPython\\Submission\\'
d = 'G:\\Data_Dev\\04_Magic\\002_CellExtraction\\04_CellDBUsingPython\\Store_Submitted\\'

submit(s,d)
        


            
        
        