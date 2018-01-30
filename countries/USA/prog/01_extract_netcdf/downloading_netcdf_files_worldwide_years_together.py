#!/usr/bin/env python

from ecmwfapi import ECMWFDataServer
import os
import sys
import pandas as pd
print(sys.argv)

# to use if you run from a bash file (ignore otherwise)
args = sys.argv

# to use if you run from a bash file (ignore otherwise)
year_start = pd.to_numeric(args[1])
year_end = pd.to_numeric(args[2])
dname = args[3]

# dictionary for numerical values of variable names
param_dic = {'t2m': '167.128', 'd2m': '168.128'}

# define metrics to download
param = param_dic[dname]

# define directory to place files
home = os.getenv("HOME")
path = os.path.join(home, 'data/climate/net_cdf', dname, 'raw')   # change to suit

# check if file directory exists
if not os.path.exists(path):
    os.makedirs(path)

# change directory to desired file location
os.chdir(path)

# create server object
server = ECMWFDataServer()

# define functions which to do work
def interim_request_worldwide(requestDates, target):
    """
        DEFINE
    """
    f = open(target, 'w+')

    server.retrieve({
        "class": "ei",
        "dataset": "interim",
        "date": requestDates,
        "expver": "1",
        "grid": "0.75/0.75",
        "levtype": "sfc",
        "param": param,
        "step": "0",
        "stream": "oper",
        "time": "00:00:00/06:00:00/12:00:00/18:00:00",
        "type": "an",
        "target": target,
        "format": "netcdf"
    })

    f.close()


def retrieve_interim_worldwide_together_onevar(year_start, year_end):
    """
       function to retrive implement above with specific year range
    """

    startDate = '%04d%02d%02d' % (year_start, 1, 1)
    lastDate = '%04d%02d%02d' % (year_end, 12, 31)
    target = "worldwide_" + dname + "_daily_four_%04d_%04d.nc" % (year_start, year_end)
    requestDates = (startDate + "/TO/" + lastDate)
    interim_request_worldwide(requestDates, target)

# run set-up functionality
if __name__ == '__main__':
    retrieve_interim_worldwide_together_onevar(year_start, year_end)
