#!/usr/bin/env python

from ecmwfapi import ECMWFDataServer
import os
import sys
import pandas as pd
print(sys.argv)

args = sys.argv

year_start = pd.to_numeric(args[1])
year_end = pd.to_numeric(args[2])
dname = args[3]

param_dic = {'t2m': '167.128', 'd2m': '168.128'}

# define metrics to download
param = param_dic[dname]

# define directory to place files
home = os.getenv("HOME")
path = home + '/data/climate/net_cdf/' + dname + '/raw_era5/'

# check if file directory exists
if not os.path.exists(path):
    os.makedirs(path)

# change directory to desired file location
os.chdir(path)

# create server object
server = ECMWFDataServer()


def retrieve_interim_worldwide_sep_onevar(start, end):
    """      
       A function to demonstrate how to iterate efficiently over several years and months etc    
       for a particular interim_request.
    """

    for year in list(range(start, end + 1)):
        startDate = '%04d%02d%02d' % (year, 1, 1)
        lastDate = '%04d%02d%02d' % (year, 12, 31)
        target = "worldwide_" + dname + "_daily_four_%04d.nc" % year
        requestDates = (startDate + "/TO/" + lastDate)
        interim_request_worldwide(requestDates, target)


def retrieve_interim_worldwide_together_onevar(year_start, year_end):
    """
       A function to demonstrate how to iterate efficiently over several years and months etc
       for a particular interim_request.
    """

    startDate = '%04d%02d%02d' % (year_start, 1, 1)
    lastDate = '%04d%02d%02d' % (year_end, 12, 31)
    target = "worldwide_" + dname + "_daily_four_%04d_%04d.nc" % (year_start, year_end)
    requestDates = (startDate + "/TO/" + lastDate)
    interim_request_worldwide(requestDates, target)


def interim_request_worldwide(requestDates, target):
    """      
        An ERA5 request from server.
    """
    f = open(target, 'w+')

    server.retrieve({
        "class": "ea",
        "dataset": "era5",
        "date": requestDates,
        "expver": "1",
        # "grid": "0.75/0.75",
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

if __name__ == '__main__':
    retrieve_interim_worldwide_sep_onevar(year_start, year_end)
