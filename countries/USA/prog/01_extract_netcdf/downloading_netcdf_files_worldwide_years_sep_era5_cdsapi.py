#!/usr/bin/env python

import cdsapi
import os
import sys
import pandas as pd
print(sys.argv)

args = sys.argv

year_start = pd.to_numeric(args[1])
year_end = pd.to_numeric(args[2])
dname = args[3]

param_dic = {'t2m': '2m_temperature', 'd2m': 'XX'}

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


def retrieve_era5_worldwide_sep_onevar(start, end):
    """      
       A function to demonstrate how to iterate efficiently over several years and months etc    
       for a particular interim_request.
    """

    for year in list(range(start, end + 1)):
        target = "worldwide_" + dname + "_daily_four_%04d.nc" % year
        era5_request_worldwide(year, target)


def era5_request_worldwide(year, target):
    """
        An ERA5 request from server.
    """
    c = cdsapi.Client()

    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type': 'reanalysis',
            'variable': param,
            'year': year,
            'month': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12'
            ],
            'day': [
                '01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12',
                '13', '14', '15',
                '16', '17', '18',
                '19', '20', '21',
                '22', '23', '24',
                '25', '26', '27',
                '28', '29', '30',
                '31'
            ],
            'time': [
                '00:00', '06:00', '12:00',
                '18:00'
            ],
            'format': 'netcdf'
        },
        target)


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
        "grid": "0.25/0.25",
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
    retrieve_era5_worldwide_sep_onevar(year_start, year_end)
