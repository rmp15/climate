#!/usr/bin/env python

from ecmwfapi import ECMWFDataServer
import os
import sys

print(sys.argv)

args = sys.argv

# ADD SOME ARGUMENTS TO THE SCRIPT FOR PROCESSING OF YEARS AND CLIMATE VARIABLE

# CREATE DICTIONARY FOR PARAMETERS

# define metrics to download
param = 165.128  # REPLACE WITH args[1]
dname = 't2m'  # REPLACE WITH DICTIONARY LOOK_UP

# define directory to place files
home = os.getenv("HOME")
path = home + '/data/climate/net_cdf/' + dname + '/raw/'
os.chdir(path)

server = ECMWFDataServer()


def retrieve_interim(start,end):
    """      
       A function to demonstrate how to iterate efficiently over several years and months etc    
       for a particular interim_request.
    """

    for year in list(range(start, end + 1)):
        startDate = '%04d%02d%02d' % (year, 1, 1)
        lastDate = '%04d%02d%02d' % (year, 12, 31)
        target = "worldwide_" + dname + "_daily_four_%04d.nc" % year
        requestDates = (startDate + "/TO/" + lastDate)
        interim_request(requestDates, target)


def interim_request(requestDates, target):
    """      
        An ERA interim request from server.
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

if __name__ == '__main__':
    retrieve_interim(1980, 1981)
