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
