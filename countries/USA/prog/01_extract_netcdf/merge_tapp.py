"""Make a new NetCDF file which merges information from two existing
Following https://gist.github.com/guziy/8543562
"""

import netCDF4
import os

# variable for converting kelvin to celsius
k_to_c = 273.15

# years to process
years = range(1979, 1979+1)
print(years)

for year in years:

    # set year of analysis
    year = year
    print('processing tapp for ' + str(year))

    netcdf_loc = os.path.join('/home/rmp15/data/climate/net_cdf')

    # input filenames
    filename_t2m = os.path.join(netcdf_loc, 't2m', 'raw', 'worldwide_t2m_daily_four_' + str(year) + '.nc')
    filename_d2m = os.path.join(netcdf_loc, 'd2m', 'raw', 'worldwide_d2m_daily_four_' + str(year) + '.nc')

    # output filename
    filename_tapp = os.path.join(netcdf_loc, 'tapp', 'processed', 'worldwide_tapp_daily_four_' + str(year) + '.nc')

    # Read in the first data set.
    print('loading t2m file for ' + str(year))
    t2m = netCDF4.Dataset(filename_t2m)

    # Make a new dataset and give it useful high-level meta information.
    combi = netCDF4.Dataset(filename_tapp,  'w', format='NETCDF4')
    combi.Conventions = 'Extended ' + t2m.Conventions
    combi.history = 'Merged manually from ERA - Interim downloads'

    # Now copy over all of the dimensional information. (Manually here, but see link for auto)
    combi.createDimension('latitude', t2m.dimensions['latitude'].size)
    combi.createDimension('longitude', t2m.dimensions['longitude'].size)
    combi.createDimension('time', t2m.dimensions['time'].size)

    # For the variables we make copies in combi from t2m using a loop.
    # The items() command makes a ordered pair with the variable name and a handle to it appear.
    for v_name, var in t2m.variables.items():
        # The new variable should have the same name, dataype and dimensions.
        new_var = combi.createVariable(v_name, var.datatype, var.dimensions)

        # We also want it to have the same meta-data attributes.
        new_var.setncatts({k: var.getncattr(k) for k in var.ncattrs()})

        # We have to do it in chunks for t2m variable, since it's so big that it eats up the memory.
        # This is a bit hack-y
        if new_var.size > 10**4:
            for t in range(combi.dimensions['time'].size):
                new_var[t, :, :] = var[t, :, :]
        else:
            new_var[:] = var[:]

    # Now combi is a copy of t2m. We can close t2m now for memory reasons.
    t2m.close()
    print('closing t2m file for ' + str(year))

    # Open the other data file.
    print('loading d2m file for ' + str(year))
    d2m = netCDF4.Dataset(filename_d2m)

    # To add the variable of interest from d2m first get a handle on the old variable.
    d2m_old = d2m.variables['d2m'] # Give the netCDF variable a name.

    # Make a slot for it in the combi netcdf file.
    d2m_var = combi.createVariable('d2m', d2m_old.datatype, d2m_old.dimensions)

    # Copy over attributes.
    d2m_var.setncatts({k: d2m_old.getncattr(k) for k in d2m_old.ncattrs()})

    # Copy of the data, split this up so it doesn't use so much memory
    for t in range(combi.dimensions['time'].size):
        d2m_var[t, :, :] = d2m_old[t, :, :]

    # All done
    print('loading d2m file for ' + str(year))
    d2m.close()

    # Get the things you want to build it from in hand.
    t2m_var = combi.variables['t2m']
    d2m_var = combi.variables['d2m']

    # Make a new variable for it.
    tapp = combi.createVariable('tapp', d2m_var.datatype, d2m_var.dimensions)
    tapp.units = 'C'
    tapp.long_name = 'Apparent temperature = combination of humidity and temperature as metric for heat stress'

    print('processing tapp for ' + str(year))

    # Make it but sequentially at each time so that no so much memory is used at once.
    # NEED TO CONVERT t2m and d2m into correct units (kelvin/celsius???)
    for t in range(combi.dimensions['time'].size):
        # work out tapp in Fahrenheit
        tapp[t, :, :] = -2.653 + 0.994*(32+1.8*(t2m_var[t, :, :]-k_to_c)) + 0.0153*(32+1.8*(d2m_var[t, :, :]-k_to_c))**2

        # convert tapp back to celsius
        tapp[t, :, :] = (tapp[t, :, :] - 32)/1.8

    print('processed and saved tapp for ' + str(year))

    # close file to save
    combi.close()


