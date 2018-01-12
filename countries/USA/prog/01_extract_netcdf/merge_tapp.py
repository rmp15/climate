"""Make a new NetCDF file which merges information from two existing
Following https://gist.github.com/guziy/8543562
"""

import netCDF4
import os

netcdf_loc = os.path.join('~/data/climate/net_cdf')

filename_t2m = os.path.join(netcdf_loc, 't2m', 'raw', 'worldwide_t2m_daily_four_1981.nc')
filename_d2m = os.path.join(netcdf_loc, 'd2m', 'raw', 'worldwide_d2m_daily_four_1981.nc')

# Read in the first dataset.
t2m = netCDF4.Dataset(filename_t2m)

# Make a new dataset and give it useful high-level meta information.
combi = netCDF4.Dataset('combined.nc',  'w', format='NETCDF4')
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

# Open the other data file.
d2m = netCDF4.Dataset(filename_d2m)

# To add the variable of interest from d2m first get a handle on the old variable.
d2m_old = d2m.variables['d2m'] # Give the netCDF variable a name.

# Make a slot for it in the combi netcdf file.
d2m_var = combi.createVariable('d2m', d2m_old.datatype, d2m_old.dimensions)

# Copy over attributes.
d2m_var.setncatts({k: d2m_old.getncattr(k) for k in d2m_old.ncattrs()})

# Copy of the data, split this up so it doesn't use so much memory
for t in range(combi.dimensions['time'].size):
    d2m_var[t, :, :] = d2m_old[t,:,:]

# All done
d2m.close()

# Now suppose you wanted to make a new variable with a new formula

# Get the things you want to build it from in hand.

t2m_var = combi.variables['t2m']
d2m_var = combi.variables['d2m']

# Make a new variable for it.
new_guy = combi.createVariable('new_guy', d2m_var.datatype, d2m_var.dimensions)
new_guy.units = 'Joules'
new_guy.long_name = 'the official or at least technical name for the thing you made'

# Make it but sequentially at each time so that no so much memory is used at once.
for t in range(combi.dimensions['time'].size):
    new_guy[t,:,:] = t2m_var[t,:,:]*32 - d2m_var[t,:,:]**2

combi.close()
