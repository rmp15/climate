model_name = {'MPI','ICHEC_KNMI','ICHEC_SMHI','MOHC_SMHI',};
scen_name = {'_hist19701999','_4520202049','_8520202049','_4520692098','_8520692098'};

for model = 4:4
    for scen = 1:5
        clear v
        scenario_years = strcat(model_name{1,model},scen_name{1,scen});
        tas_file = strcat('tas',scenario_years);
        tdps_file = strcat('tdps',scenario_years);
        load(tas_file);
        tas = v.o;
        tas = tas - 273.15;
        load(tdps_file);
        tdps = v.o;
        tdps = tdps - 273.15;
        clear v
        appt = (0.0153*(tdps.*tdps)) + (0.994*tas) - 2.653;
        variable_name = 'appt';
        v.name = variable_name;
        v.lbound = NaN;
        v.ubound = NaN;
        v.gridded = 0;
        v.sLat = 0;
        v.sLon = 0;
        v.dims = {'nDaysinYear','nYears','nLocations'};
        v.o = appt;
        save_file_suffix = scenario_years;
        save_file = strcat(variable_name,save_file_suffix)
        save(save_file,'v','-v7.3')
    end
end