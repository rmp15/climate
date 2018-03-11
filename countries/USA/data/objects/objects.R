# climate metrics currently
metrics <- c("mean","meanc","meanc2","days_below_10","days_above_30","sd","days_changing_by_5","days_increasing_by_5" ,"days_decreasing_by_5","number_of_min_3_day_above_99_upwaves","number_of_min_3_day_below_99_downwaves")

# declare metrics of interest for correlations
metrics.matrix = c(    'meanc3','sd','10percc3','90percc3',
                'number_of_days_above_nonnormal_90_2', 'number_of_days_below_nonnormal_90_2',
                'number_of_min_3_day_above_nonnormal_90_upwaves_2','number_of_min_3_day_below_nonnormal_90_downwaves_2')
