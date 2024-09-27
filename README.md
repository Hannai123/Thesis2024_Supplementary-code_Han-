# Thesis2024_Supplementary-code_Han-
The R code includes data cleaning, data restructuring, and fixed effect regression analysis for the master's thesis research "Investigating the impact of extreme heat on the financial performance of wine processors in Europe". 
Those file need to be download first to "read". 
In **0 Wine.R**, mainly actions are data cleaning and restructering the financial data and weather data:
"240403_Wine1.xlsx"
"240403_Wine2.xlsx"
"240403_Wine3.xlsx"
"geocoded_addresses.xls"
"tx_ens_mean_0.1deg_reg_v29.0e.nc"  #E-OBS: daily maximum temperature TX
"TRain_firm_output(3-11).csv"
"df_cleaned.csv"

"df_Panel.csv" --> Can be directly used as the input in **1 Wine.R**
and 
"df_Panel.33_39_fe"

In **1 Wine.R**, mainly actions are Fixed effect regression model estimation:
"df_Panel.csv"
"df_Panel.33_39_fe"
"Tmax_firm_output(3-11).csv"
"Tmax_Above.33.Simplify_output.csv"
"Tmax_Above.37.Simplify_output.csv"
"Tmax_Above.39.Simplify_output.csv"
