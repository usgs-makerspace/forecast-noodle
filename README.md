# forecast-noodle
Illustrating how river forecasts change through time due to model uncertainty.

## Where is this data from?

Found archived NWS flood forecasts on Iowa State University's website Iowa Environmental Mesonet, specifically at the [River Summary tab here](https://mesonet.agron.iastate.edu/river/?all). We will be downloading data for the Mississippi River at Osceola (NWS site OSGA4), which is just north of Memphis, TN on the Mississippi River. Archived forecast data can be [viewed on a plot here](https://mesonet.agron.iastate.edu/plotting/auto/?q=160&station=OSGA4), but we will be downloading data using the url, `https://mesonet.agron.iastate.edu/plotting/auto/plot/160/station:OSGA4::dt:2020-01-06%201951::var:primary::dpi:100.csv`. We can change the data we download by using the template, `https://mesonet.agron.iastate.edu/plotting/auto/plot/160/station:[NWS Site Code]::dt:[YYYY-MM-DD]%20[HHMM]::var:primary::dpi:100.csv`.

## How do I build this repo?

First you need to have the USGS-R repo, scipiper. Then, you can run the following:

```
library(scipiper)

# Run only the steps needed to download data
scmake(remake_file = "1_fetch.yml") 

# Run the complete pipeline
scmake()
```

To update the parameters used to build this repo, update the information in `viz_config.yml`.

## Shelved for now, but where could we go next?

See final videos [here in GS-WMA-IIDD-Makerspace Sharepoint](https://doimspp.sharepoint.com/:f:/r/sites/gs-wma-iidd-makerspace/Shared%20Documents/1%20Non-Channel/3%20Idea%20Blitzes/2020%20January/gage_forecasts?csf=1&e=4tuBpp). 

Feedback:

[Jordan, Joe, Nicole, others] These are tricky concepts – you have the time series of reality but also the timeseries of when models were predicted and to what times those predictions apply. Jordan’s initial thought is he would have shown full forecasts rather than one point per valid time. Joe and Nicole find the current presentation more intuitive for a “real person” 

[Jake] Hypothetical outcome plots might be useful – helps people understand what uncertainty means. Example of hypothetical outcome plot https://twitter.com/oecodynamics/status/1082226682164101122?s=20 

[Sam] show how the forecasted peak changes through time; [Joe] focusing on the timing & magnitude 

[Jeff] could show metrics for how well the predictions capture the timing or magnitude 

If we want hourly forecast data, we will need to look at a different source. The AHPS forecasts only have 6 hour timesteps, so the forecasts we used were not downsampled or anything (https://water.weather.gov/ahps2/hydrograph.php?wfo=jan&gage=plam6). Jordan thinks that there is a CUAHSI archive of NWM output, and a data release of NWM output for Harvey sponsored by CUAHSI (potentially here: https://www.hydroshare.org/resource/2836494ee75e43a9bfb647b37260e461/).
