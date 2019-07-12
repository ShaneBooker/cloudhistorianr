
cloudhistorianr
===============

An R library to simplify connecting to Cloud Historian and retrieving data To use this tool you will need the following information:

-   AppID and Secret
-   SYSGUID of the SDX/CHC uplifting the data
-   Tagnames

Installation
------------

``` r
# The development version from GitHub:
devtools::install_github("shanebooker/cloudhistorianr")

library(cloudhistorianr)
```

Usage
-----

To retrieve data from cloud historian, compelte the following steps:

1.  Get a secuirty Token. Use getCHToken()
2.  Retrieve data from Cloud Historian. Use getCHData()

``` r
token_uri <- 'https://login.windows.net/805ba517-8a74-4377-81c1-6af998bc4709/oauth2/token'
resource <- '<resource>'
client_id <- '<Client Id>'
client_secret <- '<secret>'
token <- get_ch_token(uri = token_uri, 
                    resource = resource, 
                    client_id = client_id, 
                    client_secret = client_secret)

data_uri <- 'https://sentt01eprod.sentienceanalytics.com/api/timeseries/values/summary'
sys_guid <- '<GUID>'
tag_names <- c("Tagname1","Tagname2") #vector list of tagnames 
down_sample <- "1m-avg" 
#'    Example responses for 'down_sample':
#'    Downsample to 1 min average: "1m-avg",
#'    Downsample to 60 second average: "60s-avg",
#'    Downsample to 1 hour average: "1h-avg",
#'    Downsample to avg of all results: "0all-avg",
#'    Downsample to max of all results: "0all-max",
#'    return raw data: ""
start_time <- "2019-02-03T09:00:00.000+09:00" 
end_time <- "2019-02-03T11:00:00.000+09:00"

#Raw data
ch_data <- get_ch_data(uri = data_uri, 
                            token = token$access_token, 
                            start_time = start_time, 
                            end_time = end_time, 
                            tag_names =tag_names, 
                            down_sample = down_sample,
                            sys_guid = sys_guid)


head(ch_data$data)
                 time    value    point_id
1 2019-02-04 05:08:00 1031.669    Tagname1
2 2019-02-04 05:09:00 1031.669    Tagname1
3 2019-02-04 05:10:00 1031.669    Tagname1
4 2019-02-04 05:11:00 1031.669    Tagname1
```

Data will be returned as a data frame in the $data variable. I recommend converting this to a tsibble object. It makes visualization and manipulation much much easier.

``` r
library(tidyverse)
library(tsibble)
library(dygraph)

ch_data_tsibble = ch_data$data %>%
  as_tsibble(
      index = time,
      key = id(point_id),
      regular = FALSE
  )


head(ch_data_tsibble)
# A tsibble: 8,073 x 3 [!] <UTC>
# Key:       point_id [2]
   time                 point_id   value
   <dttm>               <chr>      <dbl>
 1 2019-02-04 07:08:00  Tagname1   0.212
 2 2019-02-04 07:09:00  Tagname1   0.212
 3 2019-02-04 07:10:00  Tagname1   0.212
 4 2019-02-04 07:11:00  Tagname1   0.212 

#Create XTS object
ch_data_tsibble_xts <- ts_xts(ch_data_tsibble)
 
#Graph the data using dygraph
dygraph(ch_data_tsibble_xts,  main = 'Raw Data') %>% 
      dyRangeSelector() %>%
      dyHighlight(highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE) %>%
      dyOptions(useDataTimezone  = TRUE)%>% 
      dyRoller(rollPeriod = 1)%>%
      dyLegend(show = "follow") 
```
