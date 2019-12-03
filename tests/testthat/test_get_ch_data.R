context("Get Data Check")
library(cloudhistorianr)


token_uri <- 'https://login.windows.net/805ba517-8a74-4377-81c1-6af998bc4709/oauth2/token'
resource <- 'http://sentt01eprodweb.azurewebsites.net'
client_id <- '233e01b1-31fa-4cf2-a7d3-d6a6910c1605'
client_secret <- 'o.QjIFZcYGs[2/W2g/bcKxI1wqPA7Gl]'
token <- get_ch_token(uri = token_uri,
  resource = resource,
  client_id = client_id,
  client_secret = client_secret)


test_that("data is returned", {
  data_uri <- 'https://t01eprodcloudapp.sentience.honeywell.com/api/timeseries/values/summary'
  sys_guid <- 'aea3c632-e870-486d-b41a-1bed6b0e4411'
  tag_names <- c("ts.kod.1mlp-10.kwh",
                  "ts.kod.1mlp-11.kwh")
  aggregator <- ""
  down_sample <- "1ms-avg"
  start_time <- "2019-02-03T09:00:00.000+09:00"
  end_time <- "2019-02-03T09:10:00.000+09:00"

  ch_data <- get_ch_data(uri = data_uri,
    token = token$access_token,
    start_time = start_time,
    end_time = end_time,
    tag_names =tag_names,
    down_sample = down_sample,
    aggregator = aggregator,
    sys_guid = sys_guid)


  expect_equal(ch_data$url,data_uri)
  expect_equal(ch_data$status_code, 200)
})



test_that("data is NOT returned - bad URL", {
  data_uri <- 'https://t01eprodcloudapp.sentience.honeywell.com/api/timeseries/values/summarys'
  sys_guid <- 'aea3c632-e870-486d-b41a-1bed6b0e4411'
  tag_names <- c("ts.kod.1mlp-10.kwh",
    "ts.kod.1mlp-11.kwh")
  down_sample <- "1m-avg"
  start_time <- "2019-02-03T09:00:00.000+09:00"
  end_time <- "2019-02-03T10:00:00.000+09:00"

  expect_warning(
    ch_data <- get_ch_data(uri = data_uri,
      token = token$access_token,
      start_time = start_time,
      end_time = end_time,
      tag_names =tag_names,
      down_sample = down_sample,
      sys_guid = sys_guid)
  )
  expect_equal(ch_data$status_code, 404)

})


test_that("data is NOT returned - bad GUID", {
  data_uri <- 'https://t01eprodcloudapp.sentience.honeywell.com/api/timeseries/values/summary'
  sys_guid <- 'aea3c632-e870-486d-b41a-1bed6b0e4411aa'
  tag_names <- c("ts.kod.1mlp-10.kwh",
    "ts.kod.1mlp-11.kwh")
  down_sample <- "1m-avg"
  start_time <- "2019-02-03T09:00:00.000+09:00"
  end_time <- "2019-02-03T10:00:00.000+09:00"

  expect_warning(
    ch_data <- get_ch_data(uri = data_uri,
      token = token$access_token,
      start_time = start_time,
      end_time = end_time,
      tag_names =tag_names,
      down_sample = down_sample,
      sys_guid = sys_guid)
  )

  expect_equal(ch_data$status_code, 400)
})

test_that("data is NOT returned - bad tagname", {
  data_uri <- 'https://t01eprodcloudapp.sentience.honeywell.com/api/timeseries/values/summary'
  sys_guid <- 'aea3c632-e870-486d-b41a-1bed6b0e4411'
  tag_names <- c("ts.kod.1mlp-10.kwh",
    "ts.kod.1mlp-11.kwh.bad")
  down_sample <- "1m-avg"
  start_time <- "2019-02-03T09:00:00.000+09:00"
  end_time <- "2019-02-03T10:00:00.000+09:00"

  expect_warning(
    ch_data <- get_ch_data(uri = data_uri,
      token = token$access_token,
      start_time = start_time,
      end_time = end_time,
      tag_names =tag_names,
      down_sample = down_sample,
      sys_guid = sys_guid)
  )


  expect_equal(ch_data$status_code, 200)
})


test_that("data is NOT returned - bad timestamp", {
  data_uri <- 'https://t01eprodcloudapp.sentience.honeywell.com/api/timeseries/values/summary'
  sys_guid <- 'aea3c632-e870-486d-b41a-1bed6b0e4411'
  tag_names <- c("ts.kod.1mlp-10.kwh",
    "ts.kod.1mlp-11.kwh")
  down_sample <- "1m-avg"
  start_time <- "2019-02-03T79:00:00.000+09:00"
  end_time <- "2019-02-03T10:00:00.000+09:00"

  expect_warning(
    ch_data <- get_ch_data(uri = data_uri,
      token = token$access_token,
      start_time = start_time,
      end_time = end_time,
      tag_names =tag_names,
      down_sample = down_sample,
      sys_guid = sys_guid)
  )

  expect_equal(ch_data$status_code, 400)
})

test_that("data is NOT returned - bad downsample", {
  data_uri <- 'https://t01eprodcloudapp.sentience.honeywell.com/api/timeseries/values/summary'
  sys_guid <- 'aea3c632-e870-486d-b41a-1bed6b0e4411'
  tag_names <- c("ts.kod.1mlp-10.kwh",
    "ts.kod.1mlp-11.kwh")
  down_sample <- "1m-avgbaddd"
  start_time <- "2019-02-03T09:00:00.000+09:00"
  end_time <- "2019-02-03T10:00:00.000+09:00"

  expect_warning(
    ch_data <- get_ch_data(uri = data_uri,
      token = token$access_token,
      start_time = start_time,
      end_time = end_time,
      tag_names =tag_names,
      down_sample = down_sample,
      sys_guid = sys_guid)
  )

  expect_equal(ch_data$status_code, 400)
})

test_that("request is ok, but no data in given timerange", {
  data_uri <- 'https://t01eprodcloudapp.sentience.honeywell.com/api/timeseries/values/summary'
  sys_guid <- 'aea3c632-e870-486d-b41a-1bed6b0e4411'
  tag_names <- c("ts.kod.1mlp-10.kwh",
    "ts.kod.1mlp-11.kwh")
  aggregator <- ""
  down_sample <- "1m-avg"
  start_time <- "2011-02-03T09:00:00.000+09:00"
  end_time <- "2011-02-03T09:10:00.000+09:00"


  expect_warning(
    ch_data <- get_ch_data(uri = data_uri,
      token = token$access_token,
      start_time = start_time,
      end_time = end_time,
      tag_names =tag_names,
      down_sample = down_sample,
      aggregator = aggregator,
      sys_guid = sys_guid)
  )

})
