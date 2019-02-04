context("Token Check")
library(cloudhistorianr)

test_that("token is returned", {
  token_uri <- 'https://login.windows.net/805ba517-8a74-4377-81c1-6af998bc4709/oauth2/token'
  resource <- 'http://sentgeprodreguiweb.azurewebsites.net'
  client_id <- '298e41fd-251c-4bb2-a22e-41f6b61261ce'
  client_secret <- 'macfI2HUFIonc386wJmImTKaKhscuhUc873Pxl4j26c='
  token <- getCHToken(uri = token_uri,
    resource = resource,
    client_id = client_id,
    client_secret = client_secret)

  expect_equal(token$expires_in, "3600")
  expect_equal(token$resource,resource)
  expect_false(is.null(token$access_token))
  expect_equal(token$status_code, 200)
})


test_that("token is NOT returned - bad Secret", {
  token_uri <- 'https://login.windows.net/805ba517-8a74-4377-81c1-6af998bc4709/oauth2/token'
  resource <- 'http://sentgeprodreguiweb.azurewebsites.net'
  client_id <- '298e41fd-251c-4bb2-a22e-41f6b61261ce'
  client_secret <- 'macfI2HUFIonc386wJmImTKaKhscuhUc873Pxl4j26c=XX'
  token <- getCHToken(uri = token_uri,
    resource = resource,
    client_id = client_id,
    client_secret = client_secret)

  expect_equal(token$status_code, 401)
  expect_equal(token$error, "invalid_client")
})


test_that("token is NOT returned - bad client id", {
  token_uri <- 'https://login.windows.net/805ba517-8a74-4377-81c1-6af998bc4709/oauth2/token'
  resource <- 'http://sentgeprodreguiweb.azurewebsites.net'
  client_id <- '298e41fd-251c-4bb2-a22e-41f6b61261'
  client_secret <- 'macfI2HUFIonc386wJmImTKaKhscuhUc873Pxl4j26c='
  token <- getCHToken(uri = token_uri,
    resource = resource,
    client_id = client_id,
    client_secret = client_secret)

  expect_equal(token$status_code, 400)
  expect_equal(token$error, "unauthorized_client")
})

test_that("token is NOT returned - bad resource", {
  token_uri <- 'https://login.windows.net/805ba517-8a74-4377-81c1-6af998bc4709/oauth2/token'
  resource <- 'http://sentgeprodreguiweb.azurewebsites.netaa'
  client_id <- '298e41fd-251c-4bb2-a22e-41f6b61261ce'
  client_secret <- 'macfI2HUFIonc386wJmImTKaKhscuhUc873Pxl4j26c='
  token <- getCHToken(uri = token_uri,
    resource = resource,
    client_id = client_id,
    client_secret = client_secret)

  expect_equal(token$status_code, 400)
  expect_equal(token$error, "invalid_resource")
})

test_that("token is NOT returned - bad url", {
  token_uri <- 'https://login.windows.net/805ba517-8a74-4377-81c1-6af998bc4709/oauth2/tokens'
  resource <- 'http://sentgeprodreguiweb.azurewebsites.net'
  client_id <- '298e41fd-251c-4bb2-a22e-41f6b61261ce'
  client_secret <- 'macfI2HUFIonc386wJmImTKaKhscuhUc873Pxl4j26c='
  token <- getCHToken(uri = token_uri,
    resource = resource,
    client_id = client_id,
    client_secret = client_secret)

  expect_equal(token$status_code, 404)

})

