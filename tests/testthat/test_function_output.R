#GET_MODE ----------------------------------------
test_that("get_mode output has the correct format", {
  expect_equal(is.numeric(get_mode(c(NA,1,1,2))), TRUE)
  expect_equal(is.character(get_mode(c(NA,"prova"))), TRUE)
  expect_equal(is.character(get_mode(c(1,2,1,NA,"prova"))), TRUE)
  expect_equal(length(get_mode(c(1,2,1,2,3))), 2)
  expect_equal(length(get_mode(c(1,2,1,2,3), first_only = TRUE)), 1)
  expect_equal(get_mode(c(1,2,1,NA,"prova","prova")), c("1","prova"))
})


#MATCH_TABLE AND COUNTRY_NAME ----------------------
example1 <- c("United States","MArruecos","Brun3i")

test_that("match_table output has the expected format", {
  expect_equal(is.data.frame(match_table(example1, to="ISO3")), TRUE)
  expect_equal(is.list(country_name(example1, simplify = FALSE)), TRUE)
  expect_equal(length(match_table(example1, simplify = FALSE)), 5)
  expect_equal(ncol(match_table(example1, to=c("ISO3","ISO2","name_en"))), 4)
  expect_equal(ncol(match_table(example1, to=c("ISO3","ISO2","name_en"), matching_info = TRUE)), 7)
  expect_equal(is.logical(match_table(example1, to=c("ISO3","ISO2","name_en"), matching_info = TRUE)$exact_match), TRUE)
  expect_equal(is.numeric(match_table(example1, to=c("ISO3","ISO2","name_en"), matching_info = TRUE)$dist), TRUE)
})


test_that("country_name output has the correct format", {
  expect_equal(is.data.frame(country_name(example1, to=c("ISO3","ISO2"))), TRUE)
  expect_equal(is.vector(country_name(example1, to=c("ISO3"))), TRUE)
  expect_equal(is.list(country_name(example1, simplify = FALSE)), TRUE)
  expect_equal(length(country_name(example1, simplify = FALSE)), 5)
  expect_equal(length(country_name(example1, to="ISO3")), length(example1))
  expect_equal(ncol(country_name(example1, to=c("ISO3","ISO2","name_en"))), 3)
  expect_equal(all(nchar(country_name(example1, to="ISO3")==3)), TRUE)
})

test_that("country_name seems to correctly identify countries", {
  expect_equal(country_name(example1, to="ISO3"), c("USA","MAR","BRN"))
})

test_that("country_name seems to correctly deal with NA values", {
  expect_equal(country_name(c(example1,NA), to="ISO3"), c("USA","MAR","BRN",NA))
  expect_equal(country_name(c(NA,NA,NA), to="ISO3"), c(NA,NA,NA))
})


# IS_COUNTRY ---------------------------------------------------
example <- c("ITA","Estados Unidos","Estado Unidos","bungalow","dog",542,NA)
test_that("output from is_country seems to correspond to expectations", {
  expect_equal(length(is_country(example)), length(example))
  expect_equal(is_country(example, fuzzy_margin = 0.1), c(TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,NA))
  expect_equal(is_country(example, fuzzy_margin = 0), c(TRUE, TRUE, FALSE,FALSE,FALSE,FALSE,NA))
  expect_equal(is_country(x=c("Ceylon","LKA","Indonesia","Inde"), check_for=c("India","Sri Lanka")), c(TRUE,TRUE,FALSE,TRUE))
})


# FIND_COUNTRYCOL ---------------------------------------------------
example <- data.frame(a=c("Brésil","Tonga","FRA"),b=c(1,2,3))
test_that("output from is_country seems to correspond to expectations", {
  expect_equal(find_countrycol(example), "a")
  expect_equal(find_countrycol(example, TRUE), 1)
  expect_equal(length(find_countrycol(data.frame(a=c("Bra","Bro","Bru"),b=c(1,2,3)), TRUE)), 0)
  expect_equal(length(find_countrycol(data.frame(a=c("Bra","Bro","Bru"),b=c(1,2,3)))), 0)
  expect_equal(find_countrycol(data.frame(a=c("BRA","ITA"), b=c(2,3),c(1,NA),d=c("India","Paraguay"))), c("a","d"))
})


# IS_YEARCOL ---------------------------------------------------
test_that("output from is_yearcol seems to correspond to expectations", {
  expect_equal(is.yearcol(1990:3000), FALSE)
  expect_equal(is.yearcol(1990:3000, limit=c(1800,3000)), TRUE)
  expect_equal(is.yearcol(c("a",2)), FALSE)
  expect_equal(is.yearcol(c(NA,2000), allow_NA=FALSE), FALSE)
  expect_equal(is.yearcol(c(NA,2000), allow_NA=TRUE), FALSE)
  expect_equal(is.yearcol(c(1998,2000,2002), regularity = TRUE), TRUE)
  expect_equal(is.yearcol(c(1998,2000,2002,2003), regularity = TRUE), FALSE)
})

# IS_DATE ------------------------------------------------------
test_that("output from is_date seems to correspond to expectations", {
  expects_equal(is_date(NA), FALSE)
  expects_equal(is_date(NA, c("%Y",NA)), FALSE)
  expects_equal(length(is_date(c("2020-01-01","test",2020,"March 2030"))), 4)
  expects_equal(is.logical(is_date(c("2020-01-01","test",2020,"March 2030",NA))), TRUE)
  expect_equal(is_date(c("2020-01-01","test",2020,"March 2030")), c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(is_date(c("2020-01-01","test",2020,"March 2030"), allowed_formats = "%Y"), c(TRUE, FALSE, TRUE, FALSE))
})
