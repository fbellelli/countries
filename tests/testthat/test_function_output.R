#MODE ----------------------------------------
test_that("Mode output has the correct format", {
  expect_equal(is.numeric(countries::Mode(c(NA,1,1,2))), TRUE)
  expect_equal(is.character(countries::Mode(c(NA,"prova"))), TRUE)
  expect_equal(is.character(countries::Mode(c(1,2,1,NA,"prova"))), TRUE)
  expect_equal(length(countries::Mode(c(1,2,1,2,3))), 2)
  expect_equal(length(countries::Mode(c(1,2,1,2,3), first_only = TRUE)), 1)
  expect_equal(countries::Mode(c(1,2,1,NA,"prova","prova")), c("1","prova"))
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
  expect_equal(country_name("Taiwan", to = c("ISO3", "UN_en"), verbose = TRUE), data.frame(ISO3 = "TWN", UN_en = NA))
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
  expect_equal(is_country(example, fuzzy_match = TRUE), c(TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,NA))
  expect_equal(is_country(example, fuzzy_match = FALSE), c(TRUE, TRUE, FALSE,FALSE,FALSE,FALSE,NA))
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

# FIND_TIMECOL ------------------------------------------------------

example <- data.frame(a=1950:2049,
                      b=c(NA,sample(1800:2200,99)),
                      c=sample(c("a","b","c"),100, replace=TRUE),
                      d=rep("March 2020",100))
example$e <- as.Date(as.character(example$a), format="%Y")
example$f <- c(NA,as.character(example$d[2:100]))
example$year <- example$a
set.seed(123)
example$year2 <- sample(example$year,100,replace=TRUE)
example$a_fct <- as.factor(example$a)
example$year2_fct <- as.factor(example$year2)
test_that("output from find_timecol seems to correspond to expectations", {
  expect_equal(find_timecol(example), c("d","e","f","year"))
  expect_equal(find_timecol(example, return_index = TRUE), c(4,5,6,7))
  expect_equal(is.null(find_timecol(example[,2:3], allow_NA = TRUE)), FALSE)
  expect_equal(is.null(find_timecol(example[,2:3],allow_NA = FALSE)), TRUE)
  expect_equal(find_timecol(example[,2:3], return_index=TRUE, allow_NA=TRUE), 1)
})



# IS_YEARCOL ---------------------------------------------------
test_that("output from is_yearcol seems to correspond to expectations", {
  expect_equal(is.yearcol(1990:3000), FALSE)
  expect_equal(is.yearcol(1990:3000, limit=c(1800,3000)), TRUE)
  expect_equal(is.yearcol(c("a",2)), FALSE)
  expect_equal(is.yearcol(c(NA,2000), allow_NA=FALSE), FALSE)
  expect_equal(is.yearcol(c(NA,2000), allow_NA=TRUE), TRUE)
  expect_equal(is.yearcol(c(1998,2000,2002), regularity = TRUE), TRUE)
  expect_equal(is.yearcol(c(1998,2000,2002,2003), regularity = TRUE), FALSE)
})

# IS_DATE ------------------------------------------------------
test_that("output from is_date seems to correspond to expectations", {
  expect_equal(is_date(NA), FALSE)
  expect_equal(is_date(NA, c("%Y",NA)), FALSE)
  expect_equal(length(is_date(c("2020-01-01","test",2020,"March 2030"))), 4)
  expect_equal(is.logical(is_date(c("2020-01-01","test",2020,"March 2030",NA))), TRUE)
  expect_equal(is_date(c("2020-01-01","test",2020,"March 2030")), c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(is_date(c("2020-01-01","test",2020,"March 2030"), formats = "%Y"), c(TRUE, FALSE, TRUE, FALSE))
})

# IS_KEYCOL ------------------------------------------------------
example <- data.frame(a=c(1:5,1:5),b=sample(c("a","b","c"),10, replace=TRUE), c=as.factor(c(rep("a",5),rep("b",5))))
example1 <- example
example1$c[2] <- NA
test_that("output from is_key seems to correspond to expectations", {
  expect_equal(is.logical(is_keycol(example,c("a"))), TRUE)
  expect_equal(length(is_keycol(example,c("a"))), 1)
  expect_equal(is_keycol(example,c("a")), FALSE)
  expect_equal(is_keycol(example,c("a","c")), TRUE)
  expect_equal(is_keycol(example1,c("a","c"),allow_NA = FALSE, verbose=FALSE), FALSE)
  expect_equal(is_keycol(example1,c("a","c"),allow_NA = TRUE, verbose=FALSE), TRUE)
})

# FIND_KEYCOL -------------------------------------------------------
example <-data.frame(nation=c(rep(c("FRA","ALB","JOR"),3),"GBR"),
                     year=c(rep(2000,3),rep(2005,3),rep(2010,3),NA),
                     var=runif(10))
test_that("output from find_keycol seems to correspond to expectations", {
  expect_equal(length(find_keycol(example))<=ncol(example), TRUE)
  expect_equal(is.null(find_keycol(example)), TRUE)
  expect_equal(is.numeric(find_keycol(example, return_index = TRUE)), TRUE)
  expect_equal(is.character(find_keycol(example, allow_NA=TRUE)), TRUE)
  expect_equal(all(find_keycol(example, allow_NA = TRUE)== c("nation","year")), TRUE)
  expect_equal(is.null(find_keycol(example, search_only = 1:2)), TRUE)
  expect_equal(find_keycol(example, search_only = 1:2, allow_NA = TRUE), c("country"="nation","time"="year"))
  expect_equal(is.null(find_keycol(example, search_only = "nation", allow_NA = TRUE)), TRUE)
})


# WHICH_ -------------------------------------------------------

example <- c(1,2,3,3,4,NA)
test_that("output from which_min, which_max and which_mode are as expected", {
  expect_equal(which_min(example), 1)
  expect_equal(which_max(example), 5)
  expect_equal(which_mode(example), c(3,4))
  expect_equal(which_mode(example, first_only = TRUE), 3)
})

# check_wide_format --------------------------------------------

example <- data.frame(a=1,FRA=2,USA=3,osadk=4,KOR=5)
test_that("output from check_wide_format are as expected", {
  expect_equal(is.null(check_wide_format(example)), TRUE)
  expect_equal(is.null(check_wide_format(example, adjacency = FALSE)), FALSE)
  expect_equal(ncol(check_wide_format(example, adjacency = FALSE)), 3)
  expect_equal(nrow(check_wide_format(example, adjacency = FALSE)), 3)
  expect_equal(check_wide_format(example, adjacency = FALSE)[,"col_indx"], c(2,3,5))
})

# HAS.INVALID.MULTIBYTE.STRING --------------------------------------------

example1 <- c("Restaurant","Caf\xe9","Bar")
example2 <- c("Dog", "cat")
test_that("output from has.invalid.multibyte.string are as expected", {
  expect_equal(has.invalid.multibyte.string(example1), TRUE)
  expect_equal(has.invalid.multibyte.string(example2), FALSE)
  expect_equal(length(has.invalid.multibyte.string(example2, return.elements = TRUE)), 2)
  expect_equal(has.invalid.multibyte.string(example1, return.elements = TRUE), c(FALSE, TRUE, FALSE))
})




# AUTO_MERGE --------------------------------------------

example <- data.frame(HS = 85:89, freq = sample(1:3, size = 5, replace = TRUE))
example1 <- data.frame(Date = c("01.01.2019", "01.02.2019", "01.03.2019"), Japan = 1:3, Norway = 2:4, Germany = 3:5, US = 4:6)
example4 <- data.frame(chapter = 82:87, freq = runif(6))
example5 <- data.frame(HS = c(T,F,T,F,T), number = runif(5))
example3 <- data.frame(ISIC = 1000:1010, freq = sample(1:3, size = 11, replace = TRUE))
example2 <- data.frame(France = 1:10, Italy = 11:20, US = 6:15)
example6 <- data.frame(ID = 1:10, France1992 = runif(10), France1993 = runif(10), France1994 = runif(10), USA1992 = runif(10), USA1993 = runif(10), USA1994 = runif(10))
example8 <- data.frame(ID = 1001:1010, France1992 = runif(10), France1993 = runif(10), France1994 = runif(10))
example7 <-data.frame(HS = paste("Prova", 1:10), freq = runif(10))
example9 <- data.frame(a = 1:10, b = 2:11)
test_that("output from auto_merge is as expected", {
  expect_equal(auto_merge(example, example3, merging_info = T)$info_merged_columns$freq, c("freq","freq"))
  expect_equal(nrow(auto_merge(example, example3)), 17)
  expect_equal(ncol(auto_merge(example, example3)), 3)
  expect_equal(sum(is.na(auto_merge(example, example3)$freq)), 0)
  expect_equal(auto_merge(example, example3, country_to = "UN_en"), auto_merge(example, example3))
  expect_equal(sum(is.na(auto_merge(example, example2)$freq)), 30)
  expect_equal(auto_merge(example, example2, verbose = FALSE), auto_merge(example, example2))
  expect_equal(length(auto_merge(example, example2, merging_info = T)$pivoted_columns), 1)
  expect_equal(auto_merge(example, example2, merging_info = T)$pivoted_columns$`Table 2`, c("France","Italy","US"))
  expect_equal(nrow(auto_merge(example, example2, merging_info = T)$info_country_names), 3)
  expect_equal(auto_merge(list(example, example2)), auto_merge(example, example2))
  expect_equal(ncol(auto_merge(example, example5, example7, merging_info = T)$info_merged_columns), 2)
  expect_equal(auto_merge(example, example5, example7, merging_info = T)$info_merged_columns$HS, rep("HS",3))
  expect_equal(nrow(auto_merge(example5, example7, inner_join = T)), 0)
  expect_equal(nrow(auto_merge(example5, example7, inner_join = F)), 15)
  expect_equal(length(auto_merge(example2, example8, merging_info = T)$pivoted_columns), 2)
  expect_equal(nrow(auto_merge(example, example2, inner_join = T)), 0)
  expect_equal(nrow(auto_merge(example1, example2)), 59)
  expect_equal(nrow(auto_merge(example1, example2, auto_melt = FALSE)), 13)
  expect_equal(ncol(auto_merge(example1, example2, merging_info = T, auto_melt = FALSE)$info_merged_columns), 1)

})



# AUTO_MELT --------------------------------------------


test_that("output from auto_melt are as expected", {
  expect_equal(is.data.frame(auto_melt(example2)), TRUE)
  expect_equal(is.list(auto_melt(example2, pivoting_info = TRUE)), TRUE)
  expect_equal(length(auto_melt(example2, pivoting_info = TRUE)), 2)
  expect_equal(colnames(auto_melt(example2)), c("pivoted_colnames", "pivoted_data"))
  expect_equal(colnames(auto_melt(example2, names_to = "COUNTRY", values_to = "DATA")), c("COUNTRY", "DATA"))
  expect_equal(length(auto_melt(example2, pivoting_info = TRUE)$pivoted_cols), 3)
  expect_equal(auto_melt(example2, pivoting_info = TRUE)$pivoted_cols, c("France", "Italy", "US"))
  expect_equal(nrow(auto_melt(example2)), 30)
  expect_equal(auto_melt(example), example)
  expect_equal(is.null(auto_melt(example, pivoting_info = TRUE)$pivoted_cols), TRUE)
  expect_equal(auto_melt(example6), example6)
  expect_equal(auto_melt(example8, pivoting_info = TRUE)$pivoted_cols, c("France1992", "France1993", "France1994"))
  expect_equal(colnames(auto_melt(example8)), c("pivoted_colnames", "ID", "pivoted_data", "year_pivoted_colnames"))
  expect_equal(nrow(auto_melt(example8)), 30)
  expect_equal(auto_melt(example8)$year_pivoted_colnames, rep(1992:1994, 10))
})


# LIST_FIELDS -------------------------------


test_that("output from list_fields() are as expected", {
  if (curl::has_internet()){
    expect_equal(length(list_fields())>0, TRUE)
  }
})


# COUNTRY_INFO ------------------------------

test_that("output from country_info() are as expected", {
  if (curl::has_internet()){
    expect_equal(is.data.frame(country_info("USA", "capital")), TRUE)
    expect_equal(dim(country_info("Belgium", "languages")), c(1, 2))
    expect_equal(dim(country_info("Belgium", "languages", collapse = FALSE)), c(1, 4))
    expect_equal(dim(country_info(c("Bel", "Taiwan"), c("capital", "unMember"))), c(2, 3))
    expect_equal(dim(country_info(c("Bel", "China"), c("capital", "unMember"), match_info = TRUE)), c(2, 5))
    expect_equal(all(c("matched_country", "is_country") %in% colnames(country_info(c("Bel", "China"), c("capital", "unMember"), match_info = TRUE))), TRUE)
    expect_equal(unlist(country_info(c("France", "Australia", "Italy"), "capital")$capital), c("Paris", "Canberra", "Rome"))
    expect_equal(nrow(country_info(fields = "capital"))>190, TRUE)
    expect_equal(ncol(country_info("USA"))>50, TRUE)
  }
})



# PICK_COLOUR --------------------------------

test_that("output from pick_colours() are as expected",{
  expect_equal(all(is.numeric(pick_colours(3, 5))), TRUE)
  expect_equal(length(pick_colours(3, 5)), 3)
  expect_equal(suppressWarnings(pick_colours(5, 3)), c(1,2,3,1,1))
  expect_equal(pick_colours(3, 3), 1:3)
  expect_equal(pick_colours(3, 5), c(1, 3, 5))
})


# PALETTES_COUNTRIES -------------------------

test_that("output from palettes_countries() are as expected",{
  expect_equal(all(is.character(palettes_countries(5, 1))), TRUE)
  expect_equal(all(is.character(palettes_countries(5, 2))), TRUE)
  expect_equal(length(palettes_countries(3, 5)), 3)
  expect_equal(palettes_countries(2, 2), c("grey85", "grey15"))
  expect_equal(all(palettes_countries(5, 1, reverse = TRUE) == rev(palettes_countries(5, 1, reverse = FALSE))), TRUE)
  expect_equal(all(suppressWarnings(palettes_countries(10, 1))[c(1,7)] == "#cb997e"), TRUE)
  expect_equal(is.null(suppressWarnings(palettes_countries(5,100))), TRUE)
})

# THEME_COUNTRIES ----------------------------

test_that("output from palettes_countries() are as expected",{
  expect_equal(is.list(themes_countries(1)), TRUE)
  expect_equal(is.null(themes_countries(0)), TRUE)
  expect_equal(all(c("axis.text", "legend.background", "legend.position") %in% names(themes_countries(2))), TRUE)
})


# QUICK_MAP ----------------------------

example <- data.frame(country = c("FRA", "Italy", "UK", "India"), price = c(250, 10, 1000, 100), test = c("a", "b", "c", "d"))
example2 <- data.frame(country = c("FRA", "Italy", "UK", "India", "CHINA", "Russia", "US"), price = c(250, 10, 1000, 100, 5000, 1e5, 2e5))
test_that("output from quick_map() are as expected",{
  expect_equal(class(quick_map(example, "price")), c("gg", "ggplot"))
  expect_equal(class(quick_map(example, "price", theme = 2)), c("gg", "ggplot"))
  expect_equal(class(quick_map(example, "price", theme = "Candy")), c("gg", "ggplot"))
  expect_equal(class(quick_map(example, "price", verbose = FALSE)), c("gg", "ggplot"))
  expect_equal(class(quick_map(example, "price", width_plot = 10)), c("gg", "ggplot"))
  expect_equal(class(quick_map(example, "price", reverse_palette = TRUE)), c("gg", "ggplot"))
  expect_equal(class(quick_map(example, "price", col_border = "white")), c("gg", "ggplot"))
  expect_equal(class(quick_map(example2, "price", col_breaks = c(1, 5, 150,500, 1e6))), c("gg", "ggplot"))
  expect_equal(class(quick_map(example, "test",col_na = "black")), c("gg", "ggplot"))
})

# LIST_COUNTRIES ----------------------------

test_that("output from list_countries() are as expected",{
  expect_equal(length(list_countries("UN_en")), 248)
  expect_equal(is.character(list_countries("UN_en")), TRUE)
  expect_equal(list_countries("ISO3")[1], "AFG")
  expect_equal(list_countries()[1], "Afghanistan")
})


# RANDOM_COUNTRIES ----------------------------

test_that("output from list_countries() are as expected",{
  expect_equal(length(random_countries(500, replace = T)), 500)
  expect_equal(random_countries(3, seed = 2), c("Gabon", "South Africa", "Serbia"))
})
