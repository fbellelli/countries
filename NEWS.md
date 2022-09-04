# countries 0.3

# countries 0.2

* Added function *is_country()* to test whether a string is a country name or related to list of countries names
* Added function *is_date()* to test whether values are dates
* Added function *is_keycol()* to test whether a set of columns could be the keys of a table
* Added function *find_countrycol()* to automate search of columns containing country names in data frames
* Added function *find_timecol()* to automate search of date and/or year columns in data frames
* Added function *find_keycol()* to automate search of table keys in data frames
* Added functions *which_min()*, *which_max()* and *which_mode()* returning all the positions of vector's minimum, maximum and mode values.
* Speed improvements for the functions *country_name()* and *match_table()*
* Bug fixes
* Added vignette on data structure



# countries 0.1

* Function *country_name()* for converting and translating country names based on a fuzzy matching approach
* Function *match_table()* to easily construct conversion tables with the help of fuzzy matching algorithm
* Function *mode()* returns the mode of a vectors (most frequent observation)
* Dataset *country_reference_list* containing a country names in different languages and naming conventions
* Added vignettes and documentation
* Created website for the package
