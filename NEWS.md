# countries 1.2.0
* Added function *check_countries_api* to check whether connection to Countries REST API is working.
* Updated country reference table to improve matching of former Yugoslavian countries.
* Improved messages provided by *country_name* and *match_table* and added argument *na_fill* to replace potential NAs with the original country name. 
* Updated vignettes


# countries 1.1.2
* Improving handling of API connection errors in function *country_info* and *list_fields*  
* Fixing bug in *quick_map* occurring when *plot_col* is identical to one of the columns in the table *countries::world*

# countries 1.1.1
* Added function *list_countries* to obtain a list of countries
* Added function *random_countries* to obtain random country names
* Corrected small nomenclature mistake in the country reference list
* Added two new palettes to *palettes_countries*
* Updated vignettes

# countries 1.0.5
* Patch to address change in behaviour of function *is.atomic*

# countries 1.0.4
* Fixed bug in *auto_merge* that was preventing column names to be passed in *by* order for regular expressions.

# countries 1.0.3
* Fixed hyperlinks in for resubmission
* Eliminated references to unexported objects in documentation
* Package accepted on CRAN!!

# countries 1.0
* First CRAN submission

# countries 0.3

* Added function *country_info()* to download information about countries (e.g. capital city, currency, neighbouring countries, etc.)
* Added function *list_fields()* returning a list of accepted fields for *country_info()*
* Added function *auto_merge()* to quickly join multiple dataset and automatically deal with country names and time columns
* Added function *auto_melt()* to automatically pivot tables containing country names and years in their headers
* Added function *quick_map()* to easily plot coloured country maps
* Added function *palettes_countries()* providing access to the package's colour themes
* Added vignette on plotting chloropleth maps with *quick_map* and merging country data with *auto_merge()* 
* Bug fixes

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
* Function *Mode()* returns the statistical mode of a vectors (most frequent observation)
* Dataset *country_reference_list* containing a country names in different languages and naming conventions
* Added vignettes and documentation
* Created website for the package
