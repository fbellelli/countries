# Easy joining with auto_merge()

The function
[`auto_merge()`](https://fbellelli.github.io/countries/reference/auto_merge.md)
allows easy merging of data tables. This function is optimised to work
with the most common country data formats. Some of the advantages over
other merging functions are that: **1)** it allows merging of multiple
tables at the same time, **2)** it can automatically detect columns to
merge, **3)** it automatically handles different country naming
conventions and date formats, **4)** it automatically pivots country
names or years in table headers.

In this vignette we will go through some examples showcasing these
functionalities.

## Example data

Let’s first start by creating some example data to merge. All the tables
created below contain data relating to countries, but their format and
keys are different. `tab1` is a classic cross-sectional data, providing
one data point per country. `tab2` and `tab3` are typical panel (or
longitudinal) dataset, providing data for countries over different time
periods. The difference between the two is the structure of the table
(`tab2` is in a long format, while `tab3` is in a wide format) and the
time frequency of observations (annual vs monthly). Finally, `tab4` and
`tab5` provide sectoral data for different countries.

``` r

# FIFA female world cups won
tab1 <- data.frame(country = c("UNITED STATES", "GERMANY", "NORWAY", "JAPAN"),
                   FIFA_cups = c(4, 2, 1, 1))

# nominal yearly GDP estimates from IMF (World economic outlook) 
tab2 <- data.frame(nation = c("DEU", "JPN", "USA", "DEU", "JPN", "USA"),
                   year = c(rep(1980,3), rep(2019,3)), 
                   GDP = c(854, 1128, 2857, 3888, 5118, 21380), 
                   unit = "billion current USD")

# fictitious monthly time series
tab3 <- data.frame(Date = c("01.01.2019", "01.02.2019", "01.03.2019"), 
                   Japan = 1:3, 
                   Norway = 2:4, 
                   Germany = 3:5, 
                   US = 4:6)

# fictitious sectoral data
tab4 <- data.frame(year = 2019,
                   country = "US", 
                   industry = c("Agriculture", "Mining", "Manifacture", "Energy"),
                   freq = runif(1:4))

# more fictitious sectoral data
tab5 <- data.frame(country = c("Estados Unidos", "Japon", "Estados Unidos", "Japon"), 
                   sector = c("Agriculture", "Agriculture","Energy", "Energy"),
                   x = c(T, F, T, F))
```

## Quick start

Using
[`auto_merge()`](https://fbellelli.github.io/countries/reference/auto_merge.md)
is really easy. All the users needs to do is list the tables to merge.
In most cases, the function will be able take care of everything else.

``` r

auto_merge(tab1, tab2, tab3, tab4, tab5)
#> Identifying columns to merge
#> Table 3 - countries detected in column names, pivoting columns: Japan, Norway, Germany, US
#> Data type for variable time differ across tables - converting to character
#> Converting country names
#> Checking time columns
#> The following columns are being merged:
#> 
#> =======  =======================  ====  ========
#> \        country                  time  industry
#> =======  =======================  ====  ========
#> Table 1  country                                
#> Table 2  nation                   year          
#> Table 3  Table3_pivoted_colnames  Date          
#> Table 4  country                  year  industry
#> Table 5  country                        sector  
#> =======  =======================  ====  ========
#>                                               Performing merge: 1/4                                               Performing merge: 2/4                                               Performing merge: 3/4                                               Performing merge: 4/4                                               Merge complete
#> (Set merging_info to TRUE to save merging details)
#>    country FIFA_cups       time   GDP                unit Table3_pivoted_values
#> 1      USA         4 1980-01-01  2857 billion current USD                    NA
#> 2      USA         4 2019-01-01 21380 billion current USD                     4
#> 3      USA         4 2019-01-01 21380 billion current USD                     4
#> 4      USA         4 2019-01-01 21380 billion current USD                     4
#> 5      USA         4 2019-01-01 21380 billion current USD                     4
#> 6      DEU         2 1980-01-01   854 billion current USD                    NA
#> 7      DEU         2 2019-01-01  3888 billion current USD                     3
#> 8      NOR         1       <NA>    NA                <NA>                    NA
#> 9      JPN         1 1980-01-01  1128 billion current USD                    NA
#> 10     JPN         1 2019-01-01  5118 billion current USD                     1
#> 11     NOR        NA 2019-01-01    NA                <NA>                     2
#> 12     JPN        NA 2019-02-01    NA                <NA>                     2
#> 13     NOR        NA 2019-02-01    NA                <NA>                     3
#> 14     DEU        NA 2019-02-01    NA                <NA>                     4
#> 15     USA        NA 2019-02-01    NA                <NA>                     5
#> 16     JPN        NA 2019-03-01    NA                <NA>                     3
#> 17     NOR        NA 2019-03-01    NA                <NA>                     4
#> 18     DEU        NA 2019-03-01    NA                <NA>                     5
#> 19     USA        NA 2019-03-01    NA                <NA>                     6
#> 20     JPN        NA       <NA>    NA                <NA>                    NA
#> 21     JPN        NA       <NA>    NA                <NA>                    NA
#>       industry       freq     x
#> 1         <NA>         NA    NA
#> 2  Agriculture 0.08075014  TRUE
#> 3       Mining 0.83433304    NA
#> 4  Manifacture 0.60076089    NA
#> 5       Energy 0.15720844  TRUE
#> 6         <NA>         NA    NA
#> 7         <NA>         NA    NA
#> 8         <NA>         NA    NA
#> 9         <NA>         NA    NA
#> 10        <NA>         NA    NA
#> 11        <NA>         NA    NA
#> 12        <NA>         NA    NA
#> 13        <NA>         NA    NA
#> 14        <NA>         NA    NA
#> 15        <NA>         NA    NA
#> 16        <NA>         NA    NA
#> 17        <NA>         NA    NA
#> 18        <NA>         NA    NA
#> 19        <NA>         NA    NA
#> 20 Agriculture         NA FALSE
#> 21      Energy         NA FALSE
```

The function will print to the console some information on the merger
status. The most important piece of information is the table summarising
all the columns that are being merged together (see reproduction below).
Each row in this table corresponds to one of the input data tables, each
column to a key that is used for merging. The names in this table are
the column names from the original data tables. The names in the header
of the summary table is the name of the merged columns in the final
output table. For instance, the summary table is telling us that the
columns “year”, “Date” and “year”, respectively from the third, fourth
and fifth input table were merged together into a column called “time”.

|         | country                 | time | industry |
|:--------|:------------------------|:-----|:---------|
| Table 1 | country                 | NA   | NA       |
| Table 2 | nation                  | year | NA       |
| Table 3 | Table3_pivoted_colnames | Date | NA       |
| Table 4 | country                 | year | industry |
| Table 5 | country                 | NA   | sector   |

It is always a good idea to inspect the summary table and make sure that
the correct columns were merged. In case the function did not merge the
right columns, the user can manually provide a merging order. For more
information, you can may refer to the section in this article *Manual
merging orders*.

## The function in details

### Dynamic inputs

The function also accept a lists of tables as inputs. This could be
useful when passing a dynamic set of tables to the function.

``` r

data <- list(tab1, tab2, tab3, tab4, tab5)

# the following two commands are equivalent
auto_merge(tab1, tab2, tab3, tab4, tab5)
auto_merge(data)
```

### Country names

[`auto_merge()`](https://fbellelli.github.io/countries/reference/auto_merge.md)
will automatically recognise country names and convert them to the same
nomenclature for merging. For example, the country names in `tab1` and
`tab2` have a different format, but with
[`auto_merge()`](https://fbellelli.github.io/countries/reference/auto_merge.md)
there is no need of converting the names beforehand. The function will
perform this automatically. This feature is based on the function
[`country_name()`](https://fbellelli.github.io/countries/reference/country_name.md),
which is described in [this
article](https://fbellelli.github.io/countries/articles/dealing_with_names.html).

It is possible to change the destination nomenclature with the argument
`country_to`. By default, all country names will be converted to ISO
3166-1 alpha-3 standard. Any of the nomenclatures supported by
[`country_name()`](https://fbellelli.github.io/countries/reference/country_name.md)
can be requested.

``` r

# changing the country names to UN spanish official name
auto_merge(tab1, tab2, country_to = "UN_es", verbose = FALSE)
#>                     country FIFA_cups time   GDP                unit
#> 1 Estados Unidos de América         4 1980  2857 billion current USD
#> 2 Estados Unidos de América         4 2019 21380 billion current USD
#> 3                  Alemania         2 1980   854 billion current USD
#> 4                  Alemania         2 2019  3888 billion current USD
#> 5                   Noruega         1   NA    NA                <NA>
#> 6                     Japón         1 1980  1128 billion current USD
#> 7                     Japón         1 2019  5118 billion current USD
```

### Pivotting of countries and years in table headers

When country names or years are found in the column names, the function
will automatically transform the table from a wide to a [long
format](https://en.wikipedia.org/wiki/Wide_and_narrow_data) by pivoting
the country/year columns. At least 3 country names or years need to be
present in the table header to trigger the automatic pivoting. Pivoting
can be turned off by setting `auto_melt = FALSE`. For additional
information, refer to the documentation of the function
[`auto_melt()`](https://fbellelli.github.io/countries/reference/auto_melt.md).

In the first example below, the `auto_melt` option is turned on (this is
the default behaviour). The function detects country names in the header
of the table and proceeds to pivoting the corresponding columns. Pivoted
columns will have the name `"Table?_pivoted_colnames"` and the data from
these columns will be stored in a column `"Table?_pivoted_data"` (here ?
stands for the number of the table in the provided input). A message is
printed on the console to inform the user that columns have been
pivoted.

Notice that in the second example no pivoting is performed. As a result,
no shared key was found for merging and the two tables were just stacked
in the final output.

``` r

# with auto_melt
auto_merge(tab1, tab3)
#> Identifying columns to merge
#> Table 2 - countries detected in column names, pivoting columns: Japan, Norway, Germany, US
#> Converting country names
#> Checking time columns
#> The following columns are being merged:
#> 
#> =======  =======================  ====
#> \        country                  time
#> =======  =======================  ====
#> Table 1  country                      
#> Table 2  Table2_pivoted_colnames  Date
#> =======  =======================  ====
#>                                               Performing merge: 1/1                                               Merge complete
#> (Set merging_info to TRUE to save merging details)
#>    country FIFA_cups       time Table2_pivoted_values
#> 1      USA         4 2019-01-01                     4
#> 2      USA         4 2019-02-01                     5
#> 3      USA         4 2019-03-01                     6
#> 4      DEU         2 2019-01-01                     3
#> 5      DEU         2 2019-02-01                     4
#> 6      DEU         2 2019-03-01                     5
#> 7      NOR         1 2019-01-01                     2
#> 8      NOR         1 2019-02-01                     3
#> 9      NOR         1 2019-03-01                     4
#> 10     JPN         1 2019-01-01                     1
#> 11     JPN         1 2019-02-01                     2
#> 12     JPN         1 2019-03-01                     3

# without auto_melt
auto_merge(tab1, tab3, auto_melt = FALSE)
#> Identifying columns to merge
#> Converting country names
#> Checking time columns
#> The following columns are being merged:
#> 
#> =======  =======  ====
#> \        country  time
#> =======  =======  ====
#> Table 1  country      
#> Table 2           Date
#> =======  =======  ====
#>                                               Performing merge: 1/1                                               Merge complete
#> (Set merging_info to TRUE to save merging details)
#>   country FIFA_cups       time Japan Norway Germany US
#> 1     USA         4       <NA>    NA     NA      NA NA
#> 2     DEU         2       <NA>    NA     NA      NA NA
#> 3     NOR         1       <NA>    NA     NA      NA NA
#> 4     JPN         1       <NA>    NA     NA      NA NA
#> 5    <NA>        NA 2019-01-01     1      2       3  4
#> 6    <NA>        NA 2019-02-01     2      3       4  5
#> 7    <NA>        NA 2019-03-01     3      4       5  6
```

### Inner vs full join

By default, the function will return a **full join** of all the tables.
This means that all the table-key combinations are conserved in the
output table. If no information is available for a variable, `NA` will
be used to fill the output table. By contrast, an **inner join** only
keeps the combination of keys that are available across all tables.
[Click here for more information on join
types](https://en.wikipedia.org/wiki/Join_(SQL)).

An inner join can be requested with the argument `inner_join`.

``` r

auto_merge(tab1, tab2, tab3, tab4, tab5, inner_join = TRUE)
#> Identifying columns to merge
#> Table 3 - countries detected in column names, pivoting columns: Japan, Norway, Germany, US
#> Data type for variable time differ across tables - converting to character
#> Converting country names
#> Checking time columns
#> The following columns are being merged:
#> 
#> =======  =======================  ====  ========
#> \        country                  time  industry
#> =======  =======================  ====  ========
#> Table 1  country                                
#> Table 2  nation                   year          
#> Table 3  Table3_pivoted_colnames  Date          
#> Table 4  country                  year  industry
#> Table 5  country                        sector  
#> =======  =======================  ====  ========
#>                                               Performing merge: 1/4                                               Performing merge: 2/4                                               Performing merge: 3/4                                               Performing merge: 4/4                                               Merge complete
#> (Set merging_info to TRUE to save merging details)
#>   country FIFA_cups       time   GDP                unit Table3_pivoted_values
#> 1     USA         4 2019-01-01 21380 billion current USD                     4
#> 2     USA         4 2019-01-01 21380 billion current USD                     4
#>      industry       freq    x
#> 1 Agriculture 0.08075014 TRUE
#> 2      Energy 0.15720844 TRUE
```

### Turning off messages

The function’s messages tend to be very wordy. If you wish to suppress
messages printed to the console you can just turn them off with the
option `verbose = FALSE`.

``` r

auto_merge(tab1, tab2, tab3, tab4, tab5, verbose = F)
```

### Saving information on merging operations

If the argument `merging_info` is set to `TRUE`, the function will
return a list object containing additional information on the merger.
The following objects are saved in the output:

- `merged_table`: This is the merged data table (i.e. the table you get
  when `merging_info = FALSE`).
- `info_merged_columns`: This is the summary table that is printed on
  the console. It gives an overview of all the columns that are merged
  together. It is always a good idea to inspect this table and make sure
  that the correct columns were merged.
- `info_country_names`: If any country column is found, here the user
  can find a table summarising the conversion of all the country names
  to the destination nomenclature. This can be used to check if
  countries were correctly identified.
- `info_time_formats`: similar to the above, this table summarises any
  conversion that was made to date columns. It can be used to check how
  time columns were handled by the function.
- `pivotted_columns`: When country names or years are found in the
  header of the table,
  [`auto_merge()`](https://fbellelli.github.io/countries/reference/auto_merge.md)
  automatically “pivots” the table. Pivoting transforms the table into a
  [long format](https://en.wikipedia.org/wiki/Wide_and_narrow_data). The
  name of all columns that have been pivotted is saved in a list format
  here.
- `call`: This is a list recapitulating the user’s call.

## Manual merging orders

The automatic merging process starts by first identifying the key of
each table, i.e. a set of variables identifying the entries in the
table. This process is optimised for common formats of country data. The
function will then try to match key columns across tables based on their
values. Columns containing country names and time information are
identified and are processed to take into account different
nomenclatures and time formats. This automatic process works for the
most common dataset structures, but it is not foolproof. Therefore, we
always advise to check the columns that are being merged by setting
`verbose = TRUE` and reading the printout. Moreover, this automatic
detection process can increase the overall merging time considerably.
This can be especially long for tables containing many columns or when a
large number of tables is being merged.

The user can pass a merge order with argument `by` to save time during
the merger, or if the detection process did not succeed. Inputs need to
be provided either 1) as a list of column names, or 2) a vector of
regular expressions. Here we will go in more details on the format
requirements.

**A) List of column names**

In case a *list* is passed, there are 3 key requirements regarding the
format.

1.  Each element of the list must be a vector of length equal to the
    number of tables being merged. For instance, if 3 tables are being
    merged, the list needs to contain one or more character vectors of
    length 3. One vector should be provided for each variable to merge
    (in the example below one for countries and one for years).
2.  The vectors should contain the names of columns to be merged in each
    table, `NA` can be inserted for tables that do not contain the
    variable, and names should be ordered in the same order of the input
    tables (i.e. the first column name should be present in the first
    table being merged).
3.  Optionally, the name of the merged columns can be modified by
    assigning a name to the elements of the list. If no name is
    provided, the first column name is retained.

In the example below, we are requesting to merge the columns `"country"`
and `"nation"` from `tab1` and `tab2`, and the columns `"year"` from
`tab2` and `tab4`. Notice that we are inserting `NA` whenever the key is
not present in the table. In addition, we are also changing the name in
the final table to `"COUNTRIES"` and `"YEARS"` by naming the elemnts in
the list.

``` r

# asking to merge country and year columns manually with a list of column names
auto_merge(tab1, tab2, tab4,
           by = list("COUNTRIES" = c("country", "nation", NA),
                     "YEARS" = c(NA, "year", "year")))
#> Identifying columns to merge
#> Converting country names
#> Checking time columns
#> The following columns are being merged:
#> 
#> =======  =========  =====
#> \        COUNTRIES  YEARS
#> =======  =========  =====
#> Table 1  country         
#> Table 2  nation     year 
#> Table 3             year 
#> =======  =========  =====
#>                                               Performing merge: 1/2                                               Performing merge: 2/2                                               Merge complete
#> (Set merging_info to TRUE to save merging details)
#>    COUNTRIES FIFA_cups YEARS   GDP                unit country    industry
#> 1        USA         4  1980  2857 billion current USD    <NA>        <NA>
#> 2        USA         4  2019 21380 billion current USD      US Agriculture
#> 3        USA         4  2019 21380 billion current USD      US      Mining
#> 4        USA         4  2019 21380 billion current USD      US Manifacture
#> 5        USA         4  2019 21380 billion current USD      US      Energy
#> 6        DEU         2  1980   854 billion current USD    <NA>        <NA>
#> 7        DEU         2  2019  3888 billion current USD      US Agriculture
#> 8        DEU         2  2019  3888 billion current USD      US      Mining
#> 9        DEU         2  2019  3888 billion current USD      US Manifacture
#> 10       DEU         2  2019  3888 billion current USD      US      Energy
#> 11       NOR         1    NA    NA                <NA>    <NA>        <NA>
#> 12       JPN         1  1980  1128 billion current USD    <NA>        <NA>
#> 13       JPN         1  2019  5118 billion current USD      US Agriculture
#> 14       JPN         1  2019  5118 billion current USD      US      Mining
#> 15       JPN         1  2019  5118 billion current USD      US Manifacture
#> 16       JPN         1  2019  5118 billion current USD      US      Energy
#>          freq
#> 1          NA
#> 2  0.08075014
#> 3  0.83433304
#> 4  0.60076089
#> 5  0.15720844
#> 6          NA
#> 7  0.08075014
#> 8  0.83433304
#> 9  0.60076089
#> 10 0.15720844
#> 11         NA
#> 12         NA
#> 13 0.08075014
#> 14 0.83433304
#> 15 0.60076089
#> 16 0.15720844
```

**B) Vector of regular expressions**

In case a *vector* is passed, each element in the vector is interpreted
as a regular expression to be used for matching the columns to be
merged. This means that we need to provide one regex for each variable
we want to join. In our example, we would need one regex for identifying
country columns and one for identifying year columns. The function will
scan through the column names and pick the first column matching with
the regex’s pattern. A name can be provided for the variables in the
final table by naming the elements in the vector.

For example, we can achieve exactly the same merger simply with the
following order:
`by = c("COUNTRIES" = "country|nation", "YEARS" = "year")`. Since the
function will pick the first matching column in each table, the user
must be careful that the regex does not match with any other undesired
column in the tables.

``` r

# asking to merge country and year columns manually with a vector of regular expressions
auto_merge(tab1, tab2, tab4,
           by = list("COUNTRIES" = c("country", "nation", NA),
                     "YEARS" = c(NA, "year", "year")))
#> Identifying columns to merge
#> Converting country names
#> Checking time columns
#> The following columns are being merged:
#> 
#> =======  =========  =====
#> \        COUNTRIES  YEARS
#> =======  =========  =====
#> Table 1  country         
#> Table 2  nation     year 
#> Table 3             year 
#> =======  =========  =====
#>                                               Performing merge: 1/2                                               Performing merge: 2/2                                               Merge complete
#> (Set merging_info to TRUE to save merging details)
#>    COUNTRIES FIFA_cups YEARS   GDP                unit country    industry
#> 1        USA         4  1980  2857 billion current USD    <NA>        <NA>
#> 2        USA         4  2019 21380 billion current USD      US Agriculture
#> 3        USA         4  2019 21380 billion current USD      US      Mining
#> 4        USA         4  2019 21380 billion current USD      US Manifacture
#> 5        USA         4  2019 21380 billion current USD      US      Energy
#> 6        DEU         2  1980   854 billion current USD    <NA>        <NA>
#> 7        DEU         2  2019  3888 billion current USD      US Agriculture
#> 8        DEU         2  2019  3888 billion current USD      US      Mining
#> 9        DEU         2  2019  3888 billion current USD      US Manifacture
#> 10       DEU         2  2019  3888 billion current USD      US      Energy
#> 11       NOR         1    NA    NA                <NA>    <NA>        <NA>
#> 12       JPN         1  1980  1128 billion current USD    <NA>        <NA>
#> 13       JPN         1  2019  5118 billion current USD      US Agriculture
#> 14       JPN         1  2019  5118 billion current USD      US      Mining
#> 15       JPN         1  2019  5118 billion current USD      US Manifacture
#> 16       JPN         1  2019  5118 billion current USD      US      Energy
#>          freq
#> 1          NA
#> 2  0.08075014
#> 3  0.83433304
#> 4  0.60076089
#> 5  0.15720844
#> 6          NA
#> 7  0.08075014
#> 8  0.83433304
#> 9  0.60076089
#> 10 0.15720844
#> 11         NA
#> 12         NA
#> 13 0.08075014
#> 14 0.83433304
#> 15 0.60076089
#> 16 0.15720844
```

## Good to know

- When no shared key for merging is found among tables, the tables will
  get stacked.
- if no country, time column or any merging key is found in the tables,
  merging will be attempted based on column names. That is to say, the
  function will try to merge columns having identical names. If no
  overlapping name is found, then an error is returned.
- If columns to be merged have differing data types (e.g. numeric and
  logical), they will all be converted to character.
