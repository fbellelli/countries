#finds key of table
#column or set of column that uniquely identify entries in table
#this function is designed for country data: hence, it will first search for column containing country names and dates/years.
#It will then prioritise left-most columns in table

find_key <- function(x,
                     allow_NA=FALSE){

  #0) ceck inpunts
  #check validity of inputs

  #check for columns containing NA and exlcude them if requested

  #transform any factor to character?

  #1) find all country columns
  country_cols <- find_countrycol(x)

  #if none, move to 2

  #if more than one, check if columns are referring to same country,
  #if two distinct, possibly dyad data. If more than 2, issue warning and only keep most likely (name, position, NA, ...)

  #if at least one, check if it/they are key. if they are key, go to end

  #2) look for time columns

  #if none, move to 3

  #if more than one, check if the dates are repeating,
  #if still more than one, issue warning and only keep most likely (name, position, format, NA, ...)

  #if a time col is present, check if it is key with/without country cols. if they are, go to end

  #3) check for any other col

  #make list of all possible column combination.
  grid <- expand.grid(rep(list(cols), length(cols)))

  #rank combination based on the position of the columns they contain.
  #score = sum of rank of all columns involved in the combination, where the column rank is its position in the table (e.g. a combination of column 1 and 2, has a score 3)
  #assign a rank of 0 to all country and time cols.

  #starting from the combination with the lowest score, test every combination to see if they could be a key. Stop as soon as one is found.

  #END) prep output

  #NAMED vector containing names of columns that are key. the name of each entry indicates what type of column it is: country, time, other
  #create an option for return_index?

  #OPPURE:

  #inserire opzione "simiplify"

  #simplified:
  #vector containing names of columns that are key
  #create an option for return_index?

  #non simplified:
  #list breaking down the type of column: Country, time, other
  #info on data structure?info on NA?
  #list of key column indices?





}
