#review of R language

#In R, a matrix is a collection of elements of the same data type (numeric, character, or logical) 
#arranged into a fixed number of rows and columns.

ttt=matrix(1:9, byrow = TRUE, nrow = 3)

# Box office Star Wars (in millions!)
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)

# Create box_office
box_office <- c(new_hope,empire_strikes,return_jedi)
box_office

# Construct star_wars_matrix
star_wars_matrix <- matrix(box_office,nrow=3,byrow=TRUE)
star_wars_matrix


# Box office Star Wars (in millions!)
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)

# Construct matrix
star_wars_matrix <- matrix(c(new_hope, empire_strikes, return_jedi), nrow = 3, byrow = TRUE)

# Vectors region and titles, used for naming
region <- c("US", "non-US")
titles <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")

# Name the columns with region
colnames(star_wars_matrix) =region


# Name the rows with titles
rownames(star_wars_matrix) = titles


# Print out star_wars_matrix
star_wars_matrix


# Construct star_wars_matrix
box_office <- c(460.998, 314.4, 290.475, 247.900, 309.306, 165.8)
star_wars_matrix <- matrix(box_office, nrow = 3, byrow = TRUE,
                           dimnames = list(c("A New Hope", "The Empire Strikes Back", "Return of the Jedi"), 
                                           c("US", "non-US")))

# Calculate worldwide box office figures
star_wars_matrix

worldwide_vector <- rowSums(star_wars_matrix)


# Construct star_wars_matrix
box_office <- c(460.998, 314.4, 290.475, 247.900, 309.306, 165.8)
star_wars_matrix <- matrix(box_office, nrow = 3, byrow = TRUE,
                           dimnames = list(c("A New Hope", "The Empire Strikes Back", "Return of the Jedi"), 
                                           c("US", "non-US")))

# The worldwide box office figures
worldwide_vector <- rowSums(star_wars_matrix)

# Bind the new variable worldwide_vector as a column to star_wars_matrix
all_wars_matrix <- cbind(star_wars_matrix,worldwide_vector)


# Animals
animals_vector <- c("Elephant", "Giraffe", "Donkey", "Horse")
factor_animals_vector <- factor(animals_vector)
factor_animals_vector
factor_animals_vector[1] > factor_animals_vector[2]  # the factor is not ordered

# Temperature
temperature_vector <- c("High", "Low", "High","Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High"))
factor_temperature_vector
factor_temperature_vector[1] >factor_temperature_vector[2]  # ordered one can use > to compare



# Code to build factor_survey_vector
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)

# Specify the levels of factor_survey_vector
levels(factor_survey_vector) <-c("Female","Male")

factor_survey_vector

# Build factor_survey_vector with clean levels
survey_vector <- c("M", "F", "F", "M", "M")
factor_survey_vector <- factor(survey_vector)
levels(factor_survey_vector) <- c("Female", "Male")
factor_survey_vector

# Generate summary for survey_vector
summary(survey_vector)


# Generate summary for factor_survey_vector
summary(factor_survey_vector)


# Create speed_vector
speed_vector <- c("fast", "slow", "slow", "fast", "insane")

# Convert speed_vector to ordered factor vector
factor_speed_vector <-factor(speed_vector, ordered=TRUE, levels = c("slow","fast","insane"))

# Print factor_speed_vector
factor_speed_vector
summary(factor_speed_vector)

#Vectors (one dimensional array): can hold numeric, character or logical values. The elements in a vector all have the same data type.
#Matrices (two dimensional array): can hold numeric, character or logical values. The elements in a matrix all have the same data type.
#Data frames (two-dimensional objects): can hold numeric, character or logical values. Within a column all elements have the same data type, but different columns can be of different data type.
# Vector with numerics from 1 up to 10
my_vector <- 1:10 

# Matrix with numerics from 1 up to 9
my_matrix <- matrix(1:9, ncol = 3)

# First 10 entries of the built-in data frame mtcars
my_df <- mtcars[1:10,]

# Construct list with these different elements:
my_list <- list(my_vector,my_matrix,my_df)
names(my_list) =c("vec","mat","df")

is.numeric(5)

my_list

# The social data has been created for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)
views <- matrix(c(linkedin, facebook), nrow = 2, byrow = TRUE)

# When does views equal 13?
views==13

# When is views less than or equal to 14?
views<=14

c(TRUE,TRUE,FALSE) & c(TRUE,FALSE,FALSE)

#&& only look at the first one
c(TRUE,TRUE,FALSE) && c(TRUE,FALSE,FALSE)

c(TRUE,TRUE,FALSE) | c(TRUE,FALSE,FALSE)

#|| only look at the first one
c(TRUE,TRUE,FALSE) || c(TRUE,FALSE,FALSE)

x <- 5
y <- 7
!(!(x < 4) & !!!(y > 12))


ctr=1
while(ctr<=7){
  print(paste("ctr is set to", ctr))
  ctr =ctr+1
}

ctr=1
while (ctr<=7) {
  if(ctr%%5==0){
    break
    #next  //next skip to the next iteration
  }
  print (paste("ctr is set to", ctr))
  ctr = ctr+1
}


nchar("abcdef")


cities = c("A","B","C","D")

for (i in 1:length(cities)){
  print (cities[i])
}


for (city in cities){
  print (city)
}


# The nyc list is already specified
nyc <- list(pop = 8405837, 
            boroughs = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"), 
            capital = FALSE)

nyc

# Loop version 1
for (l in nyc){
  print (l)
}



# Loop version 2
for (i in 1: length(nyc)){
  print (nyc[[i]])
}


for (i in 1:nrow(ttt)) {
  for (l in 1:ncol(ttt)) {
    print(paste("On row",i, "and column", l,"the board contains", ttt[i,l]))
  }
}

values = c(1,5,6,NA)
sd(values)
sd(values,na.rm=TRUE)

#check the arguments for the function
args(sd)

?read.table()


# Finish the pow_two() function
pow_two <- function(x, print_info=TRUE) {
  y <- x ^ 2
  if(print_info==TRUE) {
    print(paste(x, "to the power two equals", y)) }
  return(y)
}

pow_two(55, print_info=FALSE)
pow_two(55)


#R passes arguments by value
#The title gives it away already: R passes arguments by value. 
#What does this mean? Simply put, it means that 
#an R function cannot change the variable that you input to that function.

triple <- function(x) {
  x <- 3*x
  x
}
a <- 5
triple(a)
a  # a did not change its value



# The linkedin and facebook vectors have already been created for you
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)

# The interpret() can be used inside interpret_all()
interpret <- function(num_views) {
  if (num_views > 15) {
    print("You're popular!")
    return(num_views)
  } else {
    print("Try to be more visible!")
    return(0)
  }
}

# Define the interpret_all() function
# views: vector with data to interpret
# return_sum: return total number of views on popular days?
interpret_all <- function(views, return_sum=TRUE) {
  count <- 0
  
  for (v in views) {
    count = count + interpret(v)
    
  }
  
  if (return_sum) {
    return (count)
    
  } else {
    return (NULL)
    
  }
}

# Call the interpret_all() function on both linkedin and facebook

interpret_all(linkedin)
interpret_all(facebook)


search()  # see the package loaded

# Chunk 1
library(data.table)
require(rjson)

# Chunk 2
library("data.table")
require(rjson)

# Chunk 3
library(data.table)
require(rjson, character.only = TRUE)

# Chunk 4
library(c("data.table", "rjson"))

nyc = list("1", 1, TRUE)
lapply(nyc,class)

nyc = list(item1="1", item2=1, item3=TRUE)
lapply(nyc,class)

#in put is a vector, output is a list using lapply
#lapply always return a list
cities = c("Ab","abc","abcd","abcde","abcedf","abcdefg")
lapply(cities,nchar)

# to turn it into a vecor, use unlist function
unlist(lapply(cities, nchar))

oil_price = list(2.37, 2.49, 2.18, 2.22, 2.47,2.32)
triple =function(x) {
  return (3*x)
}
results = lapply(oil_price,triple)
unlist(results)

multiply = function(x, factor){
  return(x*factor)
}

times3 = lapply(oil_price, multiply, 3)
unlist(times3)

times3 = lapply(oil_price, multiply, factor=3)
unlist(times3)

#lapply document
#put it generally, lapply takes a vector or list X, 
#and applies the function FUN to each of its members. 
#If FUN requires additional arguments, 
#you pass them after you've specified X and FUN (...). 
#The output of lapply() is a list, the same length as X, 
#where each element is the result of applying FUN on the corresponding element
#of X.

# The vector pioneers has already been created for you
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")

# Split names from birth year
split_math <- strsplit(pioneers, split = ":")
split_math

# Convert to lowercase strings: split_low
split_low = lapply(split_math, tolower)
split_low

# Take a look at the structure of split_low
# Write function select_first()
select_first <- function(x) {
  x[1]
}

# Apply select_first() over split_low: names
names= lapply(split_low, select_first)

# Write function select_second()
select_second = function(x){
  return (x[2])
}

#! But defining functions to use them only once is kind of overkill, isn't it? 
#That's why you can use so-called anonymous functions in R.

# Named function
triple <- function(x) { 3 * x }

# Anonymous function with same implementation
function(x) { 3 * x }

# Use anonymous function inside lapply()
lapply(list(1,2,3), function(x) { 3 * x })

# Transform: use anonymous function inside lapply
names <- lapply(split_low, function(x){x[1]})

# Transform: use anonymous function inside lapply
years <- lapply(split_low, function(x){x[2]})
names
years

# Generic select function
select_el <- function(x, index) {
  x[index]
}

# Use lapply() twice on split_low: names and years
names = lapply(split_low, select_el, index=1)
years = lapply(split_low, select_el, index=2)

split_low
lapply(split_low, function(x) {
  if (nchar(x[1]) > 5) {
    return(NULL)
  } else {
    return(x[2])
  }
})

aaaa=sapply(cities, nchar)
bbbb= sapply(cities, nchar, USE.NAMES = FALSE)
aaaa
bbbb

first_and_last = function(name){
  name = gsub(" ","",name)
  letters = strsplit(name, split="")[[1]]
  c(first=min(letters), last=max(letters))}
  
first_and_last("New York")

sapply(cities, first_and_last)  # sapply is for simplied lapply

new_df=sapply(list(runif (10), runif (10)), 
       function(x) c(min = min(x), mean = mean(x), max = max(x)))
#(2) This code generates a matrix with 3 rows and 2 columns.
#(3) The function that is used inside sapply() is anonymous.

colnames(new_df)
rownames(new_df)

# vapply has to explicilitly mention the return type

vapply(cities, nchar, numeric(1))
vapply(cities, first_and_last, character(2))


sort(rep(seq(8,2,by=-2),times =2))

seq(1,10,by=3)

rep(c(8,6,4,2),times=2)
rep(c(8,6,4,2),each=2)

sort(c(8,6,4,3,8,6,4,2))

sort(c(8,6,4,3,5,1), decreasing = TRUE)

is.character("a")

# Don't edit these two lines
vec1 <- c(1.5, 2.5, 8.4, 3.7, 6.3)
vec2 <- rev(vec1)

# Fix the error
mean(c(abs(vec1), abs(vec2)))
#for the documentation of mean(), the first argument, x, should be a vector. 

#R features a bunch of functions to juggle around with data structures::
  
#seq(): Generate sequences, by specifying the from, to and by arguments.
#rep(): Replicate elements of vectors and lists.
#sort(): Sort a vector in ascending order. Works on numerics, but also on character strings and logicals.
#rev(): Reverse the elements in a data structures for which reversal is defined.
#str(): Display the structure of any R object.
#append(): Merge vectors or lists.
#is.*(): Check for the class of an R object.
#as.*(): Convert an R object from one class to another.
#unlist(): Flatten (possibly embedded) lists to produce a vector.

# The linkedin and facebook lists have already been created for you
linkedin <- list(16, 9, 13, 5, 2, 17, 14)
facebook <- list(17, 7, 5, 16, 8, 13, 14)

# Convert linkedin and facebook to a vector: li_vec and fb_vec
li_vec = as.vector(linkedin)
fb_vec= as.vector(facebook)

# Append fb_vec to li_vec: social_vec
social_vec=unlist(append(li_vec, fb_vec))
social_vec= unlist(c(li_vec,fb_vec) ) # x must be atomic
social_vec

# Sort social_vec
sort(social_vec, decreasing =TRUE)

# The linkedin and facebook lists have already been created for you
linkedin <- list(16, 9, 13, 5, 2, 17, 14)
facebook <- list(17, 7, 5, 16, 8, 13, 14)

# Convert linkedin and facebook to a vector: li_vec and fb_vec
li_vec = as.vector(linkedin)
fb_vec= as.vector(facebook)

# Append fb_vec to li_vec: social_vec
social_vec=unlist(append(li_vec, fb_vec))

# Sort social_vec
sort(social_vec, decreasing =TRUE)

rep(1,7)
rep(seq(1, 7, by = 2), 7)

#regular expression
#grepl() and grep()
#grepl(pattern = <regex>, x=<string>)
#grepl(), which returns TRUE when a pattern is found in the corresponding character string.
#grep(), which returns a vector of indices of the character strings that contains the pattern.
animals =c("cat","moose","impala","ant","kiwi")
grepl(pattern="a", x=animals)

#search for begins with a using ^
grepl(pattern="^a", x=animals)
grep(pattern="^a", x=animals)

#search for ends with a using dollar sign $
grepl(pattern="a$", x=animals)
grep(pattern = "a$", x=animals)

# grep(), the result is the index that matches
grep(pattern="a", x=animals)
which(grepl(pattern="a", x=animals))
##################
## replacement regular expression
##################
?sub()   # only look for the first match in the string
?gsub()   # replace everything

#sub(pattern= <regex>, replacement=<str>, x=<str>)
sub(pattern="a", replacement = "o", x=animals)
#notice in impala only the first a is got replaced
gsub(pattern="a", replacement="o", x=animals)

#####################
#   | for or
#####################
gsub(pattern="a|i", replacement = "_", x=animals)

# The emails vector has already been defined for you
emails <- c("john.doe@ivyleague.edu", "education@world.gov", "dalai.lama@peace.org",
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")

# Use grepl() to match for .edu addresses more robustly
grepl(pattern="@.*\\.edu$", emails)

# Use grep() to match for .edu addresses more robustly, save result to hits
hits = grep(pattern ="@.*\\.edu$", emails)

# Subset emails using hits
emails[hits]

# The emails vector has already been defined for you
emails <- c("john.doe@ivyleague.edu", "education@world.gov", "global@peace.org",
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")

# Use sub() to convert the email domains to datacamp.edu
sub(pattern="@.*\\.edu$","@datacamp.edu",emails)


#.*: A usual suspect! It can be read as "any character that is matched zero or more times".
#\\s: Match a space. The "s" is normally a character, escaping it (\\) makes it a metacharacter.
#[0-9]+: Match the numbers 0 to 9, at least once (+).
#([0-9]+): The parentheses are used to make parts of the matching string available to define the replacement. 
#The \\1 in the replacement argument of sub() gets set to the string 
#that is captured by the regular expression [0-9]+.

awards <- c("Won 1 Oscar.",
            "Won 1 Oscar. Another 9 wins & 24 nominations.",
            "1 win and 2 nominations.",
            "2 wins & 3 nominations.",
            "Nominated for 2 Golden Globes. 1 more win & 2 nominations.",
            "4 wins & 1 nomination.")

sub(".*\\s([0-9]+)\\snomination.*$", "\\1", awards)

#The ([0-9]+) selects the entire number that comes before the 
#word “nomination” in the string, and the entire match gets replaced by this number because of the \\1 
#that reference to the content inside the parentheses

#########################################
# times and dates
#########################################
today = Sys.Date()   # D is captial
today

class(today)

now = Sys.time()  # t is small t
now
class(now)

my_date = as.Date("1971-05-14")
my_date
class(my_date)


my_date = as.Date("1971-14-05")
# the above does not work
my_date = as.Date("1971-14-05",format="%Y-%d-%m")
# the above works

my_time = as.POSIXct("1971-05-14 11:24:15")
my_time

########################
#mathmatics on dates
########################
my_date
my_date+1

my_date2 = as.Date("1998-09-29")
my_date2 -my_date


#In R, dates are represented by Date objects, 
#while times are represented by POSIXct objects. 
#Under the hood, however, these dates and times are simple numerical values. 
#Date objects store the number of days since the 1st of January in 1970. 
#POSIXct objects on the other hand, store the number of seconds 
#since the 1st of January in 1970.

#To create a Date object from a simple character string in R, 
#you can use the as.Date() function. The character string has to obey a format that can be defined using a set of symbols
#(the examples correspond to 13 January, 1982):
  
#%Y: 4-digit year (1982)
#%y: 2-digit year (82)
#%m: 2-digit month (01)
#%d: 2-digit day of the month (13)
#%A: weekday (Wednesday)
#%a: abbreviated weekday (Wed)
#%B: month (January)
#%b: abbreviated month (Jan)

as.Date("1982-01-13")
as.Date("Jan-13-82", format = "%b-%d-%y")
as.Date("13 January, 1982", format = "%d %B, %Y")

today <- Sys.Date()
format(Sys.Date(), format = "%d %B, %Y")
format(Sys.Date(), format = "Today is a %A!")

# Definition of character strings representing dates
str1 <- "May 23, '96"
str2 <- "2012-03-15"
str3 <- "30/January/2006"

# Convert the strings to dates: date1, date2, date3
date1 <- as.Date(str1, format = "%b %d, '%y")
date2 = as.Date(str2, format ="%Y-%m-%d" )
date3=as.Date(str3, format = "%d/%B/%Y")

# Convert dates to formatted strings
format(date1, "%A")
format(date2, "%d")
format(date3, "%b %Y")


#Similar to working with dates, you can use as.POSIXct() to convert from a character string to a POSIXct object, and format() to convert from a POSIXct object to a character string. Again, you have a wide variety of symbols:
  
#%H: hours as a decimal number (00-23)
#%I: hours as a decimal number (01-12)
#%M: minutes as a decimal number
#%S: seconds as a decimal number
#%T: shorthand notation for the typical format %H:%M:%S
#%p: AM/PM indicator
#as.POSIXct() uses a default format to match character strings. 
#In this case, it's %Y-%m-%d %H:%M:%S.


# Definition of character strings representing times
str1 <- "May 23, '96 hours:23 minutes:01 seconds:45"
str2 <- "2012-3-12 14:23:08"

# Convert the strings to POSIXct objects: time1, time2
time1 <- as.POSIXct(str1, format = "%B %d, '%y hours:%H minutes:%M seconds:%S")
time2 <- as.POSIXct(str2)

# Convert times to formatted strings
format(time1, "%M")
format(time2, "%I:%M %p")

?diff()
now2= Sys.Date()
diff(now2-as.Date("1982-01-13"))


now <- Sys.time()
now + 3600        # add an hour
now - 3600*24     # subtract a day

birth <- as.POSIXct("1879-03-14 14:37:23")
death <- as.POSIXct("1955-04-18 03:47:12")
einstein <- death - birth
einstein