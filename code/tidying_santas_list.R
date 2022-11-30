#Emily Black and Marshall Lab 
#29 Nov. 2022
#Tidying Santa's list

#Santa has asked the Marshall lab 
#for help digitizing his nice and naughty list! Let's get started. 

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_





#Part 0. Script setup
#It's important we make sure our script is nice and clean to begin with
#Before we look at our data! 

#clearing R's brain: 
#This helps reset R for our analysis. It removes things that might be cluttering 
#up our environment, and makes it easier to see what we're doing
rm(list=ls())


#load relevant libraries for script
#There are some packages we need to run this code. Let's install and download them!
pkgs <- c("tidyverse")
#Even though we've only got one package, this is a nice way to install a bunch of 
#packages without a ton of code 

# install.packages(pkgs) #uncomment this if you need to install packages!
lapply(pkgs, library, character.only = TRUE) #install all our packages
rm(pkgs) #remove the list of package names 







#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 1. Reading in our data (and welcome to the Tidyverse!)


#Firstly, we need to see what Santa has given us. How clean is our data to begin with?
#we can use the read_csv function from the tidyverse to read it in
#Make sure you've got your R project open so it all reads in well! 


santa_list <- read_csv("raw_data/santas_list_2019_2021.csv") #this is the tidyverse
                                                            #version of read_csv


#Take a look: 
View(santa_list)

#let's see our column names
colnames(santa_list)

#Do we think this dataset is tidy or untidy?

#Can you list some things we need to fix? 
#1. 
#2. 
#3. 



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_




#Part 2. Fixing those weird columns!
#Take a look at the columns with the years in them. What's wrong with them?



#The "gsub" function helps us replace things in cells or column names. 
#Using the "colnames" function, we can select column names and replace repeating values
#with something more tidy



#Replace the "..." with "_"
#How do we do this? 
#Why might your first try not work? 

colnames(santa_list) <- gsub("", "", colnames(santa_list))
colnames(santa_list)



#Great! We've gotten rid of our "...". Now what about those numbers?
santa_list2 <- santa_list #Remaining our dataframe occasionally gives us a checkpoint 
                          #which saves us time and energy if we mess up
View(santa_list2)



#We can see that some of these columns have gifts, and some have naughty or nice. 
#How can we rename them to reflect that?


#For this, we will need some funky regular expressions... 
#and a function we have seen before... 
#Hint: 
        #"\\d means a numerical digit
        #* means for all numbers with 0 or more digits in our columns
        #[a set of numbers] means these subset of numbers in the digits
        #$ means at the end of the number in the columns 
colnames(santa_list2) <- mystery_function("", "list", colnames(santa_list2))
colnames(santa_list2) <- mystery_function("", "gift", colnames(santa_list2))

colnames(santa_list2)


#Why not just use the colnames function, you say?? I hate regular expressions, 
#you say?? 
#Such as: 
      #colnames(santa_list) <- c("2019_list", "2019_gift", etc....)

#Thoughts?? 

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3. Splitting up information
#Take a look at the "Name" column. Notice anything wrong?


#How do we fix it? 
#again, the tidyverse has our back!

#The "separate" function can split columns into two
santa_list3 <- separate(santa_list2, #our data
                        "", #the column we want to separate
                        c("", ""), #the new column names
                        sep = "") #the separator of our two pieces of info
santa_list3



#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3. Reorganizing our data into tidy format 
#There is STILL something wrong with our dataset. Santa clearly needs
#to redo BIOL 300.... 

#Notice what it is?





#To fix what you might be thinking about, we can use the "pivot_" functions

#This takes our information from our column titles, and puts it into tidy columns
#It also rearranges all our other info in order to make it tidy
#This is pretty complex, so it's going to take two steps




#1. Pivot longer to separate information in the columns 
santa_list4 <- santa_list3 %>% #side note - this is a pipe! A nice way 
                                #to make tidy workflows
  pivot_longer( cols = matches(""), #select columns that have digits in them using regex
                 names_to = c("", ""), #what to name
                                       #our new columns?
                names_sep = "", #character separating the information in our original columns
                values_to = "" #name of new column the values are going to
  )
santa_list4



#we've got pretty tidy data now, but we've now got multiple pieces of info in 
#the same column
#using the opposite of pivot_longer, pivot_wider, we can fix this! 



santa_list4 <- santa_list4 %>% #pipe
  pivot_wider(names_from = "", #where to get the names of the columns from 
              values_from = "") #where to get the values in the column from 
santa_list4

#Is our data tidy now???

#can you think of anything else? 




