#Emily Black
#29 Nov. 2022
#Tidying Santa's list

#Santa has asked us for help digitizing his nice and naughty list! Let's get started. 

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_


#Part 0. Script setup
#clear R's brain
#This helps reset R for our analysis. It removes things that might be cluttering 
#up our environment, and makes it easier to see what we're doing
rm(list=ls())


#load relevant libraries for script
#There are some packages we need to run this code. Let's install and download them!
pkgs <- c("tidyverse")
#Even though we've only got one package, this is a nice way to install a bunch of 
#packages without a ton of code 

# install.packages(pkgs) #uncomment this if you need to install packages!
# #An easy way to comment or uncomment is ctrl-shift-C! Try it!
lapply(pkgs, library, character.only = TRUE) #install all our packages
rm(pkgs) #remove the list of package names 

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 1. Reading in our data
#Firstly, we need to see what Santa has given us. How clean is our data to begin with?
#we can use the read.csv function from the tidyverse to read it in
#Make sure you've got your R project open so it all reads in well! 
santa_list <- read_csv("raw_data/santas_list_2019_2021.csv") #this is the tidyverse
                                                            #version of read_csv


#Take a look: 
View(santa_list)
#wow! That's messy. We can do better!

#let's see our column names
colnames(santa_list)


#Can you list some things wrong with the dataset? 
#1. 
#2. 
#3. 

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 2. Fixing those weird columns!
#We can see that some of the messy columns contain gifts, and some of them contain naughty 
#or nice with the year. 
#But, all their names are messed up!


#The "gsub" function helps us replace things in values or column names. 
#Using the "colnames" function, we can select column names and replace repeating values
#with something more tidy

#Replace the "..." with "_"
#Note: "." is a special character in gsub, meaning any character. To override this, 
#we need to put \\ to indicate we actually mean "..."
colnames(santa_list) <- gsub("\\...", "_", colnames(santa_list))
colnames(santa_list)

#Great! We've gotten rid of our "...". Now what about those numbers?
santa_list2 <- santa_list
santa_list2
#I can see that even numbers are naughty and nice, and odd numbers are gifts
#so let's use gsub again on only odd or even numbers! 
#For this, we use some funky regular expressions: 
colnames(santa_list2) <- gsub("\\d*[02468]$", "list", colnames(santa_list2))
colnames(santa_list2) <- gsub("\\d*[13579]$", "gift", colnames(santa_list2))
#translating this...
        #"\\d means a numerical digit
        #* means for all numbers with 0 or more digits in our columns
        #[numbers] means these subset of numbers
        #$ means at the end of the number in the columns 
colnames(santa_list2)


#Why not just use the colnames function, you say??
#Well, Santa was nice and just gave us a list of 6 people... What if the list was 
#100? 1000? 8 billion?
#Our code should be reproducible and automatic, so it can be scaled up or copied as needed

#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3. Splitting up information
#Take a look at the name column. Notice anything wrong?

#There is more than one piece of information contained in one column!
#That's breaking tidy data rules! 

#How do we fix it? 
#again, the tidyverse has our back!
#The "separate" function can split columns into two
santa_list3 <- separate(santa_list2, #our data
                        "Name", #the column we want to separate
                        c("Name", "Gender"), #the new column names
                        sep = "-") #the separator of our two pieces of info
santa_list3
#Beautiful! 


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_

#Part 3. Reorganizing our data into tidy format 
#There is STILL something wrom with our dataset. Santa clearly needs
#to redo BIOL 300.... 

#Notice what it is?

#Our column names still contain information - year! 
#We need to reorganize this info to make our columns 
#only contain descriptive titles

#To do this, we can use the "pivot_longer" function 

#This takes our information from our column titles, and puts it into a designated column
#It also rearranges all our other info in order to make it tidy
#This is pretty complex, so it's going to take two steps

#1. Pivot longer to separate information in the columns 
santa_list4 <- santa_list3 %>% #side note - this is a pipe! A nice way 
                                #to make tidy workflows
  pivot_longer( cols = matches("\\d"), #select columns that have digits in them
                 names_to = c("year", "book"), #what information to take 
                                              #from column names
                names_sep = "_", #what's separating our names
                values_to = "value" #what column the values are going to
  )
santa_list4


#we've got pretty tidy data now, but we've now got multiple pieces of info in 
#the same column
#using the opposite of pivot_longer, pivot_wider, we can fix this! 

#now, we can use pivot_wider to get a list and a gift column 
santa_list4 <- santa_list4 %>% #pipe
  pivot_wider(names_from = book, #where to get the names of the columns from 
              values_from = value) #where to get the values in the column from 
santa_list4

#Is our data tidy now???

#can you think of anything else? 





