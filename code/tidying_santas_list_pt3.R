#Emily Black and Marshall Lab 
#30 Nov. 2022
#Tidying Santa's list - with some new data!


#Jarrett did all the hard work of melting that new dataset with this year's stats
#So now let's make a 
#master dataframe with all the info!




#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_



#Part 0. Script setup
#As usual, we reset our environment and install the packages we need

#clearing R's brain: 
rm(list=ls())


#load relevant libraries for script
pkgs <- c("tidyverse")
# install.packages(pkgs) #uncomment this if you need to install packages!
lapply(pkgs, library, character.only = TRUE) 
rm(pkgs)  





#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_




#Part 1. Reading in our data


#Firstly, let's read in both of our cleaned datasets:
  #1. The tidied data from part 1
  #2. The melted dataset from part 2


cleaned_santa_list <- read_csv("mod_data/cleaned_santas_list_pt1.csv")
head(cleaned_santa_list) #note: the head function is very useful to 
                          #view long datasets

melted_santa_data <- read_csv("mod_data/melted_santa_list.csv")
#assign more descriptive column names: 
colnames(melted_santa_data) <- c("Name", "month", "days_nice")
head(melted_santa_data)

#Great! All of this data looks tidy. But wouldn't it be nice to put them together?


#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_




#Part 2. Calculating naughty or nice... who will make the list? 



#Santa is asking - can we take these number of "nice" days and figure out
#if people are on the naughty or nice list?

#He's told us that if people are nice for 16 or more days of the month, 
#that month they are "nice". 

#And if people are nice 7 or more months of the year, they are on the nice list. 





#1. Let's start by turning the number of nice days into categories: 
#naughty or nice!


  #For this, we can use a tidyverse function called "case_when"
  #This turns numbers into characters based on a range of values

  #We also need the mutate function to create a new column in our dataset
  #with the case_when values

melted_santa2 <- melted_santa_data %>%
  mutate(n_or_n = 
  case_when(days_nice "equation" ~ "Nice", 
            days_nice "equation" ~"Naughty")
  )
head(melted_santa2)

#2. Now we can take those naughty or nice values and calculate how nice people were
#for the whole year!

  #Since characters are hard to work with when summarizing, let's convert to logical
  #Logical is just a nice way of saying we have two responses (naughty or nice)
  #And we are assigning these to either 0 or 1
melted_santa2 <- melted_santa2 %>%
  mutate(logical = 
    ifelse(melted_santa2$n_or_n=="Nice", #if "nice",
                                1, #make 1,
                                0) #if not, make 0
  )
head(melted_santa2)

  #The numbers make it really easy for us to figure out who is naughty and nice!



  #A set of very useful tidyverse functions for the next step are group_by and summarize
  #They collect rows by a common value, and then calculate summary statistics
  #We can use the sum() function in our summarize to calculate the number of nice months, 
  #since they all equal 1 right now

#(Side note: this is one of the few times base R is more streamlined than the tidyverse!)
#Check out how the table function parses our data in one line: 
table(melted_santa2$Name, melted_santa2$n_or_n)
#The drawback is, it's harder to work with this data. Tidyverse gives us more flexibility


#Proceeding with tidyverse: 

melted_santa_summary <- melted_santa2 %>%
  group_by(Name) %>%
  summarize(n_months_nice = sum(logical))
melted_santa_summary

#Why didn't that work??


#Let's try again, but with a useful function called na.omit
#This removes rows with NA values
#We should also case this data to indicate naughty or nice. 
#Remember which function does that?

melted_santa_summary <- melted_santa2 %>%
  na.omit() %>%
  group_by(Name) %>%
  summarize(n_months_nice = sum(logical)) %>%
  mutate(list = "some useful function"(
    n_months_nice "equation" ~"Nice", 
    n_months_nice "equation" ~"Naughty"
  ))
melted_santa_summary




# We can see who is on the naughty and nice list! Is it who you expect? 




#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_
#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_#_




#Part 3. Combining the new datasets!



#Ensure the types of data match in both datasets before you proceed: 
melted_santa_summary$year <- c(2022)

#The bind_rows function combines two dataframes by their shared columns, 
#even if they don't all match! 

santa_list_complete <- bind_rows(cleaned_santa_list, #dataframe to bind new rows to
                             melted_santa_summary) %>% #dataframe which adds values
  arrange(desc(year)) #arrange in order of year
View(santa_list_complete)

#Woohoo, we did it! Now it's up to Santa to complete the list... no spoilers! :) 
  



