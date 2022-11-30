# Read in list
list = read.csv(file.choose())

# Load reshape2 package and melt dataset.
# Use install.packages(reshape2) if you haven't installed reshape2
library(reshape2)
mlist = melt(list)

# melt() isn't working properly, and it looks like "September" is
# causing the problem. Use str(list) to investigate
str(list)

# It looks Trump's September entry has a rogue 'a', making "September"
# a character variable. Let's fix that.
list$September[4] = 5

# Just setting that cell to 5 didn't help. We now need to specify that
# September should be numeric.
str(list)
list$September = as.numeric(list$September)

# Looks like it worked.
str(list)

# Now lets re-melt the dataset
mlist = melt(list)

# We should also check for missing values. This is easier to do after
# the dataset has been melted. If there are no missing values, this 
# will return integer(0). In this case, we do have 1 missing value.
which(is.na(mlist$value))

# Molten data frames make it easy to run statistical tests. Here we
# run an ANOVA and Tukey pairwise comparisons.
aovlist = aov(value~Name, data = mlist)
summary(aovlist)
TukeyHSD(aovlist)

# Plots are also easy with molten data frames. This is true for both
# base R and ggplot.
boxplot(value~Name, data=mlist)


