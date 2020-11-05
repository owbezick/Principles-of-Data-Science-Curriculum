
# Create the object 'die' and assign the six digits that belong to a standard die
die <- 1:6

# Print the sample command to show how the sample command can "roll" a die or multiple dice
print(sample(x = die, size = 1))
print(sample(die, 2, TRUE))

# Assign the command to the object 'dice', which can now be called to execute the "rolling" of the dice
dice <- sample(die, 2, TRUE)

# Create the function 'roll', which
roll <- function() {
  die <- 1:6
  dice <- sample(die, 2, TRUE)
  sum(dice)
} 
# Run the function and see it work
roll()

