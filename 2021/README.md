# Advent of Code 2021

A repository containing code for Advent of Code challenges. Check out the challenge at [adventofcode.com](https://adventofcode.com/). 


## Day 1
A nice simple puzzle to get started. Solved using `{lapply}` for part one, and `{rollapply}` from the `{zoo}` package for part two.

## Day 2
I took different approaches to parts one and two for this one. I treated part one as a data wrangling exercise, and used `{dplyr}` to filter and rearrange the input data. For part two, I resorted to a for loop.

## Day 3
I found part one quite simple and used the `{strtoi()}` function for the first time to convert between binary and decimal numbers. For part two, I again used a for loop along with some `{dplyr}` magic.

## Day 4
For both parts of day 4, I used lists to store the boards along with a couple of for loops.

## Day 5
I managed to complete day 5 in base R without using any additional packages. Reading in the data and processing into the format I needed was actually the trickier part of this one for me.

## Day 6
Part one of day 6 was simple, part two not so much. There were a couple of different ways of doing this, but I didn't have a lot of time to craft the elegant solution I wanted to.  

## Day 7
For part one, I used the neat trick of using the median as it minimises the absolute differences. Part two was also quite simple, using triangular numbers.

## Day 8
I found part one very easy, just counting the number of different string lengths. Part two was a lot more complex, and relied on determining which digital number was a combination of others e.g. a 4 overlaid with a 3 gives the same output as a 9.

## Day 9
Again, part one was relatively straightforward using some for loops. After thinking about part two for a while, I went with a graph-based approach to identify connected components.

## Day 10
Day 10 was quite straightforward after working out the best way to process the brackets as data. I made a series of small functions to solve both parts one and two.


I've decided to call it quits on this year's advent of code after day 10. Life is getting busy. I've enjoyed the challenges I've completed because they're not the type of problems I usually work in. I've definitely gotten better at processing strings, and thinking about *better* ways to solve problems in base R.

