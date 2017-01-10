<b>Instructions</b>

Your goal in this project is to use XPath and regular expressions to fetch data from the website fivethirtyeight.com, put the data in a Python Pandas dataframe, and then visualize the results.

Specifically, the goal is to fetch the data from the table at the bottom of the page http://projects.fivethirtyeight.com/election-2016/delegate-targets/, parse the results, and create a dataframe with the following columns:

Trump Won Delegates
Trump Target Delegates
Cruz Won Delegates
Cruz Target Delegates
Kasich Win Delegates
Kasich Target Delegates
Each line in your dataframe should also contain a date (the day of the primary) and the name of the state.

Once you have this dataframe, you are then asked to create the following plots:

A plot of the target delegates over time for the three candidates. You will find the cumsum() command of Pandas to be useful in computing the totals over time.
A plot of the won delegates over time for the three candidates
A plot of the percentage of the target over time, for each of the delegates (#total_won / #total_target, up to that date)
