Momentum in Presidential Elections
================
Mark Rieke
9/26/2021

When discussing elections, horserace coverage tends to frame a state’s
expected outcome based on how it’s most recent voting patterns compare
to previous elections. This is, in part, why Florida is framed as
“trending towards Republicans” and Texas is framed as “trending towards
Democrats,” despite the fact that in the 2020 presidential election,
Trump won Texas by a wider margin than Florida!

![](pics/tx_fl_2pv.png) That certainly *feels* like a compelling
narrative, but I’m skeptical as to whether these trends can be explained
by momentum My hunch is that other factors, like changing demographics,
national sentiment, & presidential approval, can explain these trends &
that momentum itself offers little explanatory power. To test this
hunch, I’ll build and evaluate a few machine learning models with R’s
suite of machine learning packages,
[`tidymodels`](https://www.tidymodels.org/).

## Exploring the Data

I’ve put together a dataset for examining presidential elections at the
state level (for information on the dataset, you can reference the
[notes](https://github.com/markjrieke/thedatadiary/tree/main/2021.09.26-president_momentum/notes);
to see how it was put together, reference [this
script](https://github.com/markjrieke/thedatadiary/blob/main/2021.09.26-president_momentum/scripts/data_wrangle.R)).

yee
