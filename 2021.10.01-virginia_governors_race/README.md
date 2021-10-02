VA Governor Race Polling Average
================

### The problem with simple averages

Rolling poll averages can be misleading in the absence of errorbars or
an expected distribution of outcomes. FiveThirtyEight is currently
[tracking polls of Virginia’s Governor
race](https://projects.fivethirtyeight.com/polls/governor/virginia/)
slated for early November, and has kindly made their polls [available to
the
public](https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv).
Their current polling average, however, looks to be a simple rolling
average and doesn’t include a confidence interval. I’ve attempted to
improve upon their tracker here by providing a weighted polling average
and 95% confidence interval.

![](plots/race_results_current.png)

### How this works

Using a weak uniform prior, the two-party polling average (aka, the
polling average if including only Democrats & Republicans) is updated
daily via [Bayes’
theorem](https://en.wikipedia.org/wiki/Bayes%27_theorem). Each poll is
weighted by sample size and recency. As time passes, the effect a poll
has on the weighted average shrinks (this is why the confidence interval
“fans out” beyond the present day; as time passes, we gradually lose
confidence in the polling average). Each candidate’s probability of
winning on any given day is the portion of the projected election-day
polling distribution in their favor, based on that day’s polling
average.

### A few caveats/some closing thoughts

This is an inherently flawed method. The sample size & recency weighting
methods are somewhat arbitrary, and this ignores other weighting
factors, like pollster and survey methodology, that [should be
included](https://gelliottmorris.substack.com/p/what-people-are-missing-about-the?justPublished=true).
It is, however, transparent - you can view the code
[here](https://gelliottmorris.substack.com/p/what-people-are-missing-about-the?justPublished=true).
I’d happily welcome any comments/criticism!

This will be updated daily (I’ll manually rerun/recommit until I can
figure out GitHub Actions). This page will always show the current day’s
average & projection, but historical runs can be found in the [plots
folder](https://github.com/markjrieke/thedatadiary/tree/main/2021.10.01-virginia_governors_race/plots).
