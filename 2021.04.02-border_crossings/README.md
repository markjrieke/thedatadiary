
# The Border Surge?

### Investigating if there’s a true crisis at the border

## Intro

I’ve decided to start doing all my coding in an Rmarkdown doc - that way
I can more easily track through the iterations that it takes to get to a
final post, chart, etc. This will be a bit less formal and more loosey
goosey than the blog (maybe I should start referrring to this as the
blog & anything on the site as articles?), so don’t expect anything
fancy (or necessarily coherent).

This particular piece was inspired by [this WaPo
article](https://www.washingtonpost.com/politics/2021/03/23/theres-no-migrant-surge-us-southern-border-heres-data/),
which looked to see if there actually *is* a surge at the border or if
the surge is just a combination of seasonality and pent up demand while
restricted from travel during 2020 due to COVID-19.

## Digging into the data

``` r
library(tidyverse)
```

``` r
# creating dataframe
f_encounter <- tibble(fiscal_yr = c(rep("2018", 12),
                                    rep("2019", 12),
                                    rep("2020", 12),
                                    rep("2021", 5)),
                      # month 1 = oct (start of FY)
                      month = c(rep(seq(1, 12, 1), 3),
                                seq(1, 5)),
                      # data input manually...
                      encounters = c(34871, 39051, 40519, 35905, 36751, 50347,
                                     51168, 51862, 43180, 40149, 46719, 50568,
                                     60781, 62469, 60794, 58317, 76545, 103731,
                                     109415, 144116, 104311, 81777, 62707, 52546,
                                     45139, 42643, 40565, 36585, 36687, 34460,
                                     17106, 23237, 33049, 40929, 50014, 57674,
                                     71946, 72111, 74018, 78442, 100441))

# quick plot
f_encounter %>%
  ggplot(aes(x = month,
             y = encounters,
             group = fiscal_yr)) + 
  geom_line()
```

![](README_files/figure-gfm/frame%20setup-1.png)<!-- -->

Some notes - this is only a small subset of historical data. Not a lot
of history available, unless available somewhere else? Check if there is
a database elsewhere with this info

``` r
library(modelr)

# create model
m_crossing <- lm(encounters ~ month, data = f_encounter)
 
# add predictions & plot
f_encounter %>%
  add_predictions(m_crossing) %>%
  ggplot(aes(x = month, y = pred)) +
  geom_line()
```

![](README_files/figure-gfm/model%20setup-1.png)<!-- -->

Not sure that this is all that good for the seasonal shindig, maybe a
nonlinear model? Something to look into

``` r
# creating another frame with dates...
f_temp <- tibble(fy_month = seq(1, 12, 1),
                 cal_month = c(seq(10, 12, 1), seq(1, 9, 1)))

# joining & plotting
f_encounter %>%
  left_join(f_temp, by = c("month" = "fy_month")) %>%
  mutate(date = as.Date(paste(fiscal_yr, 
                              cal_month,
                              1,
                              sep = "-"))) %>%
  add_predictions(m_crossing) %>%
  ggplot(aes(x = date,
             y = pred)) +
  geom_line()
```

![](README_files/figure-gfm/model%20apply-1.png)<!-- -->

This is a bit askew - it’s because there’s a mix of calendar year &
fiscal year that I need to fix…

``` r
# creating new column for calendar year alignment
f_encounter$cal_yr <- c(rep(2017, 3),
                        rep(2018, 12),
                        rep(2019, 12),
                        rep(2020, 12),
                        rep(2021, 2))

# joining & plotting again
f_encounter %>%
  left_join(f_temp, by = c("month" = "fy_month")) %>%
  mutate(date = as.Date(paste(cal_yr, 
                              cal_month,
                              1,
                              sep = "-"))) %>%
  add_predictions(m_crossing) %>%
  ggplot(aes(x = date,
             y = pred)) +
  geom_line()
```

![](README_files/figure-gfm/frame%20fix-1.png)<!-- -->

Cool - that fixes that issue. Now I’ll make a plot that shows the
over/underperformance. There’s guaranteed to be quite a bit of
performance issues, since this is a 1-var linear model… We’ll see how it
looks & if it captures any of the seasonal differences.

``` r
# assigning frame now that it's fixed.
f_encounter <- f_encounter %>%
  left_join(f_temp, by = c("month" = "fy_month")) %>%
  mutate(date = as.Date(paste(cal_yr, 
                              cal_month,
                              1,
                              sep = "-"))) %>%
  add_predictions(m_crossing)

# adding over/under performance (residual)
f_encounter %>%
  mutate(res = encounters - pred) %>%
  ggplot(aes(x = date,
             y = res)) + 
  geom_line()
```

![](README_files/figure-gfm/model%20performance-1.png)<!-- -->

Coolio. This gives some useful info - 2019 was a huge year for
immigration. Should also double check the encounters overall…

``` r
# assigning frame from above
f_encounter <- f_encounter %>%
  mutate(res = encounters - pred)

# plot 
f_encounter %>%
  ggplot(aes(x = date,
             y = encounters)) + 
  geom_line()
```

![](README_files/figure-gfm/encounters%20check-1.png)<!-- -->

Hmm… Not sure if there’s actually anything useful there. It doesn’t look
like there is a lot of difference (except with scale) between the two
plots…

Ah okay I founda new dataset going back to 2012 - may be able to fit a
better model.

``` r
# rebuilding old frame, ground up...
f_encounter <- tibble(fiscal_yr = c(rep("2012", 12),
                                    rep("2013", 12),
                                    rep("2014", 12),
                                    rep("2015", 12),
                                    rep("2016", 12),
                                    rep("2017", 12),
                                    rep("2018", 12),
                                    rep("2019", 12),
                                    rep("2020", 12),
                                    rep("2021", 5)),
                      month = c(rep(seq(1, 12, 1), 9),
                                seq(1, 5)),
                      encounters = c(31323, 28601, 24360, 30758, 36990, 48520,
                                     46660, 42995, 36794, 32801, 33686, 32375,
                                     34836, 33153, 29075, 32481, 40632, 54009, 
                                     54761, 50481, 40785, 39993, 41110, 38182,
                                     41828, 38685, 36695, 35181, 42399, 57405, 
                                     59119, 68804, 66541, 48819, 39758, 34003,
                                     35895, 33023, 34238, 30178, 32550, 39159,
                                     38296, 40681, 38616, 38610, 42414, 41165,
                                     45507, 45752, 48737, 33654, 38309, 46117, 
                                     48502, 55442, 45772, 46966, 51961, 56535,
                                     66708, 63361, 58412, 42463, 23555, 16588, 
                                     15766, 19940, 21657, 25019, 30567, 31155,
                                     34871, 39051, 40519, 35905, 36751, 50347,
                                     51168, 51862, 43180, 40149, 46719, 50568,
                                     60781, 62469, 60794, 58317, 76545, 103731,
                                     109415, 144116, 104311, 81777, 62707, 52546,
                                     45139, 42643, 40565, 36585, 36687, 34460,
                                     17106, 23237, 33049, 40929, 50014, 57674,
                                     71946, 72111, 74018, 78442, 100441))

# adding in cal year and cal month
f_encounter$cal_yr <- c(rep(2011, 3),
                        rep(2012, 12),
                        rep(2013, 12),
                        rep(2014, 12),
                        rep(2015, 12),
                        rep(2016, 12),
                        rep(2017, 12),
                        rep(2018, 12),
                        rep(2019, 12),
                        rep(2020, 12),
                        rep(2021, 2))

f_encounter <- f_encounter %>%
  left_join(f_temp, by = c("month" = "fy_month")) %>%
  mutate(date = as.Date(paste(cal_yr, cal_month, 1, sep = "-")))

# quick plot
f_encounter %>%
  ggplot(aes(x = date,
             y = encounters)) +
  geom_line()
```

![](README_files/figure-gfm/new%20frame%20setup-1.png)<!-- -->

Okay that looks a lot better - can clearly see at least some sembelance
of a repeating pattern with spikes near the beginning of years. Wonder
why that is.

Back to trying to fit a linear model (which, again, isn’t necessarily a
good representation of how this works). First need to do a monthly plot.

``` r
# create plot
f_encounter %>%
  ggplot(aes(x = cal_month,
             y = encounters,
             group = cal_yr)) +
  geom_line()
```

![](README_files/figure-gfm/new%20monthly%20plot-1.png)<!-- -->

Sweet, that’s exactly what I wanted. I’ll note that there are two little
“sticks”, one on the left and one on the right. That’s because the data
is reported for the fiscal year, which starts in October 2011 (explains
the stick on the right) and I only have data up to Feb 2021 (explains
the stick on the left).

Now I’ll try to fit another linear model - hopefully, with 5 extra years
of data, it fits a bit better. 2019 & 2020 look to be off years, so I
should have known that trying to fit a model to just that subset would
have weird/uninformative results. That being said, it’s really only 60
extra data points, so I’m a bit apprehensive as to whether or not it’ll
be worth anything…

``` r
# fitting a new model based on the updated frame
m_crossing <- lm(encounters ~ cal_month, data = f_encounter)

# adding preds & plotting
f_encounter %>%
  add_predictions(m_crossing) %>%
  ggplot(aes(x = date,
             y = pred)) +
  geom_line()
```

![](README_files/figure-gfm/new%20model-1.png)<!-- -->

Alright - now we’ve got this seasonal model (again, that’s not the
greatest) going back \~9 years. Let’s see what happens when we plot the
residuals.

``` r
# plotting residuals
f_encounter %>%
  add_residuals(m_crossing) %>%
  ggplot(aes(x = date,
             y = resid)) + 
  geom_line()
```

![](README_files/figure-gfm/resids-1.png)<!-- -->

Hmm…. That’s still pretty ugly. Let me try something new.

I’ve been treating the month as a continuous variable, which is why the
model spits out this super sharp drop from December to January. I really
should treat it as a *categorical* variable. Let’s see if that helps:

``` r
# new categorical model
m_crossing <- lm(encounters ~ factor(cal_month), data = f_encounter)

# new model prediction plot
f_encounter %>%
  add_predictions(m_crossing) %>%
  ggplot(aes(x = date,
             y = pred)) +
  geom_line()
```

![](README_files/figure-gfm/new%20model%20again-1.png)<!-- -->

Okay, this is much more useful - this captures that seasonal change (and
does so better than a one off line that I got from the original linear
model). I’ll still note that this isn’t necessarily a *great* model, but
I think this gets us a lot closer to the finish line.

Now let’s plot the residuals.

``` r
# residual plot
f_encounter %>%
  add_residuals(m_crossing) %>%
  ggplot(aes(x = date, 
             y = resid)) + 
  geom_line()
```

![](README_files/figure-gfm/plotting%20resids-1.png)<!-- -->

Okay, disappointment. The residuals still don’t show any long term
trends… I wonder what it would look like if we fit a new model based off
data from *before* Trump’s inauguration and see if that has any better
explanatory power?

``` r
f_encounter %>%
  filter(date <= as.Date("2017-01-01")) %>%
  ggplot(aes(x = cal_month,
             y = encounters,
             group = cal_yr)) +
  geom_line()
```

![](README_files/figure-gfm/filtered%20frame-1.png)<!-- -->

It’s a bit more structured, so now let’s see if a new model based on
this helps…

``` r
# recreating & saving last frame
f_encounter_pre <- f_encounter %>%
  filter(date <= as.Date("2017-01-01"))

# fitting a new model
m_crossing_pre <- lm(encounters ~ factor(cal_month), data = f_encounter_pre)

# plotting new model's predictions
f_encounter_pre %>%
  add_predictions(m_crossing_pre) %>%
  ggplot(aes(x = date,
             y = pred)) +
  geom_line()
```

![](README_files/figure-gfm/yet%20another%20model-1.png)<!-- -->

Okay, I think this is a bit more representative. The *huge* spike in
2019 was giving a giant mid-year boost to the model. I’m still not sure
if cutting off the data like this is good practice, but let’s see what
the results are.

First I’ll check this model against the 2011-2016 dataset.

``` r
# model residual plot
f_encounter_pre %>%
  add_residuals(m_crossing_pre) %>%
  ggplot(aes(x = date,
             y = resid)) +
  geom_line()
```

![](README_files/figure-gfm/new%20model%20check-1.png)<!-- -->

I’m gonna guess that the residual plot of this model against the full
dataset is going to be disappointing…

``` r
# adding residual & time plot in same chunk just to compare
f_encounter %>%
  add_residuals(m_crossing_pre) %>%
  ggplot(aes(x = date,
             y = resid)) +
  geom_line()
```

![](README_files/figure-gfm/another%20residual%20plot-1.png)<!-- -->

``` r
f_encounter %>%
  ggplot(aes(x = date,
             y = encounters)) +
  geom_line()
```

![](README_files/figure-gfm/another%20residual%20plot-2.png)<!-- -->

Wow. I’m really not seeing the seasonality described in the WaPo
article. It’s hard to tell if it’s because the model is too simple (just
month as a predictor) or if there really isn’t a seasonality effect. On
the one hand, there are quite a few years where there’s an up-tick early
in the year, but there are also years where there’s a *drop* at the
beginning of the year. There are definitely a lot of other factors that
don’t get captured here, like policy decisions, country of origin (and
factors from *within* that country that would affect someones decision
to migrate), etc.

I think that there are two key things that can be taken away from this:
1. Seasonality *on it’s own* is definitely not a strong predictor of
apprehensions at the border. 2. Relative to the data from 2012-2017,
2019, 2020, and 2021 (already) have months with extremely high number of
border crossings.