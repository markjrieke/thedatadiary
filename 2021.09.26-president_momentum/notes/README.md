
## Mark to do

-   finish out analysis
    -   EDA
    -   basic logistic regression
        -   kitchen sink
        -   no regularization
        -   report vip
    -   regularized linear regression
        -   kitchen sink, no additional features
        -   cvfold or bootstrap resamples?
        -   gridregular grid or something fancier?
    -   feature engineering
        -   collinear features?
        -   interactive features
        -   non-linear features
        -   non-normally distributed features (e.g., % of populations)
            -   maybe inverse sigmoid function (i.e., stretch values
                close to 0 out to infinity)
            -   I don’t know if this is a thing though
    -   tuned logistic regression
        -   kitchen sink + features
        -   bootstrap or cvfold resamples?
        -   regular grid or something fancier?
        -   report vip
-   update plots to be pretty pretty in github/the thing
-   rewrite so that it looks like a human wrote it lol
    -   alternatively, write a less technically intense version for
        squarespace & link to the more technical version on github
-   add “data dictionary” to the notes section
-   clean up notes lol

## notes

### Variables

-   1-cycle momentum
-   2-cycle momentum
-   non-hisp white pct
-   native american pct
-   mexican/hispanic/lation pct
-   black pct
-   aapi pct
-   previous presidential party
-   democrat incumbent
-   republican incumbent
-   national 2pv
-   prev prez approval
-   prev prez disapproval
-   prev prez net approval
-   real gdp growth since last election (q2 of election yr)
-   inflation since last election

### Some notes on the variables

-   demographics: linear interpolation between decennial census reports
    used to get individual year demographics for each state. When
    unavailable, the most recent reported value was used (i.e., if for
    example Alabama didn’t start tracking the % AAPI until 1960, then
    the 1960 value was used for 1950). Demographics are sourced from
    [Wikipedia](https://en.wikipedia.org/wiki/Historical_racial_and_ethnic_demographics_of_the_United_States).
-   approval ratings: taken from
    [538](https://projects.fivethirtyeight.com/biden-approval-rating/?ex_cid=rrpromo).
    Last day approval of previous president listed (for reelected
    incumbents, was their 4-year mark approval rating). Nixon & Kennedy
    did not finish their terms - Ford & Johnson’s approval ratings were
    used to fill in the gaps. For Johnson, the previous president’s
    ratings were considered to be his approval ratings on the 350th day
    of his term (election was 350 days after Kennedy was assassinated).
-   real gdp growth since last election - taken from the July GDP report
    of the election year (accessed via
    [FRED](https://fred.stlouisfed.org/series/GDPC1)).
-   inflation since last election - taken from the October seasonally
    adjusted CPI value from the election year (accessed via
    [FRED](https://fred.stlouisfed.org/series/CPIAUCSL)).
-   Individual state election results from 1976 onward are sourced from
    the [MIT Election
    Lab](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX).
    Prior presidential election results are taken from the year’s
    respective [Wikipedia
    Page](https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin#List).

### Miscellaneous notes

-   Congressional districts are ignored for the purposes of this
    exercise (i.e., only looking at the statewide results for ME & NE)
