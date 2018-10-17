HW05-factor and figure management
================
Frederike Basedow
15 Oktober 2018

Load packages

``` r
library(tidyverse)
library(knitr)
library(gapminder)
```

Goals:

-   Reorder a factor in a principled way based on the data and demonstrate the effect in arranged data and in figures.
-   Write some data to file and load it back into R.
-   Improve a figure (or make one from scratch), using new knowledge, e.g., control the color scheme, use factor levels, smoother mechanics.
-   Make a plotly visual.
-   Implement visualization design principles.

### The Assignment

#### Part 1: Factor management

With the data set of your choice, after ensuring the variable(s) you’re exploring are indeed factors, you are expected to:

1.  Drop factor / levels;
2.  Reorder levels based on knowledge from data.

We’ve elaborated on these steps for the gapminder and singer data sets below.

Be sure to also characterize the (derived) data before and after your factor re-leveling:

-   Explore the effects of arrange(). Does merely arranging the data have any effect on, say, a figure?
-   Explore the effects of reordering a factor and factor reordering coupled with arrange(). Especially, what effect does this have on a figure?

These explorations should involve the data, the factor levels, and some figures.

**Elaboration for the gapminder data set**

**Drop Oceania.* Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.*

``` r
levels(gapminder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
nrow(gapminder)
```

    ## [1] 1704

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
no_OC <- gapminder %>% 
  filter(continent != "Oceania")

levels(no_OC$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
nrow(no_OC)
```

    ## [1] 1680

``` r
str(no_OC)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
no_OC %>% 
  ggplot(aes(continent, lifeExp)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Life Expectancy in different continents", x = "Continent", y = "Life Expectancy") 
```

![](HW05-Factor_and_figure_management_files/figure-markdown_github/unnamed-chunk-2-1.png)

Just filtering gapminder to remove Oceania, doesn't actually remove it. There are still the same number of countries. However, there are less rows in the data set and when you plot it Oceania is not shown. Let's drop the level and see what happens:

``` r
real_no_OC <- no_OC %>% droplevels()

levels(real_no_OC$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

``` r
nrow(real_no_OC)
```

    ## [1] 1680

``` r
str(real_no_OC)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

Oceania is gone as a level and the number of levels of `country` is reduced as well.

We can also use `fct_drop` to drop specifically only the levels from continent, but leave the rest intact?

``` r
other_no_OC <- no_OC %>% 
  mutate(continent = fct_drop(continent))

str(other_no_OC)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
nrow(other_no_OC)
```

    ## [1] 1680

``` r
levels(other_no_OC$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

``` r
other_no_OC %>% 
  filter(country == "Australia") %>% 
  nrow() # see if we can still make use of the Oceania countries
```

    ## [1] 0

Now all levels in the country factor are kept while Oceania is removed as a level from the factor continent. However, they are not used, as we can see when we filter for Australia, which gives us zero rows as an output. So `droplevels` is more useful in removing unused factor levels.

**Reorder the levels of country or continent.* Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median.*

Let's calculate and plot the median of lifeExp for each continent and plot it to see how it's automatically ordered.

``` r
gapminder %>% 
  group_by(continent) %>% 
  summarize(median = median(lifeExp)) %>% 
  ggplot(aes(continent, median)) +
  geom_bar(stat = "identity", width = 0.5, fill = "pink", colour = "grey40") +
  labs(title = "Median Life Expectancy in Different Continents", x = "", y = "Median Life Expectancy (years)") +
  theme_bw() + 
  theme(
    panel.background = element_rect(
      fill = "grey60",
      colour = "grey40",
      size = 1
    ),
    panel.grid.major = element_line(
      colour = NA
    ),
    panel.grid.minor = element_line(
      colour = NA
    ),
    axis.text = element_text(
      colour = "grey40",
      size = 12
    ),
    axis.title.y = element_text(
      size = 13
    ))
```

![](HW05-Factor_and_figure_management_files/figure-markdown_github/unnamed-chunk-5-1.png)

Looks like it's ordered by alphabet. Let's arrange the data by decreasing median lifeExp instead.

``` r
gapminder %>% 
  group_by(continent) %>% 
  summarize(median = median(lifeExp)) %>% 
  mutate(continent = fct_reorder(continent, median, .desc = TRUE)) %>% 
  ggplot(aes(continent, median)) +
  geom_bar(stat = "identity", width = 0.5, fill = "pink", colour = "grey40") +
  labs(title = "Median Life Expectancy in Different Continents", x = "", y = "Median Life Expectancy (years)") +
  theme_bw() + 
  theme(
    panel.background = element_rect(
      fill = "grey60",
      colour = "grey40",
      size = 1
    ),
    panel.grid.major = element_line(
      colour = NA
    ),
    panel.grid.minor = element_line(
      colour = NA
    ),
    axis.text = element_text(
      colour = "grey40",
      size = 12
    ),
    axis.title.y = element_text(
      size = 13
    ))
```

![](HW05-Factor_and_figure_management_files/figure-markdown_github/unnamed-chunk-6-1.png)

Now it's nicely ordered by descending median life expectancy.

#### Part 2: File I/O

Experiment with one or more of write\_csv()/read\_csv() (and/or TSV friends), saveRDS()/readRDS(), dput()/dget(). Create something new, probably by filtering or grouped-summarization of Singer or Gapminder. I highly recommend you fiddle with the factor levels, i.e. make them non-alphabetical (see previous section). Explore whether this survives the round trip of writing to file then reading back in.

#### Part 3: Visualization design

\_ Remake at least one figure or create a new one, in light of something you learned in the recent class meetings about visualization design and color. Maybe juxtapose your first attempt and what you obtained after some time spent working on it. Reflect on the differences. If using Gapminder, you can use the country or continent color scheme that ships with Gapminder. Consult the dimensions listed in All the Graph Things.\_

I will use the singer data set for this because it feels like I've made every possible graph in gapminder throughout this course. I'll have to load it first:

``` r
# devtools::install_github("JoeyBernhardt/singer")
library(singer)
```

Let's have a look at it:

``` r
str(songs)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    22 obs. of  3 variables:
    ##  $ title      : chr  "Corduroy" "Grievance" "Stupidmop" "Present Tense" ...
    ##  $ artist_name: chr  "Pearl Jam" "Pearl Jam" "Pearl Jam" "Pearl Jam" ...
    ##  $ year       : int  1994 2000 1994 1996 1998 1996 1995 1992 1995 1993 ...

``` r
head(songs)
```

    ## # A tibble: 6 x 3
    ##   title         artist_name  year
    ##   <chr>         <chr>       <int>
    ## 1 Corduroy      Pearl Jam    1994
    ## 2 Grievance     Pearl Jam    2000
    ## 3 Stupidmop     Pearl Jam    1994
    ## 4 Present Tense Pearl Jam    1996
    ## 5 MFC           Pearl Jam    1998
    ## 6 Lukin         Pearl Jam    1996

Cool, the songs data set has 3 variables: song title, artist name and year. Let's make a graph of the number of songs in the data set per year:

``` r
songs %>% 
  ggplot(aes(year)) +
  geom_bar()
```

![](HW05-Factor_and_figure_management_files/figure-markdown_github/unnamed-chunk-9-1.png)

*Then, make a new graph by converting this visual (or another, if you’d like) to a plotly graph. What are some things that plotly makes possible, that are not possible with a regular ggplot2 graph? *

#### Part 4: Writing figures to file

Use ggsave() to explicitly save a plot to file. Then use `![Alt text](/path/to/img.png)` to load and embed it in your report. You can play around with various options, such as:

-   Arguments of ggsave(), such as width, height, resolution or text scaling.
-   Various graphics devices, e.g. a vector vs. raster format.
-   Explicit provision of the plot object p via ggsave(..., plot = p). Show a situation in which this actually matters.

#### But I want to do more!

If you’re particularly keen on levelling up the challenge of this assignment, try these things (this is all optional):

Make a deeper exploration of the forcats packages, i.e. try more of the factor level reordering functions.

Revalue a factor, e.g.:

    Gapminder version: Pick a handful of countries, each of which you can associate with a stereotypical food (or any other non-controversial thing … sport? hobby? type of music, art or dance? animal? landscape feature?). Create an excerpt of the Gapminder data, filtered to just these countries. Create a new factor – you pick the name! – by mapping the existing country factor levels to the new levels.
        Examples: Italy –> wine, Germany –> beer, Japan –> sake. (Austria, Germany) –> German, (Mexico, Spain) –> Spanish, (Portugal, Brazil) –> Portuguese. Let your creativity flourish.
