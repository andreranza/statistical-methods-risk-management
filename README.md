ME 317: Statistical Methods in Risk Management at LSE
================
Andrea Ranzato;
August 1st, 2019

## Summary

The general purpose of this project is to use techniques learned during
the course in *Statistical methods in risk management* useful to analyze
stock price data. In particular, we consider prices concerning ten
different stocks recorded during the 2007-2009 financial crisis. The
data source is the *Wharton Research Data Services*.

The analysis is structured as follows. In the first section, I show how
two extreme events, the rescue of Bear Sterns on March 14, 2008 and
Lehman Brothers bankruptcy on September 15, 2008, affected the price and
log-return dynamics. Next, I test both the marginal and joint normality
of the ten stock returns by means of the *Jarque-Bera* and *Mardia test*
respectively. First, we observe that the normal distribution fails to
capture returns happening in the tails. Second, joint normality is not
detected. Subsequently, I evaluate some pair-wise stock returns, either
from companies of the same sector and different ones, in order to
quantify their correlation.

In the second section, I focus on financial stocks with the objective of
assessing their dependence using copulas of different types. We see,
that the copula which fits the observations best is the t-Copula.

In the third section, I consider a linear portfolio which invest $1000
in each of the stocks at the beginning of the period, assuming that its
composition does not change during the entire period. Afterwards, I
evaluate the Value-at-Risk of the portfolio at 95% level using two
different strategies. Firstly, I compute the VaR using the empirical
profit and loss distribution, secondly using a normal model. The
comparison between the two shows that the parametric approach would have
led to more conservative capital management decisions compared to
empirical one, since it signals higher potential losses. In the end, I
add to the analysis 2010 portfolio returns, in order to record the
number of violations occurred under the two different frameworks. We see
that the normal model incurs in fewer violations with respect to the the
historical one.

The last section is devoted to the description of the topics covered
during the course.

## Prices and log returns dynamics

The purpose of this section consists in showing some empirical
properties of financial time series using a sample of US assets. First
of all, I conduct an exploratory analysis of the price dynamics of
equities in the Table below between 3 January 2007 and 31 December 2009.
As we can see in the Figure below, a strong negative trend plays out in
correspondence with Bear Stearns rescue on March 14 2008 and terminates
around January 20 2009. Table\~ quantifies the percentage price change
recorded in that period summarizing the worst downturn of the US
financial system since the Great Depression.

| Company                      | Sector     | Ticker |
| :--------------------------- | :--------- | :----- |
| American International Group | Financials | AIG    |
| Amazon.com, Inc.             | Consumer   | AMZN   |
| Bank of America Corp.        | Financials | BA     |
| Goldman Sachs Group, Inc.    | Financials | GS     |
| Coca-Cola Company            | Consumer   | KO     |
| Morgan Stanley               | Financials | MS     |
| Microsoft Corp.              | IT         | MSFT   |
| Northern Trust Corp.         | Financials | NTRS   |
| Wells Fargo & Co             | Financials | WFC    |
| ExxonMobil Corp.             | Energy     | XOM    |

Table: Equity portfolio composition.

| Ticker | Sector    | P.2008-03-14 | P.2009-01-20 | (%) Change |
| :----- | :-------- | -----------: | -----------: | ---------: |
| MSFT   | IT        |        27.96 |        18.48 | \-33.90558 |
| KO     | Consumer  |        57.53 |        42.88 | \-25.46497 |
| XOM    | Energy    |        85.91 |        76.29 | \-11.19777 |
| BA     | Financial |        76.23 |        40.36 | \-47.05497 |
| WFC    | Financial |        28.45 |        14.23 | \-49.98243 |
| NTRS   | Financial |        66.08 |        43.93 | \-33.51998 |
| AIG    | Financial |        41.18 |         1.37 | \-96.67314 |
| MS     | Financial |        39.55 |        13.10 | \-66.87737 |
| AMZN   | Consumer  |        68.22 |        48.44 | \-28.99443 |
| GS     | Financial |       156.86 |        59.20 | \-62.25934 |

Table: Percentage price change between 14 Mar. 2008 and 20 Jan. 2009

<img src="code/stock-portfolio-analysis_files/figure-gfm/percentage-price-change-graph-1.png" style="display: block; margin: auto;" />

<img src="code/stock-portfolio-analysis_files/figure-gfm/price-dynamics-1.png" style="display: block; margin: auto;" />

<img src="code/stock-portfolio-analysis_files/figure-gfm/log-returns-graph-1.png" style="display: block; margin: auto;" />

| TICKER |    statistic | p.value | Normal |
| :----- | -----------: | ------: | :----- |
| BA     |       441,26 |       0 | No     |
| MSFT   |     1.457,46 |       0 | No     |
| GS     |     1.927,01 |       0 | No     |
| WFC    |     1.971,43 |       0 | No     |
| NTRS   |     2.104,45 |       0 | No     |
| AMZN   |     2.531,43 |       0 | No     |
| XOM    |     3.295,95 |       0 | No     |
| KO     |     3.459,27 |       0 | No     |
| MS     |    20.343,61 |       0 | No     |
| AIG    | 2.102.833,21 |       0 | No     |

Table: Jarque Bera Test, n. obs. = 756

<img src="code/stock-portfolio-analysis_files/figure-gfm/marginal-normality-QQplot-1.png" style="display: block; margin: auto;" />

| Ticker | Statistic | p-value | Alternative | t-Student |
| :----- | --------: | ------: | :---------- | :-------- |
| AIG    |    0,4170 |       0 | two-sided   | NO        |
| WFC    |    0,4389 |       0 | two-sided   | NO        |
| MS     |    0,4392 |       0 | two-sided   | NO        |
| NTRS   |    0,4503 |       0 | two-sided   | NO        |
| GS     |    0,4518 |       0 | two-sided   | NO        |
| AMZN   |    0,4556 |       0 | two-sided   | NO        |
| MSFT   |    0,4664 |       0 | two-sided   | NO        |
| BA     |    0,4687 |       0 | two-sided   | NO        |
| XOM    |    0,4688 |       0 | two-sided   | NO        |
| KO     |    0,4769 |       0 | two-sided   | NO        |

Table: One-sample Kolmogorov-Smirnov test for student-t distribution,
n. obs. = 756, dgf. = 10

<img src="code/stock-portfolio-analysis_files/figure-gfm/marginal-t-student-QQplot-1.png" style="display: block; margin: auto;" />

<table>

<caption>

Table: Log Returns Skewness and Kurtosis, n. obs. = 756

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Skewness

</th>

<th style="text-align:right;">

Kurtosis

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

BA

</td>

<td style="text-align:right;">

0,25

</td>

<td style="text-align:right;">

3,69

</td>

</tr>

<tr>

<td style="text-align:left;">

MSFT

</td>

<td style="text-align:right;">

0,35

</td>

<td style="text-align:right;">

6,74

</td>

</tr>

<tr>

<td style="text-align:left;">

GS

</td>

<td style="text-align:right;">

0,43

</td>

<td style="text-align:right;">

7,75

</td>

</tr>

<tr>

<td style="text-align:left;">

WFC

</td>

<td style="text-align:right;">

0,67

</td>

<td style="text-align:right;">

7,77

</td>

</tr>

<tr>

<td style="text-align:left;">

NTRS

</td>

<td style="text-align:right;">

0,18

</td>

<td style="text-align:right;">

8,14

</td>

</tr>

<tr>

<td style="text-align:left;">

AMZN

</td>

<td style="text-align:right;">

1,21

</td>

<td style="text-align:right;">

8,61

</td>

</tr>

<tr>

<td style="text-align:left;">

XOM

</td>

<td style="text-align:right;">

0,14

</td>

<td style="text-align:right;">

10,20

</td>

</tr>

<tr>

<td style="text-align:left;">

KO

</td>

<td style="text-align:right;">

0,75

</td>

<td style="text-align:right;">

10,34

</td>

</tr>

<tr>

<td style="text-align:left;">

MS

</td>

<td style="text-align:right;">

1,40

</td>

<td style="text-align:right;">

25,20

</td>

</tr>

<tr>

<td style="text-align:left;">

AIG

</td>

<td style="text-align:right;">

11,64

</td>

<td style="text-align:right;">

256,80

</td>

</tr>

</tbody>

</table>

<table>

<caption>

Table: Mardia test for Multivariate Normality.

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:left;">

p value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

28.530,781

</td>

<td style="text-align:left;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

462,258

</td>

<td style="text-align:left;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

<img src="code/stock-portfolio-analysis_files/figure-gfm/multivariate-normality-graph-1.png" style="display: block; margin: auto;" />

## Copulas

### Multivariate Copulas

### Bivariate Copulas

<img src="code/stock-portfolio-analysis_files/figure-gfm/pairwise-scatter-financials-1.png" style="display: block; margin: auto;" />

<table class="kable_wrapper">

<tbody>

<tr>

<td>

<table>

<caption>

Normality test between: BA-WFC

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

105,76

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

44,62

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: BA-NTRS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

47,25

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

48,27

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: BA-AIG

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

20.346,53

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

991,90

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: BA-MS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

305,84

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

103,65

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: BA-GS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

62,90

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

48,98

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: WFC-NTRS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

95,44

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

76,17

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: WFC-AIG

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

22.091,12

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

1.060,78

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: WFC-MS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

566,85

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

180,16

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: WFC-GS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

133,46

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

68,20

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: NTRS-AIG

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

20.380,86

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

1.008,30

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: NTRS-MS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

396,38

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

181,75

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: NTRS-GS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

59,37

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

80,46

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: AIG-MS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

23.664,08

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

1.147,44

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: AIG-GS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

21.703,57

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

1.037,07

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Normality test between: MS-GS

</caption>

<thead>

<tr>

<th style="text-align:left;">

Test

</th>

<th style="text-align:right;">

Statistic

</th>

<th style="text-align:right;">

P.value

</th>

<th style="text-align:left;">

Result

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Mardia Skewness

</td>

<td style="text-align:right;">

380,49

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

<tr>

<td style="text-align:left;">

Mardia Kurtosis

</td>

<td style="text-align:right;">

159,79

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

NO

</td>

</tr>

</tbody>

</table>

</td>

</tr>

</tbody>

</table>

<div class="figure" style="text-align: center">

<img src="code/stock-portfolio-analysis_files/figure-gfm/empirical-scatter-pseudo-observations-1.png" alt="Figure (a): Pseudo observations (uniform margins) obtained from actual data, with non parametric estimation of the CDFs."  />

<p class="caption">

Figure (a): Pseudo observations (uniform margins) obtained from actual
data, with non parametric estimation of the CDFs.

</p>

</div>

<img src="code/stock-portfolio-analysis_files/figure-gfm/multivariate-student-t-simulation-1.png" style="display: block; margin: auto;" /><img src="code/stock-portfolio-analysis_files/figure-gfm/multivariate-student-t-simulation-2.png" style="display: block; margin: auto;" />

<table>

<caption>

Table: MLE results of four different multivariate copula of the
Financial stocks

</caption>

<thead>

<tr>

<th style="text-align:right;">

Normal

</th>

<th style="text-align:right;">

Student

</th>

<th style="text-align:right;">

Gumbel

</th>

<th style="text-align:right;">

Clayton

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1.256,09

</td>

<td style="text-align:right;">

1.667,14

</td>

<td style="text-align:right;">

944,6

</td>

<td style="text-align:right;">

944,6

</td>

</tr>

</tbody>

</table>

<div class="figure" style="text-align: center">

<img src="code/stock-portfolio-analysis_files/figure-gfm/histograms-pseudo-obs-1.png" alt="Figure: Pseudo-observations Distribution"  />

<p class="caption">

Figure: Pseudo-observations Distribution

</p>

</div>

## Portfolio Value-at-Risk

<img src="code/stock-portfolio-analysis_files/figure-gfm/portfolio-value-1.png" style="display: block; margin: auto;" />
<img src="code/stock-portfolio-analysis_files/figure-gfm/portfolio-weights-1.png" style="display: block; margin: auto;" />

<img src="code/stock-portfolio-analysis_files/figure-gfm/portfolio-profit-loss-1.png" style="display: block; margin: auto;" />

<table>

<caption>

95% Value-at-Risk calculated on 31 December 209 for a one-day horizon
under parametric and non-parametric appproach.

</caption>

<thead>

<tr>

<th style="text-align:right;">

Gaussian VaR

</th>

<th style="text-align:right;">

Historical VaR

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

0,0391

</td>

<td style="text-align:right;">

0,0346

</td>

</tr>

</tbody>

</table>

<img src="code/stock-portfolio-analysis_files/figure-gfm/VAR-1.png" style="display: block; margin: auto;" />

<table class="kable_wrapper">

<tbody>

<tr>

<td>

<table>

<caption>

Number violations for 2010 returns under Gaussian VaR

</caption>

<thead>

<tr>

<th style="text-align:left;">

Violation

</th>

<th style="text-align:right;">

n

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

250

</td>

</tr>

<tr>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

2

</td>

</tr>

</tbody>

</table>

</td>

<td>

<table>

<caption>

Number violations for 2010 returns under Historical VaR

</caption>

<thead>

<tr>

<th style="text-align:left;">

Violation

</th>

<th style="text-align:right;">

n

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

NO

</td>

<td style="text-align:right;">

249

</td>

</tr>

<tr>

<td style="text-align:left;">

YES

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

</td>

</tr>

</tbody>

</table>

<img src="code/stock-portfolio-analysis_files/figure-gfm/var-backtesting-1.png" style="display: block; margin: auto;" />
