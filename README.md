# teal.osprey

The teal.osprey package provides community contributed `teal` modules of the analysis functions from the [osprey](https://insightsengineering.github.io/osprey) R package.
This enables `teal` app developers to easily create applications making use of the `osprey` analysis functions.

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.osprey@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

In order to run many of the examples you will also need to install the [`scda`](https://insightsengineering.github.io/scda) package.

[![Stargazers repo roster for @insightsengineering/teal.osprey](https://reporoster.com/stars/insightsengineering/teal.osprey)](https://github.com/insightsengineering/teal.osprey/stargazers)
[![Forkers repo roster for @insightsengineering/teal.osprey](https://reporoster.com/forks/insightsengineering/teal.osprey)](https://github.com/insightsengineering/teal.osprey/network/members)

## Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.osprey.svg)](https://starchart.cc/insightsengineering/teal.osprey)
