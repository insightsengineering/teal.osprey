# teal.osprey

<!-- start badges -->
[![Release 🎈](https://github.com/insightsengineering/teal.osprey/actions/workflows/release.yaml/badge.svg)](https://github.com/insightsengineering/teal.osprey/releases)
[![Check 🛠](https://github.com/insightsengineering/teal.osprey/actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering/teal.osprey/actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering/teal.osprey/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.osprey/)

[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.osprey/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/teal.osprey/_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.osprey?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/teal.osprey?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.osprey)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.osprey)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.osprey)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.osprey)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.osprey)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.osprey)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.osprey/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.osprey/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.osprey?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.osprey/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

The teal.osprey package provides community contributed `teal` modules of the analysis functions from the [osprey](https://insightsengineering.github.io/osprey/) R package.
This enables `teal` app developers to easily create applications making use of the `osprey` analysis functions.

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.osprey@*release")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

In order to run many of the examples you will also need to install the [`scda`](https://insightsengineering.github.io/scda/) package.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.osprey.svg)](https://starchart.cc/insightsengineering/teal.osprey)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.osprey](https://reporoster.com/stars/insightsengineering/teal.osprey)](https://github.com/insightsengineering/teal.osprey/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.osprey](https://reporoster.com/forks/insightsengineering/teal.osprey)](https://github.com/insightsengineering/teal.osprey/network/members)
