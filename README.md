# The 3-Point Revolution 🏀
### How the NBA Reinvented Itself

An interactive R Shiny app exploring the NBA's dramatic shift toward three-point shooting over the past two decades, built for ST 437 Data Visualization at Oregon State University.

## 🔗 Live App
[View the app on shinyapps.io](https://andynyugen.shinyapps.io/final-project/)

## 📖 About
Over the past two decades, the NBA has systematically abandoned the mid-range jumper in favor of three-point shots and at-rim attempts. This app explores that shift using play-by-play and team box score data from 2002 to 2026, sourced through the `hoopR` R package.

## 📊 Features
- **League Trend** — tracks the league-wide three-point attempt rate from 2002 to 2026
- **Wins vs 3PA** — scatter plot exploring the relationship between three-point volume and team wins
- **Shot Breakdown** — stacked bar chart showing how shot selection has changed across four eras
- Interactive season range slider and team filter across all three tabs
- Hover tooltips on all visualizations

## 🗂️ Data
Data is sourced through the [`hoopR`](https://hoopr.sportsdataverse.org/) R package, which pulls historical NBA play-by-play and team box score data from the ESPN NBA Stats API. The dataset covers over 14 million plays across 24 NBA seasons (2002–2026).

## 🛠️ Built With
- [R](https://www.r-project.org/)
- [Shiny](https://shiny.posit.co/)
- [bslib](https://rstudio.github.io/bslib/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [plotly](https://plotly.com/r/)
- [hoopR](https://hoopr.sportsdataverse.org/)
- [tidyverse](https://www.tidyverse.org/)

## 🚀 Running Locally
1. Clone the repository
```r
git clone https://github.com/yourusername/your-repo-name.git
```
2. Install required packages
```r
install.packages(c("shiny", "bslib", "shinythemes", "tidyverse", "plotly", "hoopR", "rsconnect"))
```
3. Run the app
```r
shiny::runApp()
```

## 👤 Author
Andy Nguyen — Oregon State University, ST 437 Data Visualization
