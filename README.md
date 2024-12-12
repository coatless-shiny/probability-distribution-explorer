# Probability Distributions Explorer

The Probability Distributions Explorer is a web-based application that helps
users understand various statistical distributions through visualization 
and interactive parameter manipulation. The application is built with
R Shiny and deployed using shinylive. Check out the live application at
<https://shiny.thecoatlessprofessor.com/probability-distribution-explorer/>

With the shiny app, you can:

- Visualize distributions: View the probability density function and cumulative distribution function
- Distribution comparison: Compare any two distributions side by side
- Probability region calculation: Calculate probabilities for specific intervals
- Central Limit Theorem demonstration: Visualize the sampling distribution of means

## Deployment

This application is deployed using shinylive, allowing it to run directly in
the browser without requiring an R server. Shinylive converts the R code to 
WebAssembly, making it possible to run R applications entirely client-side.

## Local Development Environment

1. Clone the repository:

```bash
git clone https://github.com/coatless-shiny/probability-distributions-explorer.git
```

2. Open the `probability-distributions-explorer.Rproj`

3. Install required R packages:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "bslib", "shinylive"))
```

4. Run the application:

```r
shiny::runApp()
```

5. Check if the application can be converted to `{shinylive}`:

```r
shinylive::export(".", "_site")
```

## Acknowledgments

- Built using the R Shiny framework
- Uses the bslib package for Bootstrap 5 theming
- Deployed using shinylive for browser-based execution
