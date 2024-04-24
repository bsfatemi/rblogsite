source("posts/_draft/Top Brands Daily/pub/R/brands-daily.R")

# Run Script ----------------------------------------------------------------------------------

run_build_plot_data <- function(as_of_date) {
  population  <- dbGetPop(as_of_date)
  brandsDaily <- build_brands_daily(population, as_of_date)
  ## Top Brands by Average Daily Sales ($)
  pdata <- dcast(
    brandsDaily[, .(brand_name, period, ave_sales_daily, pct_growth)],
    brand_name + pct_growth ~ period,
    value.var = "ave_sales_daily"
  )
  return(pdata)
}

pdata <- run_build_plot_data(today() - days(3))
pdata
