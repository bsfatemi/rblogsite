library(hcapipelines2)
library(data.table)
library(hcadscore)
library(ggplot2)
library(ggtext)
library(fst)
library(lubridate)

options(scipen = 1000)

cn <- dbc("prod2", "integrated")

qry <- "
SELECT org_uuid, store_id, order_time_utc, item_total
FROM population2
WHERE item_total > 1
AND order_time_utc > '2022-10-01'
AND order_time_utc < '2023-10-31'
"

DT <- setDT(DBI::dbGetQuery(cn, qry))
dbd(cn)



## plot cumulative growth rate by week
plot_weekly_cgr <- function(DT) {
  pdt <- setkey(DT, org_uuid, store_uuid)[]


  tmp <- pdt[, .(
    start_year = year(min(order_time_utc)),
    start_month = month(min(order_time_utc)),
    stop_year = year(max(order_time_utc)),
    stop_month = month(max(order_time_utc))
  ), keyby = .(org_uuid, store_uuid)][
    start_year == 2022 & start_month == 10 &
      stop_year == 2023 & stop_month == 10
  ][, .(org_uuid, store_uuid)]

  salesPerPeriod <- pdt[tmp, .(
        total_sales = sum(item_total),
        total_items = .N
      ), keyby = .(
        org_uuid,
        store_uuid = store_uuid,
        period_date = floor_date(order_time_utc, unit = "week")
      )][, id := .GRP, c("org_uuid", "store_uuid")]

  pll <- split(salesPerPeriod, by = "id", keep.by = TRUE)


  pdata <- rbindlist(lapply(pll, function(ss) {
    dt <- ss[c(-1, -.N)]
    dt[, c("gr_sales", "gr_items") := .(
      c(1, cumprod(1 + diff(total_sales) / total_sales[-.N])),
      c(1, cumprod(1 + diff(total_items) / total_items[-.N]))
    )][, .(id, period_date, gr_sales, gr_items)]
  }))

  var <- "gr_sales"
  plotDT <- pdata[get(var) < 2 & get(var) > .25, .(
    ave_gr = mean(get(var)),
    std_gr = sd(get(var)),
    q20_gr = quantile(get(var), .20),
    q50_gr = quantile(get(var), .50),
    q80_gr = quantile(get(var), .80)
  ), period_date][ave_gr < 1.05]

  dropIndex <- c(1, plotDT[period_date < "2023-07-01" & q50_gr < .9, which = TRUE])

  dt <- plotDT[-dropIndex][, .(period_date, y = q50_gr)][y > .85 & y < 1.01][, x := .I]

  fit <- nls(y ~ a*log(x)^1.5 + b, data = dt, start = list(a = 1, b = 1))
  fitvals <- 1 / rev(fit$m$fitted())
  fitvals <- fitvals - (fitvals[1] - (dt[1, y]))
  dt[, fit := fitvals]
  # dt[, i := .I]

  xlabDT <- dt[, .(
    date = as.Date(period_date)
  )][, c("yr", "mon") := .(
    stringr::str_remove(year(date), "^20"),
    month.abb[month(date)]
  )][, .GRP, .(label = paste(mon, yr))][-.N]



  xlabDT[, brk := seq(2, nrow(dt), by = 4)]


  ggplot(dt, aes(x = x)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = y)) +
    geom_line(aes(x = x, y = fit, color = "Sales Trend"),
              linewidth = 1) +
    scale_color_manual(name = NULL, values = "#DB14BF") +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = c(0.02, 0)
    ) +
    scale_x_continuous(
      breaks = xlabDT$brk,
      labels = xlabDT$label,
      expand = c(.02, 0)
    ) +
    labs(
      title = "<b><span style= 'color:#041E39;'>Average Weekly Change in Sales
      Across 118 Retailers</span></b><br>
      *<span style= 'font-size:12pt;color:#041E39;'>
      Change in Sales Relative to Total Sales in Sep 22</span>*
      ",
      caption = "A value of 87.5% means sales is 87.5% of the total sales in Sep 2022"
    ) +
    theme(
      axis.title = element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "none",
      plot.title = element_textbox_simple(
        color = "#041E39",
        box.color = "#041E39",
        size = 13,
        linetype = 1,
        lineheight = 1.1,
        padding = margin(5.5, 5.5, 5.5, 5.5),
        margin = margin(0, 0, 5.5, 0),
        fill = "#E0ECF9"
      ),
      plot.caption = element_textbox(
        size = 9,
        face = "italic",
        padding = margin(5.5, 5.5, 5.5, 5.5),
        margin = margin(5, 0, 5.5, 0),
        colour = "white",
        fill = "#DB14BF"
      ),
      axis.text = element_textbox(
        size = 10,
        color = "#041E39"
      ),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(
        color = "white",
        linewidth = .25
      ),
      panel.grid.major.y = element_line(
        colour = "#167ED3",
        linewidth = .25,
        linetype = 2
      )
    )
}


setnames(DT, "store_id", "store_uuid")
plot_weekly_cgr(DT)


