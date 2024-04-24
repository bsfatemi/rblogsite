

plot_base_layer <- function(salesDT) {
  p_theme <- function() {
    theme_hc() +
      theme(
        plot.title.position = "plot",
        axis.title = element_blank(),
        legend.position = "right",
        legend.direction = "vertical",
        legend.justification = c(1, 1)
      )
  }

  process_image <- function(url, sku, outdir = "images/products") {
    dir_path <- fs::dir_create("images/products", recurse = TRUE)
    image_read(url) |>
      image_resize("200x200") |>
      image_crop("185x185") |>
      image_trim(20) |>
      image_background("#FFF", flatten = TRUE) |>
      image_convert(format = "jpeg") |>
      image_write(path = str_glue("{dir_path}/{sku}.jpeg"))
  }

  b0 <- ggplot(salesDT, aes(x = wts)) +
    geom_ribbon(aes(ymin = c_sales_actual, ymax = c_sales_est, fill = "opportunity"), alpha = .7) +
    geom_segment(aes(xend = wts, y = c_sales_actual, yend = c_sales_est), linetype = "dotted") +
    geom_point(aes(y = c_sales_est, color = "estimated"), shape = 18) +
    geom_line(aes(y = c_sales_est, color = "estimated")) +
    geom_point(aes(y = c_sales_actual, color = "actual", shape = !has_sales), size = 2) +
    scale_y_continuous(name = NULL, labels = scales::dollar_format(accuracy = 1)) +
    scale_x_continuous(name = NULL, labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(
      name = "Cumulative Sales",
      values = c("#e23a2c", "#638ffb"),
      labels = c("Actual", "Expected")
    ) +
    scale_fill_manual(
      name = "Stockout Impact",
      values = c("#69cf48"),
      labels = "Lost Sales Dollars"
    ) +
    scale_shape_manual(
      name = "Stock Disruption",
      values = c(16, 4),
      labels = c("No Stockout", "Has Stockout")
    ) +
    guides(
      color = guide_legend(order = 1),
      shape = guide_legend(order = 2),
      fill = guide_legend(order = 3)
    ) +
    p_theme()

  ##
  ## Add image if available
  ##

  img_path <- tryCatch({
    process_image(salesDT[1, image_url], salesDT[1, product_sku])
  }, error = function(c) {
    warning(str_glue("Unable to process image for sku {sku}"), call. = FALSE)
    return(NULL)
  })

  if (!is.null(img_path)) {
    b0 <- b0 +
      geom_richtext(aes(x = 1, y = 0),
                    label = str_glue("<img src='{img_path}' width='125'/>"),
                    hjust = -.3,
                    vjust = 0,
                    color = NA) +
      coord_cartesian(xlim = c(0,1), clip = "off")
  }
  return(b0)
}


add_guide <- function(p, labDT, ax = .5) {
  tot_impact <- labDT[, scales::dollar(c_sales_est - c_sales_actual, accuracy = 1)]
  p +
    geom_linerange(
      data = labDT,
      aes(xmin = 0, xmax = ax, y = c_sales_est)
    ) +
    geom_linerange(
      data = labDT,
      aes(xmin = 0, xmax = ax, y = c_sales_actual)
    ) +
    geom_linerange(
      data = labDT,
      aes(x = ax, ymin = 0, ymax = c_sales_est)
    ) +
    geom_segment(
      data = labDT,
      aes(x = 0, xend = 0, y = c_sales_actual, yend = c_sales_est),
      arrow = arrow(length = unit(0.15, "inches"), ends = "both", type = "closed"),
      arrow.fill = "#ffa412"
    ) +
    geom_textbox(
      data = labDT[, lab_y_est := str_glue(
        "*y<sub>estimate</sub>= {scales::dollar(c_sales_est, accuracy = 1)}*",
        .envir = .SD
      )],
      aes(x = 0, y = c_sales_est + 1, label = lab_y_est),
      hjust = 0,
      vjust = -0.05,
      box.color = NA,
      fill = NA,
      width = NULL
    ) +
    geom_textbox(
      data = labDT[, lab_y_act := str_glue(
        "*y<sub>actual</sub>= {scales::dollar(c_sales_actual, accuracy = 1)}*",
        .envir = .SD
      )],
      aes(x = 0, y = c_sales_actual, label = lab_y_act),
      hjust = 0,
      vjust = 1,
      box.color = NA,
      fill = NA,
      width = NULL
    ) +
    geom_textbox(
      data = labDT[, lab_x := str_glue("*x = {ifelse(ax == .5, '45 days', '90 days')}*")],
      aes(x = ax, y = 0, label = lab_x),
      hjust = 1,
      vjust = 0,
      box.color = NA,
      fill = NA,
      width = NULL
    ) +
    geom_textbox(
      data = labDT,
      aes(x = 0, y = (c_sales_est + c_sales_actual) * .5),
      label = str_glue("<span><b>Difference</b>=
                     **<span style = 'color:#00a651;'>{tot_impact}</span></span>**<br>
                     (*y<sub>estimate</sub> - y<sub>actual</sub>*)"),
      hjust = -.05,
      box.color = "#ffa412",
      lineheight = 1,
      fill = "cornsilk"
    ) +
    theme(
      plot.title = element_textbox_simple(
        size = 13,
        lineheight = 1,
        padding = margin(5.5, 5.5, 5.5, 5.5),
        margin = margin(0, 0, 5.5, 0),
        fill = "cornsilk"
      )
    )
}


plot_mid_period <- function(b0, salesDT) {

  lab50 <- salesDT[wts > .49 & wts < .51, .(
    wts = .5,
    c_sales_est = mean(c_sales_est),
    c_sales_actual = mean(c_sales_actual)
  )]

  tot_impact_50 <- lab50[, scales::dollar(c_sales_est - c_sales_actual, accuracy = 1)]
  pct_impact_50 <- lab50[, scales::percent(c_sales_actual / c_sales_est, accuracy = 1)]

  add_guide(p = b0, labDT = lab50, ax = .5) +
    labs(
      title = str_glue("<b>Impact of Stockouts on Total Product Sales</b><br>
    <span style = 'font-size:10pt'>
    Expecting **<span style = 'color:#00a651;'>+{tot_impact_50}</span>**
    in additional revenue generated by day 45 **without stockout events**<br>
    This means revenues for this product are **<span style = 'color:red;'>{pct_impact_50}</span>**
    of the prediction **but for historical stockouts**</span>"
      ))
}


plot_end_period <- function(b0, salesDT) {

  lab100 <- salesDT[wts > .98 & wts < 1, .(
    wts = 1,
    c_sales_est = mean(c_sales_est),
    c_sales_actual = mean(c_sales_actual)
  )]

  tot_impact_100 <- lab100[, scales::dollar(c_sales_est - c_sales_actual, accuracy = 1)]
  pct_impact_100 <- lab100[, scales::percent(c_sales_actual / c_sales_est, accuracy = 1)]

  add_guide(p = b0, labDT = lab100, ax = 1) +
    labs(
      title = str_glue("<b>Impact of Stockouts on Total Product Sales</b><br>
    <span style = 'font-size:10pt'>
    Expecting **<span style = 'color:#00a651;'>+{tot_impact_100}</span>**
    in additional revenue generated by day 90 **without stockout events**<br>
    This means revenues for this product are **<span style = 'color:red;'>{pct_impact_100}</span>**
    of the prediction **but for historical stockouts**</span>"
      ))
}
