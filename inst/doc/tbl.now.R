## ----include = FALSE--------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  # `html_vignette` renders at 96 dpi, which for the wide multi-panel figures
  # below produces PNGs wider than the 700px the vignette CSS will ever show
  # and inflates the installed size of `doc/` for pixels nobody sees. 72 dpi
  # keeps every figure at essentially its displayed width.
  dpi = 72
)
options(tibble.width = Inf, width = 120, pillar.width = 120)
options(pillar.print_max = 6, pillar.print_min = 6,
        tibble.print_min = 6, dplyr.print_min = 6,
        tibble.print_max = 6, dplyr.print_max = 6)

## ----workflow-diagram, echo = FALSE, fig.width = 7, fig.height = 6, out.width = "100%", fig.alt = "Flow diagram read top to bottom. Clean your data leads to tbl_now(), which leads to diagnose() and summary(), which lead to autoplot(). A dashed arrow loops back from diagnose() to the cleaning step, labelled fix what it finds. Below autoplot() the path forks in two: left to tbl_now_to_*(), labelled hand it to another package, and right to run_nowcast(engine()), which leads on to nowcast_backtest() and score_nowcast()."----
library(ggplot2)
pal <- tbl.now::tbl_now_palette()

boxes <- data.frame(
  x     = c(5.0, 5.0, 5.0, 5.0, 2.2, 7.8, 7.8),
  y     = c(9.3, 7.7, 6.1, 4.5, 2.6, 2.6, 0.9),
  hw    = c(1.9, 1.3, 1.9, 1.3, 2.0, 2.1, 2.1),
  hh    = c(0.55, 0.4, 0.55, 0.4, 0.4, 0.4, 0.55),
  label = c(
    "Clean your data\n(dplyr, as usual)", 
    "State the `tbl_now()`",
    "Diagnose data problems\n`diagnose()`",
    "Describe\n(`summary()`, `autoplot()`, ...)",
    "tbl_now_to_*()", 
    "Nowcast\n`run_nowcast(engine())`",
    "Evaluate\n`nowcast_backtest()`\n`score_nowcast()`"
  ),
  fill = c(
    pal[["reporting_light"]], pal[["reporting_light"]], pal[["reporting_light"]],
    pal[["reporting_light"]], pal[["reporting_light"]], pal[["reporting_light"]],
    pal[["reporting_light"]]
  ),
  ink = c(
    pal[["ink"]], pal[["ink"]], pal[["ink"]],
    pal[["ink"]], pal[["ink"]], pal[["ink"]],
    pal[["ink"]]
  )
)

arrows <- data.frame(
  x    = c(5.0, 5.0, 5.0, 4.6, 5.4, 7.8),
  y    = c(8.75, 7.30, 5.55, 4.10, 4.10, 2.20),
  xend = c(5.0, 5.0, 5.0, 2.6, 7.4, 7.8),
  yend = c(8.25, 6.65, 4.90, 3.10, 3.10, 1.45)
)

ggplot() +
  geom_segment(
    data = arrows, aes(x = x, y = y, xend = xend, yend = yend),
    colour = pal[["guide_strong"]], linewidth = 0.45,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  geom_curve(
    aes(x = 3.05, y = 6.1, xend = 3.05, yend = 9.0),
    colour = pal[["guide_strong"]], linewidth = 0.4, linetype = "22",
    curvature = -0.55, ncp = 12,
    arrow = arrow(length = unit(0.15, "cm"), type = "closed")
  ) +
  annotate("text", x = 1.35, y = 7.6, angle = 90, label = "fix what it finds",
           colour = pal[["ink"]], size = 3, fontface = "bold") +
  annotate("text", x = 2.2, y = 1.7, label = "hand it to another package",
           colour = pal[["ink"]], size = 3, fontface = "bold") +
  geom_label(
    data = boxes, aes(x = x, y = y, label = label),
    colour = boxes$ink, fill = boxes$fill, 
  ) +
  scale_x_continuous(limits = c(-0.1, 10.1)) +
  scale_y_continuous(limits = c(0.1, 10.0)) +
  theme_void()

## ----setup, message = FALSE-------------------------------------------------------------------------------------------
library(dplyr)
library(tbl.now)

data(denguedat)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------
# denguedat

## ----echo = FALSE-----------------------------------------------------------------------------------------------------
tibble(denguedat)

## ----declare, message = TRUE------------------------------------------------------------------------------------------
#For this example, we filter the data to keep only those cases that happened 
#on 2005 and were reported before October 2005
denguedat <- denguedat |>
  filter(onset_week >= as.Date("2005-01-01") & report_week <= as.Date("2005-10-01")) 

#We then create the tbl_now object
dengue <- denguedat |>
  tbl_now(
    event_date  = onset_week,  
    report_date = report_week
  )

dengue

## ----diagnose---------------------------------------------------------------------------------------------------------
diagnose(dengue)

## ----summary----------------------------------------------------------------------------------------------------------
summary(dengue) 

## ----autoplot, fig.width = 9, fig.height = 7, out.width = "100%", fig.alt = "A grid of diagnostic panels for the dengue data, with the epidemic process in the left column and the reporting process in the right column."----
autoplot(dengue)

## ----converters, warning = FALSE--------------------------------------------------------------------------------------
#Transform to a baselinenowcast reporting triangle
triangle <- tbl_now_to_baselinenowcast(dengue)

#This is now a reporting triangle:
triangle[(nrow(triangle) - 5):nrow(triangle), 1:7]

## ----eval=FALSE-------------------------------------------------------------------------------------------------------
# library(baselinenowcast)
# 
# #and nowcast within the framework
# baselinenowcast(triangle)

## ----nowcast, fig.width = 9, fig.height = 4, out.width = "100%", fig.alt = "Nowcast of dengue cases by onset week and gender: observed counts as bars with the predicted median and interval overlaid on the most recent weeks."----
#Change to engine_diseasenowcasting() if possible
fit <- run_nowcast(dengue, example_engine())

#Visualize the results
autoplot(fit)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------
# tidy(fit)

## ----echo = FALSE-----------------------------------------------------------------------------------------------------
tidy(fit) |> tail(5)

## ----backtest---------------------------------------------------------------------------------------------------------
backtest <- nowcast_backtest(
  dengue,
  example_engine(),
  now_dates = as.Date(c("2005-08-07", "2005-09-04")), #Evaluate at these two dates
  verbose = FALSE
)

backtest

## ----learning-more, echo=FALSE, results="asis"------------------------------------------------------------------------
cat(
  knitr::knit_child(
    system.file("fragments", "learning-more.Rmd", package = "tbl.now"),
    quiet = TRUE
  ),
  sep = "\n"
)

