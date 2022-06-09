dirYear <- "2022"
dirProject <- "2022-06-07-berkah-ramadan-belum-kembali-ke-retail"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(paletteer)
library(ggtext)
library(dfrtheme)


# Helper ----

lshi_geom_vline_zero <- function(.lwd = 12/22, ...) {
  ggplot2::geom_vline(
    xintercept = 0, 
    lwd = .lwd, 
    ...
  )
}

lshi_geom_point <- function(color = "white") {
  ggplot2::geom_point(
    ggplot2::aes(fill = z_score > 0),
    size = 4,
    pch = 21,
    color = color,
    show.legend = FALSE
  )
}

lshi_annotate_text <- function(.color = "#757575", ...) {
  ggtext::geom_richtext(
    data = tibble::tibble(
      x = c(-1, 1),
      y = c("<b>Keseluruhan</b>", "Barang budaya & rekreasi"),
      label = c("&larr; Di bawah normal", "Di atas normal &rarr;")
    ),
    mapping = ggplot2::aes(x = x, y = y, label = label),
    size = dfrtheme::dfr_convert_font_size(),
    color = .color,
    label.padding = ggplot2::unit(0, "cm"),
    label.size = NA,
    ...
  )
}

lshi_apply_palette <- function(palette = "ggthemes::Tableau_20") {
  
  rsiPalette <- paletteer::paletteer_d(palette)
  
  valuePalette <- c("TRUE" = rsiPalette[[9]], "FALSE" = rsiPalette[[3]])
  
  list(
    ggplot2::scale_fill_manual(values = valuePalette),
    ggplot2::scale_color_manual(values = valuePalette)
  )  
  
}


# Palette ----

rsiPalette <- paletteer_d("ggthemes::Tableau_20")


# Plot ----

rsiZscore <- read_csv(here(dirYear, dirProject, "result", "rsi-z-score.csv"))

rsiZscorePrep <- rsiZscore |> 
  mutate(
    category_idn = str_replace(
      category_idn, 
      "Keseluruhan",
      "<b>Keseluruhan</b>"
    ),
    category_idn = fct_reorder(category_idn, z_score)
  )


## Base plot ----

plotBase <- ggplot(rsiZscorePrep, aes(z_score, category_idn)) +
  geom_vline(
    xintercept = c(-1, 1),
    lwd = 12/22,
    color = c(rsiPalette[[4]], rsiPalette[[10]])
  ) +
  geom_segment(
    aes(xend = 0, yend = category_idn, color = z_score > 0),
    lwd = 2,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(-3, 3),
    limits = c(-3, 3),
    position = "top"
  ) +
  lshi_apply_palette() +
  labs(
    title = "Penjualan eceran belum pulih sepenuhnya di tengah Ramadan",
    subtitle = paste0(
      "Jarak antara indeks penjualan riil dan rata-ratanya dalam 10 tahun ",
      "terakhir, April 2022\\*<br>(dalam simpangan baku)"
    ),
    x = NULL,
    y = NULL,
    caption = paste0(
      "\\*Angka sementara<br>",
      "Sumber: Bank Indonesia; Dzulfiqar Fathur Rahman<br>",
      "Grafik: Dzulfiqar Fathur Rahman"
    ) 
  ) 


## Light theme ----

plotLight <- plotBase +
  lshi_geom_vline_zero() +
  lshi_geom_point() +
  lshi_annotate_text() +
  dfr_theme() +
  theme(
    axis.text.y = element_markdown(hjust = 0),
    panel.grid.major.y = element_blank()
  )

ggsave(
  here(dirYear, dirProject, "result", "rsi-z-score-light.svg"),
  plot = plotLight,
  width = 8,
  height = 4.5,
  dpi = 96
)


## Dark theme ----

darkPaletteMain <- "#000f2b"
darkPaletteText <- "grey75"

plotDark <- plotBase +
  lshi_geom_vline_zero(.lwd = 0.75, color = "white") +
  lshi_geom_point(color = darkPaletteMain) +
  lshi_annotate_text(.color = darkPaletteText, fill = darkPaletteMain) +
  dfr_theme() %+replace%
  theme(
    axis.text.x = element_markdown(
      size = rel(0.9166667),
      color = darkPaletteText
    ),
    axis.text.y = element_markdown(
      size = rel(0.9166667),
      color = darkPaletteText,
      hjust = 0
    ),
    plot.title = element_markdown(
      face = "bold",
      hjust = 0,
      size = rel(1.166667),
      color = "white",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_markdown(
      hjust = 0,
      size = rel(1.041667),
      color = darkPaletteText,
      margin = margin(b = 10)
    ),
    plot.caption = element_markdown(
      hjust = 0,
      size = rel(0.875),
      color = "grey50",
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = darkPaletteMain, color = NA),
    panel.grid.major.x = element_line(color = "grey50"),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = darkPaletteMain, color = NA)
  )

ggsave(
  here(dirYear, dirProject, "result", "rsi-z-score-dark.svg"),
  plot = plotDark,
  width = 8,
  height = 4.5,
  dpi = 96
)