require(ggplot2, quietly = TRUE)
require(aniMotum, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(sf, quietly = TRUE)
require(hexSticker, quietly = TRUE)
require(cowplot, quietly = TRUE)

sese <- readRDS("data-raw/sese.RDS")
fit <- fit_ssm(sese,
               model = "crw",
               time.step = 6,
               control = ssm_control(verbose = 0))

prj <- "+proj=stere +lon_0=68 +units=km +datum=WGS84"

d <- grab(fit, "data", as_sf = TRUE) |>
  st_transform(crs = prj)
p <- grab(fit, "predicted", as_sf = TRUE) |>
  st_transform(crs = prj)

p.tracks <- p |>
  group_by(id) |>
  summarise(do_union = FALSE) |>
  st_cast("MULTILINESTRING")

bb <- st_bbox(d %>% st_transform(crs = prj))
bb[c("xmin","xmax")] <- extendrange(r=c(bb["xmin"]-1200, bb["xmax"]+200), f=0.01)
bb[c("ymin","ymax")] <- extendrange(r=c(bb["ymin"]-200, bb["ymax"]-50), f=0.01)


m <- ggplot() +
  geom_sf(data = d, col="white", size=1) +
  geom_sf(data = p.tracks, col="firebrick", linewidth = 0.75) +
#  geom_sf(data = p, col="firebrick", size=0.2) +
  coord_sf(xlim = bb[c("xmin","xmax")],
           ylim = bb[c("ymin","ymax")],
           expand = FALSE,
           crs = prj) +
  theme_minimal() +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme(legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.1),
        panel.grid.minor = element_line(linewidth = 0.1),
        axis.text = element_blank())

m1 <- ggdraw() +
  # draw_image("inst/logo/img/sese_male.png",  x=0.5, y=0.82, scale=0.275, hjust=0.5, vjust=0.5, interpolate=FALSE) +
  # draw_image("inst/logo/img/huwh2.png",  x = 0.78, y = 0.78, scale=0.25, hjust=0.5, vjust=0.5) +
  # draw_image("inst/logo/img/whsh2.png",  x = 0.27, y = 0.79, scale = 0.16, hjust=0.5, vjust=0.5) +
  # draw_image("inst/logo/img/lbtu2.png", x = 0.9, y = 0.725, scale = 0.125, hjust=0.5, vjust=0.5) +
  # draw_image("inst/logo/img/kipe2.png",  x = 0.15, y = 0.735, scale = 0.1, hjust=0.5, vjust=0.5) +
  draw_image("inst/logo/img/satellite-128.png",  x = 0.53, y = 1.2, scale=0.15, hjust=0.5, vjust=0.5) +
  draw_plot(m, x = 0, y = 0.075, scale = 0.95)

s <- sticker(
  m1,
  package = "ArgosQC",
  p_size = 48,
  p_y = 1.6,
  p_family = "sans",
  p_color = "white",
  h_color = "white",
  h_fill =  "#045a8d",
  h_size = 1.3,
  s_x = 0.945,
  s_y = 0.775,
  s_width = 1.5,
  s_height = 1.5,
  spotlight = FALSE,
  l_x = 0.94,
  l_y = 1.08,
  l_width = 2,
  filename = "man/figures/logo.png",
  url = "https://ianjonsen.github.io/ArgosQC/",
  u_x = 0.185,
  u_y = 0.53,
  u_color = "white",
  u_size = 16,
  u_angle = -30,
  dpi = 300
)
## overwrite to get dpi correct
ggsave(s, file = "man/figures/logo.png")
