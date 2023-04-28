
pacman::p_load(
    tidyverse,
    here,
    eventstudyr,
    hexSticker
)

color_point_border <- "#4477AA"
color_interior <- "#EE7733"

estimates_ols <- EventStudy(
    estimator = "OLS",
    data = example_data,
    outcomevar = "y_jump_m",
    policyvar = "z",
    idvar = "id",
    timevar = "t",
    post = 3,
    pre = 0)

plt_ols <- EventStudyPlot(
    estimates = estimates_ols,
    pre_event_coeffs = FALSE,
    post_event_coeffs = FALSE,
    add_zero_line = FALSE,
    smpath = TRUE
) + theme_minimal() +
    scale_x_continuous(breaks = NULL, name = NULL) +
    scale_y_continuous(breaks = NULL, name = NULL)

plt_ols$layers[[4]] <-  geom_point(color = color_point_border, size = 1.5)

path_output <- here("issue36/hex.png")

hex_sticker <- sticker(plt_ols, package = "eventstudyr", filename = path_output,
                       p_size=14, s_x=.9999, s_y=.91, s_width=1.5, s_height=1.5, p_fontface = "bold",
                       p_y = 1.641, h_fill = color_interior, h_color = color_point_border)

plot(hex_sticker)
usethis::use_logo(path_output)
