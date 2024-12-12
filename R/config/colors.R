# Stanford Brand Colors
stanford_colors <- list(
  # Core brand colors
  cardinal_red = "#8C1515",
  cardinal_dark = "#820000",
  cool_grey = "#4D4F53",
  process_black = "#2E2D29",
  fog = "#DAD7CB",
  stone = "#544948",
  beige = "#F4F4F4",
  white = "#FFFFFF",

  # Digital interaction colors
  digital_red = "#B1040E",
  digital_red_light = "#E50808",
  digital_red_dark = "#820000",

  digital_blue = "#006CB8",
  digital_blue_light = "#6FC3FF",
  digital_blue_dark = "#00548F",

  digital_green = "#008566",
  digital_green_light = "#1AECBA",
  digital_green_dark = "#006F54"
)

# Bootstrap theme color mapping
bootstrap_colors <- list(
  # Light mode
  light = list(
    primary = stanford_colors$cardinal_red,
    secondary = stanford_colors$cool_grey,
    success = stanford_colors$cardinal_red,
    info = stanford_colors$digital_blue,
    warning = stanford_colors$digital_red,
    danger = stanford_colors$cardinal_dark,
    light = stanford_colors$beige,
    dark = stanford_colors$process_black,

    # Background and text
    bg = stanford_colors$white,
    fg = stanford_colors$process_black,

    # Interactive elements
    link = stanford_colors$digital_blue,
    link_hover = stanford_colors$digital_blue_dark,

    # Specific UI elements
    border = stanford_colors$fog,
    input_bg = stanford_colors$white,
    input_fg = stanford_colors$process_black
  ),

  # Dark mode
  dark = list(
    primary = stanford_colors$cardinal_red,
    secondary = stanford_colors$fog,
    success = stanford_colors$digital_green_light,
    info = stanford_colors$digital_blue_light,
    warning = stanford_colors$digital_red_light,
    danger = stanford_colors$cardinal_dark,
    light = stanford_colors$process_black,
    dark = stanford_colors$beige,

    # Background and text
    bg = "#1A1A1A",  # Darker than process_black for better contrast
    fg = stanford_colors$beige,

    # Interactive elements
    link = stanford_colors$digital_blue_light,
    link_hover = stanford_colors$digital_blue,

    # Specific UI elements
    border = stanford_colors$stone,
    input_bg = stanford_colors$process_black,
    input_fg = stanford_colors$beige
  )
)

# Stanford fonts configuration
stanford_fonts <- list(
  base = font_collection(
    #font_google("Source Sans Pro", wght = c(400, 600), local = TRUE),
    "system-ui", "-apple-system", "BlinkMacSystemFont", "'Segoe UI'",
    "Roboto", "'Helvetica Neue'", "Arial", "sans-serif"
  ),
  heading = font_collection(
    #font_google("Source Serif Pro", wght = c(400, 600), local = TRUE),
    "Georgia", "'Times New Roman'", "Times", "serif"
  ),
  code = font_collection(
    #font_google("Source Code Pro", wght = 400, local = TRUE),
    "'Courier New'", "Courier", "monospace"
  )
)

#' Create Theme
#'
#' Creates a Bootstrap theme with Stanford colors
#'
#' @param dark_mode Logical indicating dark mode state (default: FALSE)
#' @return bslib theme object
#' @export
create_theme <- function(dark_mode = FALSE) {
  colors <- if(dark_mode) bootstrap_colors$dark else bootstrap_colors$light

  bs_theme(
    version = 5,
    bootswatch = if(dark_mode) "darkly" else "flatly",

    # Core colors
    primary = colors$primary,
    secondary = colors$secondary,
    success = colors$success,
    info = colors$info,
    warning = colors$warning,
    danger = colors$danger,
    light = colors$light,
    dark = colors$dark,

    # Background and text
    bg = colors$bg,
    fg = colors$fg,

    # Fonts
    font_scale = 1.0,
    base_font = stanford_fonts$base,
    heading_font = stanford_fonts$heading,
    code_font = stanford_fonts$code,

    # Custom CSS for better accessibility
    "link-color" = colors$link,
    "link-hover-color" = colors$link_hover,

    "input-bg" = colors$input_bg,
    "input-color" = colors$input_fg,
    "input-border-color" = colors$border,

    # Ensure sufficient contrast for form elements
    "input-focus-border-color" = colors$link,
    "input-focus-box-shadow" = sprintf("0 0 0 0.25rem %s33", colors$link),  # 20% opacity

    # Card styling
    "card-bg" = colors$bg,
    "card-color" = colors$fg,
    "card-border-color" = colors$border
  )
}

#' Create Plot Theme
#'
#' Creates a ggplot2 theme matching the application theme
#'
#' @param dark_mode Logical indicating dark mode state (default: FALSE)
#' @return ggplot2 theme object
#' @export
create_plot_theme <- function(dark_mode = FALSE) {
  colors <- if(dark_mode) bootstrap_colors$dark else bootstrap_colors$light

  theme_minimal() +
    theme(
      # Text elements
      text = element_text(color = colors$fg),
      plot.title = element_text(color = colors$primary, face = "bold"),
      plot.subtitle = element_text(color = colors$secondary),
      axis.title = element_text(color = colors$fg),
      axis.text = element_text(color = colors$secondary),

      # Grid and panel
      panel.grid.major = element_line(color = alpha(colors$secondary, 0.2)),
      panel.grid.minor = element_line(color = alpha(colors$secondary, 0.1)),
      plot.background = element_rect(fill = colors$bg, color = NA),
      panel.background = element_rect(fill = colors$bg, color = NA),

      # Legend
      legend.text = element_text(color = colors$fg),
      legend.title = element_text(color = colors$fg),
      legend.background = element_rect(fill = colors$bg)
    )
}
