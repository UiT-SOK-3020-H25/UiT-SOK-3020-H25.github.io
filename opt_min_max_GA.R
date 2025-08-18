# Example from: https://luca-scr.github.io/GA/articles/GA.html
# browse URL
#browseURL("https://luca-scr.github.io/GA/articles/GA.html")

library(tidyverse)
#install.packages("GA") # if needed
library(GA)

# --- 1. Define the Functions ---
# The original function
f <- function(x) (x^2 + x) * cos(x)
# The derivative of the function
fp <- function(x) (2*x + 1) * cos(x) - (x^2 + x) * sin(x)


# --- 2. Prepare the Data for ggplot2 ---
# Create a sequence of x-values for a smooth curve.
# We'll plot from -10 to 10, with 500 points in between.
x_vals <- seq(-10, 10, length.out = 500)

# Create a data frame with the x values and the corresponding y values for each function.
# This is often called a "wide" data format.
plot_data_wide <- data.frame(
  x = x_vals,
  f_x = f(x_vals),
  f_prime_x = fp(x_vals)
)

# Convert the data from "wide" to "long" format.
# This is the preferred format for ggplot2 as it allows us to map the
# 'function_type' column to aesthetics like color or linetype easily.
plot_data_long <- pivot_longer(
  plot_data_wide,
  cols = c("f_x", "f_prime_x"),
  names_to = "function_type",
  values_to = "y"
)


# --- 3. Create the Plot ---
ggplot(plot_data_long, aes(x = x, y = y, color = function_type)) +
  
  # Add lines for the functions
  geom_line(linewidth = 1) +
  
  # Add a horizontal line at y=0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Manually set the colors and legend labels for clarity
  scale_color_manual(
    name = "Function", # Title for the legend
    values = c("f_x" = "blue", "f_prime_x" = "red"),
    labels = c("f(x) = (x^2 + x)cos(x)", "f'(x) [Derivative]")
  ) +
  
  # Add informative labels and a title
  labs(
    title = "Plot of a Function and its Derivative",
    subtitle = "Note where the derivative f'(x) is zero (crosses the axis) at the peaks and troughs of f(x).",
    x = "x",
    y = "y"
  ) +
  
  # Apply a clean, minimal theme
  theme_minimal() +
  
  # Customize theme elements for better readability
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# --- Deterministic (calculus) search for stationary points ---
grid <- seq(-10, 10, length.out = 40001)
sgn  <- sign(fp(grid))
idx  <- which(diff(sgn) != 0 & is.finite(sgn[-length(sgn)]) & is.finite(sgn[-1]))

crit <- c(-10, 10)
for (i in idx) {
  a <- grid[i]; b <- grid[i+1]
  if (is.finite(fp(a)) && is.finite(fp(b)) && fp(a) * fp(b) <= 0) {
    r <- try(uniroot(fp, c(a,b))$root, silent = TRUE)
    if (!inherits(r, "try-error")) crit <- c(crit, r)
  }
}
crit <- sort(unique(round(crit, 10)))

vals <- sapply(crit, f)
res  <- tibble(x = crit, fx = vals) %>% arrange(fx)
res_min <- slice(res, 1)         # global min (calc)
res_max <- slice(res, n())       # global max (calc)
opt_calc <- bind_rows(
  res_min %>% mutate(method = "Calculus (min)"),
  res_max %>% mutate(method = "Calculus (max)")
)

# --- GA search (stochastic) for max and min ---
set.seed(1234)

GA_max <- ga(
  type = "real-valued",
  fitness = function(x) f(x),
  lower = -10, upper = 10,
  popSize = 120, maxiter = 250, run = 80
)
GA_min <- ga(
  type = "real-valued",
  fitness = function(x) -f(x),   # maximize -f -> minimize f
  lower = -10, upper = 10,
  popSize = 120, maxiter = 250, run = 80
)

# Pull best solutions (handle multiple near-ties)
sol_max <- as.numeric(GA_max@solution)
sol_min <- as.numeric(GA_min@solution)

opt_ga <- tibble(
  x  = c(sol_min, sol_max),
  fx = f(c(sol_min, sol_max)),
  method = c("GA (min)", "GA (max)")
)

# --- Data for curves ---
plot_data <- tibble(
  x  = seq(-10, 10, length.out = 1000),
  fx = f(x),
  dfx = fp(x)
)

# --- Plot ---
ggplot(plot_data, aes(x = x)) +
  # function and derivative
  geom_line(aes(y = fx), color = "blue", size = 1) +
  geom_line(aes(y = dfx), color = "orange", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey50") +
  # calculus optima: solid red
  geom_point(
    data = opt_calc,
    aes(y = fx, shape = method),
    color = "red", size = 3, stroke = 1, alpha = 1
  ) +
  # GA optima: semi-transparent red (so overlap is visible)
  geom_point(
    data = opt_ga,
    aes(y = fx, shape = method),
    color = "black", size = 1.5, stroke = 1, alpha = 0.95
  ) +
  labs(
    title = expression(paste("f(x) == (x^2 + x) * cos(x)  and  f'(x)")),
    subtitle = "Blue = f(x), Orange dashed = f'(x), Red points = optima (solid: Calculus, transparent: GA)",
    y = "Value", x = "x"
  ) +
  theme_minimal() +
  scale_shape_manual(values = c(
    "Calculus (min)" = 16,
    "Calculus (max)" = 17,
    "GA (min)"       = 1,
    "GA (max)"       = 2
  ))
