#' Plot Segmentation Results
#'
#' Creates visualization plots for the time series segmentation results.
#'
#' @param segmentation_result Result from segment_time_series or classify_states function
#' @param plot_type Type of plot: "trajectory", "density", "time_series", or "all"
#' @param use_ggplot Logical, whether to use ggplot2 for plotting (default: TRUE)
#' @return Plot object(s) or NULL if plotting to device
#' @export
#' @examples
#' # Using classification results
#' dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
#' values <- c(1000, 500, 200, 50, 25, 100, 500, 1000, 2000, 1500, 800, 300)
#' sample_data <- data.frame(date = dates, result = values)
#' interpolated <- interpolate_viral_load(sample_data)
#' segmentation <- segment_time_series(interpolated)
#' classified <- classify_states(segmentation)
#' plot_segmentation(classified, plot_type = "all")
plot_segmentation <- function(segmentation_result, plot_type = "trajectory", use_ggplot = TRUE) {
  if (!"data" %in% names(segmentation_result)) {
    stop("Input must be a result from segment_time_series or classify_states function")
  }
  
  data <- segmentation_result$data
  
  if (use_ggplot) {
    return(create_ggplot_visualizations(segmentation_result, plot_type))
  } else {
    return(create_base_plots(segmentation_result, plot_type))
  }
}

#' Create ggplot2 Visualizations
#' @param segmentation_result Segmentation results
#' @param plot_type Type of plot to create
#' @return ggplot object or list of ggplot objects
create_ggplot_visualizations <- function(segmentation_result, plot_type) {
  data <- segmentation_result$data
  
  # Define colors for states
  if ("state_label" %in% names(data)) {
    unique_labels <- unique(data$state_label)
    colors <- rainbow(length(unique_labels))
    names(colors) <- unique_labels
  } else {
    colors <- c("Basin" = "blue", "Bridge" = "red")
  }
  
  plots <- list()
  
  if (plot_type %in% c("trajectory", "all")) {
    # 2D trajectory plot in (value, slope) space
    color_var <- if ("state_label" %in% names(data)) "state_label" else "state"
    
    plots$trajectory <- ggplot2::ggplot(data, ggplot2::aes(x = value, y = slope, color = !!ggplot2::sym(color_var))) +
      ggplot2::geom_path(alpha = 0.7, size = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(
        x = "Viral Load Value", 
        y = "Slope",
        title = "Time Series Trajectory in (Value, Slope) Space",
        color = "State"
      ) +
      ggplot2::theme_minimal()
  }
  
  if (plot_type %in% c("density", "all")) {
    # Density contour plot
    kde_result <- segmentation_result$kde_result
    
    # Convert KDE result to long format for ggplot
    kde_df <- expand.grid(x = kde_result$x, y = kde_result$y)
    kde_df$z <- as.vector(kde_result$z)
    
    color_var <- if ("state_label" %in% names(data)) "state_label" else "state"
    
    plots$density <- ggplot2::ggplot() +
      ggplot2::geom_contour(data = kde_df, ggplot2::aes(x = x, y = y, z = z), 
                           color = "gray70", alpha = 0.7) +
      ggplot2::geom_path(data = data, ggplot2::aes(x = value, y = slope, color = !!ggplot2::sym(color_var)), 
                        alpha = 0.8, size = 1) +
      ggplot2::geom_point(data = data, ggplot2::aes(x = value, y = slope, color = !!ggplot2::sym(color_var)), 
                         size = 2) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(
        x = "Viral Load Value", 
        y = "Slope",
        title = "Density Contours with Segmented Trajectory",
        color = "State"
      ) +
      ggplot2::theme_minimal()
  }
  
  if (plot_type %in% c("time_series", "all")) {
    # Time series plot
    color_var <- if ("state_label" %in% names(data)) "state_label" else "state"
    
    plots$time_series <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = value, color = !!ggplot2::sym(color_var))) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(
        x = "Date", 
        y = "Viral Load Value",
        title = "Time Series with Basin/Bridge Segmentation",
        color = "State"
      ) +
      ggplot2::theme_minimal()
  }
  
  if (plot_type == "all") {
    return(plots)
  } else {
    return(plots[[plot_type]])
  }
}

#' Create Base R Plots
#' @param segmentation_result Segmentation results
#' @param plot_type Type of plot to create
#' @return NULL (plots to device)
create_base_plots <- function(segmentation_result, plot_type) {
  data <- segmentation_result$data
  
  # Define colors for states
  if ("state_label" %in% names(data)) {
    unique_labels <- unique(data$state_label)
    colors <- rainbow(length(unique_labels))
    names(colors) <- unique_labels
    point_colors <- colors[data$state_label]
  } else {
    colors <- c("Basin" = "blue", "Bridge" = "red")
    point_colors <- colors[data$state]
  }
  
  if (plot_type %in% c("trajectory", "all")) {
    # 2D trajectory plot
    plot(data$value, data$slope, 
         col = point_colors, 
         pch = 19,
         xlab = "Viral Load Value", 
         ylab = "Slope",
         main = "Time Series Trajectory in (Value, Slope) Space")
    
    # Draw trajectory lines
    lines(data$value, data$slope, col = "gray50", lwd = 1.5)
    
    # Add legend
    if ("state_label" %in% names(data)) {
      legend("topright", legend = names(colors), col = colors, pch = 19)
    } else {
      legend("topright", legend = c("Basin", "Bridge"), col = c("blue", "red"), pch = 19)
    }
  }
  
  if (plot_type %in% c("density", "all")) {
    # Density contour plot
    kde_result <- segmentation_result$kde_result
    
    # Create new plot for density
    if (plot_type != "all") {
      contour(kde_result$x, kde_result$y, kde_result$z, 
              xlab = "Viral Load Value", 
              ylab = "Slope",
              main = "Density Contours with Segmented Trajectory")
    }
    
    # Add contours
    contour(kde_result$x, kde_result$y, kde_result$z, add = TRUE, col = "gray70")
    
    # Add trajectory
    points(data$value, data$slope, col = point_colors, pch = 19)
    lines(data$value, data$slope, col = "black", lwd = 1.5)
    
    # Add legend
    if ("state_label" %in% names(data)) {
      legend("topright", legend = names(colors), col = colors, pch = 19)
    } else {
      legend("topright", legend = c("Basin", "Bridge"), col = c("blue", "red"), pch = 19)
    }
  }
  
  if (plot_type %in% c("time_series", "all")) {
    # Time series plot
    plot(data$date, data$value, 
         col = point_colors, 
         pch = 19,
         xlab = "Date", 
         ylab = "Viral Load Value",
         main = "Time Series with Basin/Bridge Segmentation",
         type = "b")
    
    # Add legend
    if ("state_label" %in% names(data)) {
      legend("topright", legend = names(colors), col = colors, pch = 19)
    } else {
      legend("topright", legend = c("Basin", "Bridge"), col = c("blue", "red"), pch = 19)
    }
  }
  
  return(invisible(NULL))
}