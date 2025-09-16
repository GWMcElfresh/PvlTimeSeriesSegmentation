#' Classify States with Descriptive Labels
#'
#' Assigns descriptive state labels like "Basin1", "Bridge", "Basin2" to segmented data.
#'
#' @param segmentation_result Result from segment_time_series function
#' @return Data frame with additional state_label column
#' @export
#' @examples
#' # Using segmentation results
#' dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
#' values <- c(1000, 500, 200, 50, 25, 100, 500, 1000, 2000, 1500, 800, 300)
#' sample_data <- data.frame(date = dates, result = values)
#' interpolated <- interpolate_viral_load(sample_data)
#' segmentation <- segment_time_series(interpolated)
#' classified <- classify_states(segmentation)
classify_states <- function(segmentation_result) {
  if (!"data" %in% names(segmentation_result)) {
    stop("Input must be a result from segment_time_series function")
  }
  
  data <- segmentation_result$data
  
  # Create descriptive state labels
  state_labels <- character(nrow(data))
  
  for (i in 1:nrow(data)) {
    if (data$state[i] == "Basin") {
      basin_num <- data$basin_index[i]
      state_labels[i] <- paste0("Basin", basin_num)
    } else {
      # For bridges, we can be more descriptive
      bridge_num <- data$bridge_index[i]
      state_labels[i] <- "Bridge"
      
      # Optionally add more context about the bridge
      if (!is.na(bridge_num)) {
        # Check what basins this bridge connects
        prev_basin <- find_previous_basin(data, i)
        next_basin <- find_next_basin(data, i)
        
        if (!is.na(prev_basin) && !is.na(next_basin)) {
          state_labels[i] <- paste0("Bridge", bridge_num, "_", prev_basin, "to", next_basin)
        } else {
          state_labels[i] <- paste0("Bridge", bridge_num)
        }
      }
    }
  }
  
  # Add state labels to the data
  result_data <- data
  result_data$state_label <- state_labels
  
  # Update the segmentation result
  segmentation_result$data <- result_data
  
  # Add classification summary
  segmentation_result$classification_summary <- summarize_classification(result_data)
  
  return(segmentation_result)
}

#' Find Previous Basin
#' @param data Segmentation data
#' @param current_index Current row index
#' @return Basin index of previous basin, or NA if none found
find_previous_basin <- function(data, current_index) {
  if (current_index <= 1) return(NA)
  
  for (i in (current_index - 1):1) {
    if (data$state[i] == "Basin") {
      return(data$basin_index[i])
    }
  }
  return(NA)
}

#' Find Next Basin
#' @param data Segmentation data
#' @param current_index Current row index
#' @return Basin index of next basin, or NA if none found
find_next_basin <- function(data, current_index) {
  if (current_index >= nrow(data)) return(NA)
  
  for (i in (current_index + 1):nrow(data)) {
    if (data$state[i] == "Basin") {
      return(data$basin_index[i])
    }
  }
  return(NA)
}

#' Summarize Classification Results
#' @param classified_data Data frame with state classifications
#' @return Summary of classification results
summarize_classification <- function(classified_data) {
  # Count occurrences of each state label
  state_counts <- table(classified_data$state_label)
  
  # Get unique basins and bridges
  unique_basins <- unique(classified_data$basin_index[!is.na(classified_data$basin_index)])
  unique_bridges <- unique(classified_data$bridge_index[!is.na(classified_data$bridge_index)])
  
  list(
    state_label_counts = as.list(state_counts),
    unique_basins = unique_basins,
    unique_bridges = unique_bridges,
    total_states = length(unique(classified_data$state_label))
  )
}