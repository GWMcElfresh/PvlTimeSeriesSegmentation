# PvlTimeSeriesSegmentation

An R package for segmenting viral load time series data using kernel density estimation to identify basins and bridges in 2D (value, slope) feature space.

## Overview

This package provides tools for analyzing viral load time series data by:

1. **Interpolating** viral load measurements from (date, result) format to a (value, slope) feature space
2. **Segmenting** the time series using kernel density estimation to identify:
   - **Basins**: Temporally contiguous, high-density regions representing stable viral load states
   - **Bridges**: Single, connected, low-density paths between basins representing transitions
3. **Classifying** timepoints with descriptive state labels ("Basin1", "Bridge", "Basin2", etc.)
4. **Visualizing** the results with multiple plot types

## Installation

```r
# Install dependencies
install.packages(c("MASS", "ggplot2", "dplyr", "tidyr"))

# Load the package (development version)
devtools::load_all()
```

## Quick Start

```r
library(PvlTimeSeriesSegmentation)

# Generate sample viral load data
sample_data <- generate_sample_data(12, pattern = "decline_rebound")

# Interpolate to feature space
interpolated <- interpolate_viral_load(sample_data, n_points = 30)

# Segment time series
segmentation <- segment_time_series(interpolated)

# Classify states with descriptive labels
classified <- classify_states(segmentation)

# View results
print(classified$summary)
table(classified$data$state_label)

# Create visualizations
plot_segmentation(classified, plot_type = "trajectory")
plot_segmentation(classified, plot_type = "time_series")
plot_segmentation(classified, plot_type = "density")
```

## Key Functions

### `interpolate_viral_load(data, method = "linear", n_points = 100)`
Converts viral load data from (date, result) format to (value, slope) feature space.

### `segment_time_series(interpolated_data, density_threshold = 0.1, min_basin_size = 5, variance_threshold = 0.5)`
Performs kernel density estimation and identifies basins and bridges.

### `classify_states(segmentation_result)`
Assigns descriptive state labels to segmented data.

### `plot_segmentation(segmentation_result, plot_type = "trajectory", use_ggplot = TRUE)`
Creates visualizations of segmentation results.

### `generate_sample_data(n_months = 12, pattern = "decline_rebound")`
Generates synthetic viral load data for testing and examples.

## Parameters

### Segmentation Parameters
- `density_threshold`: Threshold for distinguishing basins from bridges (default: 0.1)
- `min_basin_size`: Minimum number of consecutive points to form a basin (default: 5) 
- `variance_threshold`: Threshold for bridge variance heuristic (default: 0.5)

### Plot Types
- `"trajectory"`: 2D trajectory in (value, slope) space
- `"density"`: Density contours with segmented trajectory
- `"time_series"`: Time series with basin/bridge segmentation
- `"all"`: All plot types

## Example Output

The package identifies temporal patterns such as:
- **Basin1**: Initial high viral load phase
- **Bridge**: Transition period (e.g., treatment response)
- **Basin2**: Stabilized low viral load phase
- **Bridge**: Potential rebound transition
- **Basin3**: Rebound phase

## Algorithm Details

1. **Feature Extraction**: Interpolates viral load values and calculates slopes
2. **Kernel Density Estimation**: Uses 2D KDE to estimate density in (value, slope) space
3. **Basin Identification**: Identifies high-density regions as basins
4. **Bridge Classification**: Uses variance heuristics to classify low-density transitions
5. **Temporal Constraints**: Enforces minimum basin sizes and temporal contiguity

## License

MIT License - see LICENSE file for details.