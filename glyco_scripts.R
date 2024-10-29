library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggh4x)

# A color palette that I like
my_colors <-c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
              "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", 
              "#bcbd22", "#17becf")



#' facetted_boxplots
#' 
#' Create a figure with boxplots, facetted by variable.
#'
#' @param data_long 
#' A dataframe in long format that contains at least a column for variable and
#' a column for the value.
#' @param variable_column
#' Column name for the column that contains names of the variables (used as facet titles.)
#' When unspecified, it is assumed to be called "variable".
#' @param value_column
#' Column name for the column that contains values for the y-axis.
#' When unspecified, assumed to be called "value".
#' @param value_column
#' Column name for the groups for which boxplots should be created (x-axis variable.)
#' When unspecified, assumed to be called "group".
#' @param ncol
#' Number of columns for the facetted boxplot.
#' Chosen automatically when left empty.
#' @param order
#' Optional: character vector with order of facets.
#' @param jitter_width
#' Width of point jitter over the boxplots
#' @param jitter_alpha
#' Alpha of jittered points.
#' @param xlab
#' Character: title of x-axis
#' @param ylab
#' Character: title of y-axis
#' @param legend_title
#' Title of legend. Empty by default.
#' @param color_palette
#' Optional: character vector with custom color_palette
#' @param names
#' Optional: a vector with labels for facets
facetted_boxplots <- function(data_long, 
                              variable_column = "variable",
                              value_column = "value",
                              groups_column = "group",
                              xlab = "",
                              ylab = "",
                              ncol = NULL,
                              order = NULL,
                              labels = NULL,
                              jitter_width = 0.25,
                              jitter_alpha = 0.5
                              ) {
  # If order is specified, turn variable column into factor
  if (!is.null(order)) {
    data_long <- data_long %>% 
      mutate(!!sym(variable_column) := factor(!!sym(variable_column), levels = order))
  }
  # Create facetted plot, check if they should be labeled.
  if (!is.null(labels)) {
    facets <- ggplot(data_long, aes(.data[[groups_column]], .data[[value_column]])) +
                facet_wrap(formula(paste("~", variable_column)), scales = "free_y", ncol = ncol,
                          labeller = as_labeller(labels))
  } else {
    facets <- ggplot(data_long, aes(.data[[groups_column]], .data[[value_column]])) +
      facet_wrap(formula(paste("~", variable_column)), scales = "free_y", ncol = ncol)
  }
  plot <- facets +
    geom_boxplot(outlier.shape = NA, fill = "lightgrey", alpha = 0.5) +
    geom_jitter(aes(color = .data[[groups_column]]), height = 0, width = jitter_width, alpha = jitter_alpha) +
    labs(x = xlab, y = ylab) +
    theme_classic() +
    border(size = 0.5) +
    theme(
      legend.position = "bottom",
	  legend.key = element_rect(colour = NA)
    ) +
    guides(color = guide_legend(override.aes = list(size = 3)))
  # Return the plot
  return(plot)
}




#' significance_scales
#' 
#' Create a dataframe with significance values that can be added to facetted boxplots.
#'
#' @param data_long 
#' Dataframe in long format.
#' @param value_column 
#' Name of the column containing values.
#' @param variable_column 
#' Name of the columm containing variables by which the boxplots are facetted.
#' @param test_results_df 
#' A dataframe with test results from comparing two groups.
#' @param group1_name 
#' Name of the first group.
#' @param group2_name 
#' Name of the second group.
#' @param order
#' Optional: character vector specifying order of facets
#' @param height_factor 
#' Number that determines height of annotation, with respect to max y value.
#' 0.2 by default.
#' @param yscale_factor
#' Number that determines the maximum height of y-axis. 
#' Necessary to prevent significance annotation from being cut off.
#' 0.3 by default.
significance_annotation <- function(data_long, 
                                    value_column = "value",
                                    variable_column = "variable",
                                    test_results_df, 
                                    group1_name, 
                                    group2_name, 
                                    order = NULL,
                                    height_factor = 0.2,
                                    yscale_factor = 0.3) {
  # Create dataframe for annotation
  annotation_df <- test_results_df %>% 
    rowwise() %>% 
    mutate(
      group1 = group1_name,
      group2 = group2_name,
      y_max = max((data_long[[value_column]])[data_long[[variable_column]] == .data[[variable_column]]], na.rm = TRUE),
      y_min = min((data_long[[value_column]])[data_long[[variable_column]] == .data[[variable_column]]], na.rm = TRUE),
      y_diff = y_max - y_min,
      y.position = y_max + height_factor * y_diff
    ) %>% 
    ungroup()
  # Optional: ordering of facets
  if (!is.null(order)) {
    annotation_df <- annotation_df %>% 
      mutate(!!sym(variable_column) := factor(!!sym(variable_column), levels = order))
  }
  return(annotation_df)
}



annotation_yscales <- function(annotation_df, variable_column = "variable", yscale_factor = 0.3) {
  # Specify y-scales per facet, for ggh4x::facetted_pos_scales() function
  yscales_df <- annotation_df %>%
    mutate(ylimit = y_max + yscale_factor * y_diff) %>%
    select({{variable_column}}, ylimit)
  yscales_df <- split(yscales_df, yscales_df[[variable_column]])
  yscales <- lapply(yscales_df, function(x) {
    scale_y_continuous(limits = c(NA, x$ylimit))
  })
  return(yscales)
}





# Increase y-axis space of a facetted ggplot, for use with stat_cor() annotation
increase_y_limits <- function(faceted_plot,
                              facet_variable,
                              increase_by) {
  
  facet_layout <- ggplot_build(faceted_plot)$layout$layout
  
  limits <- facet_layout %>%
    pmap(.,
         function(ROW, COL, ...) {
           limits <- tibble(
             max_y = max(layer_scales(faceted_plot, i = ROW, j = COL)$y$get_limits()),
             min_y = min(layer_scales(faceted_plot, i = ROW, j = COL)$y$get_limits()),
             range = max_y - min_y
           )
         }) %>% 
    reduce(., full_join) %>%
    mutate(new_max_y = max_y + increase_by * range,
           facet = facet_layout[[facet_variable]])
  
  join_by <- "facet"
  names(join_by) <- facet_variable
  
  for_geom_blank <- full_join(faceted_plot$data,
                              limits,
                              by = join_by)
  
  plot_with_new_limits <- faceted_plot +
    geom_blank(data = for_geom_blank,
               aes(y = new_max_y))
  return(plot_with_new_limits)
}
