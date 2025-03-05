
#' Create custom `ggplot()` boxplots.
#'
#' Creates a faceted `ggplot()` figure with boxplots per study site. Ignores
#' time/visit dependencies (will group data of all visits together).
#'
#' @param data data frame.
#' @param xval Character vector with the x-axis variable.
#' @param yval Character vector with the y-axis variable.
#' @param text Character vector, text label variable. Not used by ggplot2, but
#'   can be used to display a text label on hover after converting the ggplot to
#'   a plotly object with [plotly::ggplotly()].
#' @param color_fill Character vector with the name of the column containing the
#'   categorical color scale.
#' @param sumval summary value, used to display a horizontal line
#' @param title Character vector, main figure title.
#' @param y_title y axis title.
#' @param x_title x-axis title.
#'
#' @export
#'
#' @examples
#' fig_data <- data.frame(
#'  "site_code" = sample(c("Site 1", "Site 2"), 50, replace = TRUE),
#'  "item_value" = runif(50, 1, 100),
#'  "subject_id"  = sample(1:10, 50, replace = TRUE),
#'  "text_label" = "test label",
#'  "significance" = "test"
#'  )
#'  fig_boxplots(
#'    fig_data,
#'    sumval = 20,
#'    title = "main_title",
#'    y_title = "y-axis_title",
#'    x_title = "x-axis_title"
#'    )
fig_boxplots <- function(
    data, 
    xval = "site_code",
    yval = "item_value",
    text = "text_label",
    color_fill = "significance",
    sumval = NULL, 
    title = NULL,
    x_title = "Site",
    y_title = NULL
    ){
    fig <- ggplot2::ggplot(
      data, 
      ggplot2::aes(
        x = .data[[xval]], 
        y = .data[[yval]]
        )
      ) +
      ggplot2::geom_boxplot(
        ggplot2::aes(group = .data[[xval]]), 
        fill = "grey80"
        ) +
      # suppress warnings since the plotly::ggplotly() aesthetic 'text' is unknown by ggplot2:
      suppressWarnings(ggplot2::geom_point(
        size = 2, shape = 21, ggplot2::aes(fill = .data[[color_fill]], text = .data[[text]]),
        position = ggplot2::position_jitter(width = 0.18, seed = 2022)
      ))+
    ggplot2:: scale_fill_brewer(palette = "OrRd") +
    custom_plot_theme()
    
    if(!is.null(sumval)){
      fig <- fig + ggplot2::geom_hline(yintercept = sumval,lty = "dashed") 
    }
    if(!is.null(title)){
      fig <- fig + ggplot2::labs(title = title)
    }
    if(!is.null(x_title)){
      fig <- fig + ggplot2::labs(x = x_title)
    }
    if(!is.null(y_title)){
      fig <- fig + ggplot2::labs(y = y_title)
    }
    fig
}

#' Create timeline
#'
#' Function to create a simple timeline figure using `ggplot2`.
#'
#' @param data Data frame to use.
#'
#' @return A ggplot2 object.
#' @export
#' 
fig_timeline <- function(
    data
    ){
  stopifnot(is.data.frame(data))
  
  labels_in_data <- unique(na.omit(data$event_label))
  all_labels <- levels(data$event_label)
  all_events <- data.frame(event_label = factor(all_labels, levels = all_labels))
  
  completed_events <- all_events[
    all_events$event_label %in% labels_in_data, , drop = FALSE]
  uneven_events   <- all_events[1:length(all_events$event_label) %% 2 == 0, , drop = FALSE]
  even_events     <- all_events[1:length(all_events$event_label) %% 2 != 0, , drop = FALSE]
 fig <- ggplot2::ggplot(
    mapping = ggplot2::aes(x = event_label, y = factor(1))
    ) +
    ggplot2::geom_point(
      data = all_events, 
      alpha = 0
      ) +
    ggplot2::geom_text(
      data = even_events, 
      ggplot2::aes(y = 1.3, label = event_label), 
      size = 4.5
      ) +
    ggplot2::geom_text(
      data = uneven_events,
      ggplot2::aes(y = 0.7, label = event_label), 
      size = 4.5
      ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = .05)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank()
    )
  # by showing these layers conditionally, no error will occur when no subject 
 # is yet selected
 if(nrow(data) == 0 || nrow(completed_events) == 0) fig else {
   fig + 
     ggplot2::annotate(
       geom = "segment",
       x = dplyr::first(completed_events$event_label), 
       xend = dplyr::last(completed_events$event_label),
       y = 1, 
       yend = 1
     ) +
     ggplot2::geom_point(
       data = completed_events, 
       fill = "grey80",
       size = 6, pch = 21
     )
 }
}

#' time series figure
#'
#' Creates a ggplot2 faceted time series line plot. Can be called within
#' [plotly_figure()] to create an interactive figure.
#'
#' @param data A data frame containing the data of interest.
#' @param xval x-value to be used.
#' @param id Character string. Column containing the unique Patient IDs.
#' @param id_to_highlight Character vector. Unique Patient ID. Connects the
#'   lines in the time series.
#' @param color_fill Character vector. Column name of the variable to be used
#'   for filling color in the figure.
#' @param point_size Character vector. Column name of the variable to control
#'   the point size.
#' @param label Character vector. Label to be used for each data point. Will
#'   only be visible if the ggplot object is converted to an interactive plot
#'   using `plotly::ggplotly()`. See [plotly_figure()]
#' @param scale A logical. Whether to us a scaled value (value_scaled) or the
#'   raw variable (item_value).
#' @param use_unscaled_limits If TRUE, limits provided in the data frame will be
#'   used. This parameter will be ignored if scaled is set to `TRUE`.
#' @param point_size character vector. Column in the data frame that controls
#'   the point size in the figure.
#'
#' @return A faceted ggplot2 time series figure.
#' @export
#' @seealso [plotly_figure()]
#' @examples
#' set.seed(2025)
#' mock_data <- lapply(paste0("Subject", 1:10) , \(x){
#'   data.frame(
#'     subject_id = x,
#'     day = sample(1:25, 10),
#'     item_name = sample(c("item1", "item2"), 10, replace = TRUE),
#'     item_value = runif(10, 0 , 50),
#'     significance = sample(
#'       c("limits unknown", "out of limits, clinically significant", 
#'         "out of limits, clinically insignificant"), 
#'       10, 
#'       replace = TRUE
#'     ),
#'     text_label = "test text",
#'     reviewed = sample(c("Yes", "No"), replace = TRUE)
#'   )
#' }) |>
#'   dplyr::bind_rows()
#' fig_timeseries(mock_data, id_to_highlight = "Subject10")

fig_timeseries <- function(
    data, 
    xval = "day",
    id = "subject_id",
    id_to_highlight,
    color_fill = "significance",
    point_size = "reviewed",
    label = "text_label",
    scale = FALSE,
    use_unscaled_limits = FALSE
){
  df_id <- data[data[[id]] == id_to_highlight, ]
  yval <- ifelse(scale, "value_scaled", "item_value")
  fig <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[xval]], 
                                            y = .data[[yval]],  
                                            group = .data[[id]]
                                            )) + 
    ggplot2::facet_wrap(~item_name, ncol = 2, scales = "free_y") +
    ggplot2::scale_fill_manual(values = col_palette) +
    ggplot2::scale_x_continuous(limits = \(x){
      c(0, pmax(x[2], 3)) # keeps minimum scale of 3 days if not much data is available
    }) + 
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0.15, 0.1))) +
    custom_plot_theme() +
    ggplot2::labs(
      x = "day",
      y = "value"
    ) + 
    list(
      if(scale){
        list(
          lapply(c(0,1), \(x){
            ggplot2::geom_hline(yintercept = x,lty = 3, linewidth = 0.5, col = "grey50")
          }),
          ggplot2::labs(y = "Scaled value (>1 or <0 is out of range)")
        )
      } else if(use_unscaled_limits){
        list(ggplot2::geom_hline(ggplot2::aes(yintercept = .data[["upper_lim"]]),lty = 3, linewidth = 0.5, col = "grey50"),
             ggplot2::geom_hline(ggplot2::aes(yintercept = .data[["lower_lim"]]),lty = 3, linewidth = 0.5, col = "grey50"))
      },
      ggplot2::geom_line(alpha = 0.2),
      ggplot2::scale_size_manual(values = setNames(c(2,4), c("Yes", "No")))
    )
  
  if(nrow(df_id) == 0) return(fig)
  # Fri Nov 24 11:08:18 2023 ------------------------------
  # LSA This zooms on the active participants' data. 
  # Prevents outliers of patterns in the background from skewing the entire figure. 
  # at the moment it is only implemented when figure uses scaled figures since 
  # it sets the limits for all facets, which is undesirable if the units differ per facet.
  # note that this still skews all figures
  if(!scale){ 
    y_range <- NULL
    } else{
    y_range <- range(c(0, 1, df_id[[yval]]), na.rm = TRUE)
  } 
  
  fig +
    ggplot2::geom_line(data = df_id, linewidth = 1) + 
    ggplot2::coord_cartesian(ylim = y_range) + 
    # suppress since the text aes is unknown for geom_point but will be used for labels in ggplotly:
    suppressWarnings( 
      ggplot2::geom_point(
        data = df_id,
        mapping = ggplot2::aes(fill = .data[[color_fill]],
                               size = .data[[point_size]], text = .data[[label]]),
        shape = 21
      ) 
    )
}

#' Bar chart
#'
#' Creates a small bar chart.
#'
#' @param data Data set to be used.
#' @param x Variable to plot.
#' @param col_fill Column with logical values, that defines the fill color.
#' @param x_lab Character string with the text on the x-axis.
#' @param y_lab Character string with the text on the y-axis.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#'  df <- mtcars |>
#'  dplyr::mutate(
#'   fill_color = sample(c(TRUE, FALSE), 1),
#'   .by = cyl
#'   )
#' fig_barplot(df, cyl, fill_color)
fig_barplot <- function(
    data, 
    x, 
    col_fill,
    x_lab = "Adverse events",
    y_lab = "Individuals"
    ){
  ggplot2::ggplot(
    data, 
    mapping = ggplot2::aes(x = {{x}}, fill = {{col_fill}})) + 
    ggplot2::geom_bar() + 
    ggplot2::scale_fill_manual(
      values = setNames(c("#446e9b", "grey50"), c(TRUE, FALSE))
      ) + 
    custom_plot_theme() +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 14), 
                   axis.title = ggplot2::element_text(size = 18))
}


#' Plotly figure
#' 
#' Makes a ggplot2 figure interactive using plotly 
#'
#' @param data Data frame to use.
#' @param fig A character vector with the figure function to use.
#' @param height Height of the plot.
#' @param ... Other variables, passed onto the figure functions that are called to create the plots.
#'
#' @export
#' @seealso [fig_timeseries()], [fig_boxplots()]
plotly_figure <- function(
    data, 
    fig = "timeseries_fig",
    height = NULL,
    ...
){
  fig_funcs <- list(
    "sites_fig"      = fig_boxplots,
    "timeseries_fig" = fig_timeseries
  )
  if(length(fig) != 1) stop("only one function allowed")
  if(!fig %in% names(fig_funcs)) stop("invalid function provided")
  golem::cat_dev("Figure", fig, "selected\n")
  plot_object <- data |> 
    fig_funcs[[fig]](...) |> 
    plotly::ggplotly(
      source = "click_individuals",
      height = height,
      tooltip = "text"
    ) 
  # 20.09.2022 LSA: manual hotfix plotly issue, unintentionally showing outlier 
  # points in (and hoverinfo) boxplots. See 
  # https://github.com/plotly/plotly.R/issues/1114#issuecomment-1239268701
  if(fig == "sites_fig") return(remove_boxplot_outliers(plot_object))
  plot_object
}
