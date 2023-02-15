#' Print graphical representation of a correlation matrix.
#'
#'\code{ggcormat} makes the same correlation matrix as \link{cortestR} 
#' and graphically represents it in a plot
#'
#' @param cor_mat correlation matrix as produced by cor.
#' @param method text specifying type of correlation.
#' @param title plot title.
#' @param maxpoint maximum for scale_size_manual, may need adjustment depending on plotsize.
#' @param textsize for theme text.
#' @param axistextsize relative text size for axes.
#' @param titlesize as you already guessed, relative text size for title.
#' @param breaklabels currently not used, intended for str_wrap.
#' @param .low Color for heatmap.
#' @param .high Color for heatmap.
#' @param lower_only should only lower triangle be plotted?
#' @param .legendtitle Optional name for color legend.
#' @param p_mat Optional matrix of p-values; if provided, this is used to define
#' size of dots rather than absolute correlation.
#'
#' @return A ggplot object, allowing further styling.
#'
#' @examples
#' coeff_pvalues <- cortestR(mtcars[, c("wt", "mpg", "qsec", "hp")],
#'   split = TRUE, sign_symbol = FALSE
#' )
#' # focus on coefficients:
#' ggcormat(cor_mat = coeff_pvalues$corout, maxpoint = 5)
#' # size taken from p-value:
#' ggcormat(
#'   cor_mat = coeff_pvalues$corout,
#'   p_mat = coeff_pvalues$pout, maxpoint = 5)
#' 
#' @export
ggcormat <- function(cor_mat, p_mat = NULL,
                     method = "Correlation", title = "",
                     maxpoint = 2.1, textsize = 5, axistextsize = 2,
                     titlesize = 3, breaklabels = NULL,
                     lower_only = TRUE,
                     .low = "blue3", .high = "red2",
                     .legendtitle = NULL) {
  `.` <- Variable1 <- Variable2 <- value <- size <- x <- y <- NULL
  rownames(cor_mat) <- colnames(cor_mat)
  # dd <- as.dist((1-cor_mat)/2)
  # hc <- hclust(dd)
  # cor_mat <-cor_mat[hc$order, hc$order]
  var_order <- rownames(cor_mat)
  if (lower_only) {
    cor_mat[upper.tri(cor_mat)] <- NA
  }
  melted_cor_mat <- cor_mat |>
    as_tibble() |>
    mutate(Variable1 = colnames(cor_mat)) |>
    pivot_longer(-Variable1,names_to = 'Variable2') |> 
    # gather(key = Variable2, value = value, -Variable1) |>
    na.omit() |>
    mutate(
      Variable1 = factor(Variable1, levels = var_order),
      Variable2 = factor(Variable2, levels = var_order),
      value = as.numeric(value),
      size = abs(value)
    )
  corvar_count <- nrow(cor_mat)
  if (!is.null(p_mat)) {
    melted_p_mat <- 
      suppressWarnings(
        p_mat |>
          as_tibble() |>
          mutate(Variable1 = colnames(p_mat)) |>
          pivot_longer(-Variable1,names_to = 'Variable2', values_to = 'size') |>
          # gather(key = Variable2, value = size, -Variable1) |>
          mutate(
            Variable1 = factor(Variable1, levels = var_order),
            Variable2 = factor(Variable2, levels = var_order),
            size = -log10(formatP(as.numeric(size),
                                  textout = FALSE
            ))
          ) |>
          na.omit())
    melted_p_mat$size[which(melted_p_mat$size > 3)] <- 3
    melted_cor_mat <- full_join(melted_cor_mat[1:3], melted_p_mat,
                                by = c("Variable1", "Variable2")
    )
  }
  if (lower_only) {
    triangel <- data.frame(
      x = c(0, rep(corvar_count + 2, 2)),
      y = c(rep(corvar_count + 2, 2), 0)
    )
    viereck1 <- data.frame(
      x = c(rep(0, 2), rep(corvar_count + 2.2, 2)),
      y = c(0, 0.4, 0.4, 0)
    )
    viereck2 <- data.frame(
      x = c(0, 0.4, 0.4, 0),
      y = c(rep(0, 2), rep(corvar_count + 2.2, 2))
    )
    viereck3 <- data.frame(
      x = c(
        corvar_count + 0.6, corvar_count + 2.2,
        corvar_count + 2.2, corvar_count + 0.6
      ),
      y = c(rep(0, 2), rep(corvar_count + 2.2, 2))
    )
    viereck4 <- data.frame(
      x = c(rep(0, 2), rep(corvar_count + 2.2, 2)),
      y = c(
        corvar_count + 0.6, corvar_count + 2.2,
        corvar_count + 2.2, corvar_count + 0.6
      )
    )
  }
  if (is.null(breaklabels)) {
    breaklabels <- levels(melted_cor_mat$Variable1)
  }
  
  ggheatmap <- ggplot(
    melted_cor_mat,
    aes(Variable2, Variable1)
  ) +
    # geom_tile(color = "white")+
    geom_point(aes(Variable2, Variable1,
                   size = size, color = value
    )) +
    geom_vline(xintercept = seq(0.5, corvar_count + 1, by = 1), 
               color = "grey", linewidth = .25) +
    geom_hline(yintercept = seq(0.5, corvar_count + 1, by = 1), 
               color = "grey", linewidth = .25) +
    # geom_point(aes(Variable2, Variable1, size=abs(value),color=value))+
    scale_color_gradient2(
      low = .low, high = .high, mid = "lightgrey",
      midpoint = 0, limit = c(-1, 1), space = "Lab",
      name = method
    ) +
    theme_minimal() + # minimal theme
    theme(
      axis.text.x = element_text(
        angle = 90, vjust = 0,
        hjust = 1
      ),
      text = element_text(size = textsize),
      axis.text = element_text(size = rel(axistextsize)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = rel(titlesize), face = "bold"),
      legend.text = element_text(size = rel(1.25)),
      legend.title = element_text(size = rel(1.65))
      # legend.key.size = unit(.5,'lines'),
      # legend.key.width=unit(5.5,'cm'),
    ) +
    coord_fixed() +
    # theme(legend.position = "right")+
    scale_y_discrete(
      limits = rev(levels(melted_cor_mat$Variable1)),
      labels = rev(breaklabels)
    ) +
    scale_x_discrete(
      limits = levels(melted_cor_mat$Variable1),
      labels = breaklabels
    ) +
    ggtitle(title)
  if (is.null(p_mat)) {
    ggheatmap <- ggheatmap +
      scale_size_continuous(
        name = ifelse(
          is.null(.legendtitle), "abs. correlation scaling",
          .legendtitle
        ),
        limits = c(0, 1),
        breaks = c(.1, .5, .9),
        labels = c(.1, .5, .9),
        range = rel(c(.2, maxpoint))
      )
  } else {
    ggheatmap <- ggheatmap +
      scale_size_continuous(
        name = ifelse(
          is.null(.legendtitle), "p-value scaling",
          .legendtitle
        ),
        limits = c(0, 3),
        breaks = c(-log10(.05), 2, 3),
        labels = c(.05, .01, .001),
        range = rel(c(.2, maxpoint))
      )
  }
  if (lower_only) {
    ggheatmap <- ggheatmap +
      geom_polygon(data = triangel, aes(x = x, y = y), fill = "white") +
      geom_polygon(data = viereck1, aes(x = x, y = y), fill = "white") +
      geom_polygon(data = viereck2, aes(x = x, y = y), fill = "white") +
      geom_polygon(data = viereck3, aes(x = x, y = y), fill = "white") +
      geom_polygon(data = viereck4, aes(x = x, y = y), fill = "white") +
      guides(
        color = guide_colorbar(
          barwidth = 7, barheight = 1,
          title.position = "top",
          title.hjust = 0.5, order = 2
        ),
        size = guide_legend(
          order = 1,
          title.position = "top"
        )
      ) +
      theme(
        legend.justification = c("right", "top"),
        legend.direction = "horizontal",
        legend.position = c(1, .99)
      )
  }
  return(ggheatmap)
}

