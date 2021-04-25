GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                        required_aes = c("x"),
                        default_aes = ggplot2::aes(y=0, shape = 19, size = 1, alpha = 0.75,
                                          colour = "grey", fill = NA),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_scales, coord){
                          ## Transform the data first
                          coords <- coord$transform(data, panel_scales)
                          coords$size <- coords$size / 3
                          seg <- grid::segmentsGrob(x0 = min(coords$x),
                                              x1 = max(coords$x),
                                              y0 = coords$y,
                                              y1 = coords$y)
                          pts <- grid::pointsGrob(x = coords$x,
                                            y = coords$y,
                                            pch = coords$shape,
                                            gp = grid::gpar(cex = coords$size,
                                                      alpha = coords$alpha,
                                                      col = coords$colour))
                          grid::gTree(children = grid::gList(seg, pts))
                        })

#' @title Timeline of Earthquakes.
#'
#' @description A geom for ggplot2 for plotting a time line of earthquakes ranging from xmin to xmaxdates
#'  with a point for each earthquake. Optional aesthetics include color, size, and alpha (for transparency).
#'  The xaesthetic is a date and an optional y aesthetic is a factor indicating some stratification in which
#'  case multiple time lines will be plotted for each level of the factor (e.g. country).
#'
#' @param mapping Set of aesthetic mappings created by \href{https://www.rdocumentation.org/packages/ggplot2/versions/3.3.3/topics/aes}{aes()} or \href{https://www.rdocumentation.org/packages/ggplot2/versions/3.3.3/topics/aes_}{aes_()}.
#'  If specified and `inherit.aes = TRUE`` (the default), it is combined with the default mapping at the top
#'  level of the plot. You must supply mapping if there is no plot mapping
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified in the call to \href{https://www.rdocumentation.org/packages/ggplot2/versions/3.3.3/topics/ggplot}{ggplot()}.
#'
#' A data.frame, or other object, will override the plot data. All objects will be fortified to produce a
#' data frame. See \href{https://www.rdocumentation.org/packages/precrec/versions/0.12.5/topics/fortify}{fortify()} for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame,
#' and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if
#' any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to
#' finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' This is most useful for helper functions that define both data and aesthetics and shouldn't inherit
#' behaviour from the default plot specification, e.g. \href{https://www.rdocumentation.org/packages/ggplot2/versions/0.9.0/topics/borders}{borders()}.
#' @param ... Other arguments passed on to \href{https://ggplot2.tidyverse.org/reference/layer.html}{layer()}. These are often aesthetics, used to set an aesthetic to a
#' fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#' @return A plot of the timeline of earthquakes.
#' @export
#'
#' @import ggplot2
#' @import grid
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' data(eaq_data)
#' eaq_data%>%
#'   eq_clean_data()%>%
#'   dplyr::filter(`Country` %in% c("MEXICO") & !(is.na(Mag)))
#'   ggplot(data = data, aes(x = Year, y = Country, size = Mag, colour = `Total Deaths`)) +
#'    geom_timeline(alpha = 0.3)
#' }
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                        required_aes = c("x"),
                        default_aes = ggplot2::aes(y=0, shape = 19, size = 1, alpha = 0.75,
                                                   len = 10, label = NA, rot = 45, n_max = 10,
                                                   colour = "grey", fill = NA, check_overlap = FALSE),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_scales, coord){
                          ## Transform the data first
                          coords <- coord$transform(data, panel_scales)
                          if (nrow(coords) >= 10) {
                            coords1 <- coords %>%
                              dplyr::arrange(desc(size)) %>%
                              dplyr::slice(1:n_max) %>%
                              dplyr::mutate(len = scales::rescale(len, from = panel_scales$y.range))
                          }
                          else{
                            coords1 <- coords
                          }

                          coords$size <- coords$size / 3
                          seg1 <- grid::segmentsGrob(x0 = unit(min(coords$x), "native"),
                                               x1 = unit(max(coords$x), "native"),
                                               y0 = unit(coords$y, "native"),
                                               y1 = unit(coords$y, "native"))
                          pts <- grid::pointsGrob(x = unit(coords$x, "native"),
                                            y = unit(coords$y, "native"),
                                            pch = coords$shape,
                                            gp = grid::gpar(cex = coords$size,
                                                      alpha = coords$alpha,
                                                      col = coords$colour))
                          seg2 <- segmentsGrob(x0 = unit(coords1$x, "native"),
                                               x1 = unit(coords1$x, "native"),
                                               y0 = unit(coords1$y, "native"),
                                               y1 = unit(coords1$len, "cm"),
                                               gp = grid::gpar(alpha = coords1$alpha))
                          txt <- textGrob(label = coords1$label,
                                          rot = coords1$rot,
                                          x = unit(coords1$x, "native"),
                                          y = unit(coords1$len + 0.5, "cm"),
                                          check.overlap = coords1$check_overlap)

                          grid::gTree(children = grid::gList(seg1, pts, seg2, txt))
                        })
#' @title Timeline of Earthquakes (annoted).
#'
#' @description  A geom for adding annotations to the earthquake data. This geom adds a vertical line to each
#' data point with a text annotation (e.g. the location of the earthquake) attached to each line.
#' There should be an option to subset to n_max number of earthquakes, where we take the n_max largest
#' (by magnitude) earthquakes. Aesthetics are x, which is the date of the earthquake and label which takes
#' the column name from which annotations will be obtained.
#'
#' @param mapping Set of aesthetic mappings created by \href{https://www.rdocumentation.org/packages/ggplot2/versions/3.3.3/topics/aes}{aes()} or \href{https://www.rdocumentation.org/packages/ggplot2/versions/3.3.3/topics/aes_}{aes_()}.
#'  If specified and `inherit.aes = TRUE`` (the default), it is combined with the default mapping at the top
#'  level of the plot. You must supply mapping if there is no plot mapping
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If NULL, the default, the data is inherited from the plot data as specified in the call to \href{https://www.rdocumentation.org/packages/ggplot2/versions/3.3.3/topics/ggplot}{ggplot()}.
#'
#' A data.frame, or other object, will override the plot data. All objects will be fortified to produce a
#' data frame. See \href{https://www.rdocumentation.org/packages/precrec/versions/0.12.5/topics/fortify}{fortify()} for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame,
#' and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if
#' any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to
#' finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' This is most useful for helper functions that define both data and aesthetics and shouldn't inherit
#' behaviour from the default plot specification, e.g. \href{https://www.rdocumentation.org/packages/ggplot2/versions/0.9.0/topics/borders}{borders()}.
#' @param ... Other arguments passed on to \href{https://ggplot2.tidyverse.org/reference/layer.html}{layer()}. These are often aesthetics, used to set an aesthetic to a
#' fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#' @return A plot of the timeline of earthquakes with annotations.
#'
#' @import ggplot2
#' @import grid
#' @import scales
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' data(eaq_data)
#' eaq_data%>%
#'   eq_clean_data()%>%
#'   dplyr::filter(`Country` %in% c("MEXICO") & !(is.na(Mag))) %>%
#'   ggplot(aes(x = Year, y = Country, size = Mag, colour = `Total Deaths`)) +
#'    geom_timeline_label(alpha = 0.3, label = `Location Name`)
#' }
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



