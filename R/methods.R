#' @description  Méthodes S3 pour \code{gpava} et \code{activeset}.
#' @keywords internal
"_PACKAGE"

# ─────────────────────────────────────────────────────────────────────────────
# Imports (roxygen)
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom ggplot2 autoplot ggplot aes geom_point geom_step geom_line
#'   geom_segment labs theme_minimal
#' @exportPattern ^tidy\\.|^augment\\.|^glance\\.|^autoplot\\.

# ─────────────────────────────────────────────────────────────────────────────
# 0.  Outils internes ----------------------------------------------------------

.build_iso_tbl <- function(x_vec, y_vec, fit_vec) {
  tibble::tibble(
    index    = seq_along(y_vec),
    x        = x_vec,
    y        = y_vec,
    fitted   = fit_vec,
    residual = y_vec - fit_vec
  )
}

.pick_x <- function(obj, n) {
  if (!is.null(obj$z))         return(obj$z)        # prédicteur réel (gpava)
  if (!is.null(obj$x_pred))    return(obj$x_pred)   # cas spécial utilisateur
  seq_len(n)                                       # défaut : 1,2,…,n
}

# ─────────────────────────────────────────────────────────────────────────────
# 1.  Méthodes pour classe « gpava » ------------------------------------------

#' @export
tidy.gpava <- function(object, ...) {
  y_vec      <- object$y
  fitted_vec <- object$x                       # valeurs ajustées
  x_vec      <- .pick_x(object, length(y_vec)) # abscisses

  tibble::tibble(
    index    = seq_along(y_vec),
    x_pred   = x_vec,
    y        = y_vec,
    fitted   = fitted_vec,
    residual = y_vec - fitted_vec
  )
}

#' @export
augment.gpava <- function(object, data = NULL, ...) {
  y_vec      <- object$y
  fitted_vec <- object$x
  x_vec      <- .pick_x(object, length(y_vec))

  if (is.null(data))
    data <- tibble::tibble(x_pred = x_vec, y = y_vec)

  tibble::as_tibble(data) |>
    dplyr::mutate(
      .fitted = fitted_vec,
      .resid  = y - .fitted
    )
}

#' @export
glance.gpava <- function(object, ...) {
  tibble::tibble(
    n   = length(object$y),
    rss = sum((object$y - object$x)^2)
  )
}

#' @export
autoplot.gpava <- function(object, step = FALSE, residuals = FALSE, ...) {
  x_vec <- .pick_x(object, length(object$y))
  df    <- tibble::tibble(x = x_vec, y = object$y, fitted = object$x)

  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point(alpha = .25, size = .8)

  p <- if (step)
    p + ggplot2::geom_step(ggplot2::aes(y = fitted),
                           direction = "hv",
                           colour   = "blue",
                           linewidth = 1)
  else
    p + ggplot2::geom_line(ggplot2::aes(y = fitted),
                           linewidth = 1)

  if (residuals)
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(xend = x, yend = fitted),
      colour = "red", ...)

  p + ggplot2::labs(x = "x", y = "y") +
    ggplot2::theme_minimal()
}


# ─────────────────────────────────────────────────────────────────────────────
# 2.  Méthodes pour classe « activeset » --------------------------------------

#' @export
tidy.activeset <- function(object, ...) {
  stopifnot(!is.null(object$y))
  .build_iso_tbl(.pick_x(object, length(object$y)),
                 object$y, object$x)
}

#' @export
augment.activeset <- function(object, data = NULL, ...) {
  stopifnot(!is.null(object$y))
  x_vec <- .pick_x(object, length(object$y))
  if (is.null(data))
    data <- tibble::tibble(x = x_vec, y = object$y)

  dplyr::mutate(tibble::as_tibble(data),
                .fitted = object$x,
                .resid  = y - object$x)
}

#' @export
glance.activeset <- function(object, ...) {
  stopifnot(!is.null(object$y))
  tibble::tibble(
    n   = length(object$y),
    rss = sum((object$y - object$x)^2)
  )
}

#' @export
autoplot.activeset <- function(object, step = TRUE,
                               residuals = FALSE, ...) {
  stopifnot(!is.null(object$y))
  df <- .build_iso_tbl(.pick_x(object, length(object$y)),
                       object$y, object$x)

  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point(alpha = .25, size = .8)      # ← ajouté

  p <- if (step)
    p + ggplot2::geom_step(ggplot2::aes(y = fitted),
                           direction = "hv",
                           colour   = "red",
                           linewidth = 1)            # ← ajouté
  else
    p + ggplot2::geom_line(ggplot2::aes(y = fitted),
                           linewidth = 1)            # ← ajouté

  if (residuals)
    p <- p + ggplot2::geom_segment(
      ggplot2::aes(xend = x, yend = fitted),
      colour = "red")

  p + ggplot2::labs(x = "x", y = "y") +
    ggplot2::theme_minimal()
}
