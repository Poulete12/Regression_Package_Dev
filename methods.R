# gpava_broom_methods.R
# Méthodes S3 pour broom (tidy, augment, glance) et autoplot pour gpava()

# Imports roxygen2
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom broom tidy augment glance
#' @importFrom ggplot2 autoplot ggplot aes geom_point geom_step geom_line geom_segment labs
#' @export


# ─────────────────────────────────────────────────────────────────────────────
# 0.  Fonction utilitaire ------------------------------------------------------
# Construit un tibble commun avec colonne index, x, y, fitted, residual.
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
  # Récupère l'abscisse d'origine si disponible, sinon 1:n
  if (!is.null(obj$x_pred))        return(obj$x_pred)
  if (!is.null(obj$x)) {
    # Attention : dans gpava, obj$x n'est pas le prédicteur mais l'ordonnateur ;
    # si toutes les valeurs sont identiques, on considère qu'il ne s'agit pas
    # d'une vraie abscisse.
    if (length(unique(obj$x)) > 1) return(obj$x)
  }
  seq_len(n)
}

# ─────────────────────────────────────────────────────────────────────────────
# 1. gpava --------------------------------------------------------------------


# tidy.gpava : retourne index, x_pred (abscisses), y, fitted (object$z), residual
tidy.gpava <- function(object, ...) {
  y_vec      <- object$y
  fitted_vec <- object$x    # <-- c'est ici que doit apparaître object$z
  x_vec      <- if (!is.null(object$x_pred)) object$x_pred else object$x

  tibble::tibble(
    index    = seq_along(y_vec),
    x_pred   = x_vec,
    y        = y_vec,
    fitted   = fitted_vec,
    residual = y_vec - fitted_vec
  )
}

# augment.gpava : ajoute .fitted et .resid au data.frame
#' @export
augment.gpava <- function(object, data = NULL, ...) {
  y_vec      <- object$y
  fitted_vec <- object$x
  x_vec      <- if (!is.null(object$x_pred)) object$x_pred else object$x

  if (is.null(data)) {
    data <- tibble::tibble(x_pred = x_vec, y = y_vec)
  }

  tibble::as_tibble(data) %>%
    dplyr::mutate(
      .fitted = fitted_vec,    # <-- object$z ici aussi
      .resid  = y - .fitted
    )
}

# glance.gpava : résumé n et RSS
#' @export
glance.gpava <- function(object, ...) {
  y_vec      <- object$y
  fitted_vec <- object$z      # <-- et ici
  rss        <- sum((y_vec - fitted_vec)^2)

  tibble::tibble(
    n   = length(y_vec),
    rss = rss
  )
}

# autoplot.gpava : trace avec geom_step ou geom_line
#' @export
autoplot.gpava <- function(object, step = FALSE, residuals = FALSE, ...) {
  x_vec      <- if (!is.null(object$x_pred)) object$x_pred else seq_along(object$y)
  df         <- tibble::tibble(x = x_vec, y = object$y, fitted = object$x)  # <-- object$z !

  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(...) +
    labs(x = "x", y = "y")

  if (step) {
    p <- p + geom_step(aes(y = fitted), direction = "hv", ...)
  } else {
    p <- p + geom_line(aes(y = fitted), ...)
  }

  if (residuals) {
    p <- p + geom_segment(aes(x = x, xend = x, y = y, yend = fitted),
                          colour = "red", ...)
  }

  p
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. activeset ----------------------------------------------------------------


#' @export
tidy.activeset <- function(object, ...) {
  if (is.null(object$y)) stop("L'objet activeset ne contient pas $y (réponse).")
  n <- length(object$y)
  message(sprintf("[tidy.activeset] Démarrage avec %d observations", n))
  x_vec <- .pick_x(object, n)
  .build_iso_tbl(x_vec, object$y, object$x)
}

#' @export
augment.activeset <- function(object, data = NULL, ...) {
  if (is.null(object$y)) stop("L'objet activeset ne contient pas $y (réponse).")
  n <- length(object$y)
  message("[augment.activeset] Démarrage")
  x_vec <- .pick_x(object, n)
  if (is.null(data)) {
    message("[augment.activeset] Pas de données fournies : reconstruction")
    data <- tibble::tibble(x = x_vec, y = object$y)
  }
  dplyr::mutate(tibble::as_tibble(data),
                .fitted = object$x,
                .resid  = y - object$x)
}

#' @export
glance.activeset <- function(object, ...) {
  if (is.null(object$y)) stop("L'objet activeset ne contient pas $y (réponse).")
  message("[glance.activeset] Calcul de la synthèse")
  tibble::tibble(n = length(object$y), rss = sum((object$y - object$x)^2))
}

#' @export
autoplot.activeset <- function(object, step = TRUE, residuals = FALSE, ...) {
  if (is.null(object$y)) stop("L'objet activeset ne contient pas $y (réponse).")
  message(sprintf("[autoplot.activeset] Construction du graphique (step = %s)", step))
  n   <- length(object$y)
  x_v <- .pick_x(object, n)
  df  <- .build_iso_tbl(x_v, object$y, object$x)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()
  if (step) {
    p <- p + ggplot2::geom_step(ggplot2::aes(y = fitted), direction = "hv")
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes(y = fitted))
  }
  if (residuals) {
    message("[autoplot.activeset] Ajout des résidus")
    p <- p + ggplot2::geom_segment(ggplot2::aes(xend = x, yend = fitted), colour = "red")
  }
  p + ggplot2::labs(x = "x", y = "y") + ggplot2::theme_minimal()
}

