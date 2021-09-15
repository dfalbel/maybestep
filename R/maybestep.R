#' Allows tuning the existance of a step.
#'
#' @inheritParams recipes::step_normalize
#' @param step_fn A recipe step function that will be added to `recipe`.
#' @param ... Additional arguments passed to `step_fn`.
#' @param use Whether to include `step_fn` or not into the `recipe`.
#'
#'
#' @importFrom recipes bake prep add_step step rand_id tunable
#' @importFrom tune tune_args
#' @export
step_maybe <- function(
  recipe,
  step_fn,
  use,
  ...,
  trained = FALSE,
  skip = FALSE,
  id = rand_id("maybestep")
) {

  original_step <- step_fn(recipe, ..., trained = trained,
                           skip = skip, id = id)
  original_step <- original_step$steps[[length(original_step$steps)]]

  add_step(
    recipe,
    step_maybe_new(
      step = original_step,
      use = use,
      role = original_step$role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_maybe_new <- function(step, use, role, trained, skip, id) {
    step(
      subclass = "maybe",
      step = step,
      use = use,
      trained = trained,
      skip = skip,
      id = id,
      role = role
    )
}

#' @export
prep.step_maybe <- function(x, training, info = NULL) {
  x$trained <- TRUE
  if (!x$use) return(x)
  x$step <- prep(x$step, training = training, info = info)
  x
}

#' @export
bake.step_maybe <- function(object, new_data, ...) {
  if (!object$use) return(new_data)
  bake(object$step, new_data, ...)
}

#' @export
tunable.step_maybe <- function(x, ...) {
  dplyr::bind_rows(
    tunable(x$step),
    tibble::tibble(
      name = c("use"),
      call_info = list(list(pkg = "maybestep", fun = "use_step")),
      source = "recipe",
      component = "step_maybe",
      component_id = x$id
    )
  )
}

#' @export
use_step <- function(values = c(FALSE, TRUE)) {
  dials::new_qual_param(
    type = "logical",
    values = values,
    default = dials::unknown(),
    label = c("use_step" = "Using the selected step"),
    finalize = NULL
  )
}

#' @export
tune_args.step_maybe <- function(object, full = FALSE, ...) {
  args <- NextMethod()
  dplyr::bind_rows(args, tune::tune_args(object$step))
}

#' @export
merge.step_maybe <- function(object, ...) {
  object$step <- merge(object$step, ...)
  NextMethod()
}
