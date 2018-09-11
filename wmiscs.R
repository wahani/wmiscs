as.function.formula <- function(x, ...) {
  if (length(x) == 2) {
    args <- deparseLhs(x)
  }
  else if (length(x) == 3) {
    args <- getArgs(x)
  }
  else stop ("Don't know what to do with this formula.")
  aoos:::makeFunDef(args, deparseRhs(x), environment(x))
}

deparseLhs <- function(x) {
  if (length(x) == 2) "." else deparse(x[[2]])
}

deparseRhs <- function(x) {
  if (length(x) == 2) deparse(x[[2]]) else deparse(x[[3]])
}

getArgs <- function(x) {
  lhs <- deparseLhs(x)
  lhs <- rev(aoos:::splitTrim(lhs, ":"))[1]
  if (grepl("\\(", lhs)) {
    lhs <- aoos:::deleteBeforeParan(lhs)
    lhs <- aoos:::deleteEnclosingParan(lhs)
    aoos:::splitTrim(lhs, ",")
  } else {
    lhs
  }
}

extract <- function(x, ind, ...) UseMethod("extract", ind)

extract.default <- function(x, ind, ...) x[ind, ...]

extract.function <- function(x, ind, ...) {
  if (is.list(x)) x[vapply(x, ind, logical(1), ...)]
  else if (is.atomic(x)) x[ind(x, ...)]
  else stop("extract.function: No implementation for", class(x)[1])
}

extract.formula <- function(x, ind, ...) {
  extract(x, as.function(ind), ...)
}

extract.character <- function(x, ind, ...) {
  stopifnot(!is.null(names(x)))
  ind <- if (length(ind) == 1 && grepl("^\\^", ind)) {
    grepl(ind, names(x), ...)
  } else {
    stopifnot(all(ind %in% names(x)))
    names(x) %in% ind
  }
  extract(x, ind)
}

extract2 <- function(x, ind, ...) extract(x, ind, ...)[[1]]

replace <- function(x, ind, values, ...) UseMethod("replace", ind)

replace.default <- function(x, ind, values, ...) base::replace(x, ind, values)

replace.function <- function(x, ind, values, ...) {
  if (is.atomic(x)) replace(x, ind(x, ...), values)
  else if (is.list(x)) replace(x, vapply(x, ind, logical(1), ...), values)
  else stop("replace.function: No implementation for", class(x)[1])
}

replace.formula <- function(x, ind, values, ...) {
  replace(x, as.function(ind), values, ...)
}

replace.character <- function(x, ind, values, ...) {
  stopifnot(!is.null(names(x)))
  ind <- if (length(ind) == 1 && grepl("^\\^", ind)) {
    grepl(ind, names(x), ...)
  } else {
    stopifnot(all(ind %in% names(x)))
    names(x) %in% ind
  }
  replace(x, ind, values, ...)
}
