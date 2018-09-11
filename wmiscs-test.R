modules::depend("magrittr")
modules::import("magrittr", "%>%")

modules::use("wmiscs.R", attach = TRUE)

equals <- function(x, y) {
  stopifnot(all.equal(x, y))
}

as.function(~.[[1]])(list(1)) %>% equals(1)
as.function(x ~ x[[1]])(list(1)) %>% equals(1)
as.function(f(x) ~ x[1])(list(1)) %>% equals(list(1))
as.function(f(x, y) ~ x + y)(1, 2) %>% equals(3)

is.even <- function(x) (x %% 2) == 0

equals(extract(1:10, is.even), c(2, 4, 6, 8, 10))
equals(extract(1:10, ~ . %% 2 == 0), c(2, 4, 6, 8, 10))
equals(extract(1:10, c(FALSE, TRUE)), c(2, 4, 6, 8, 10))
equals(extract(1:10, 1:2), 1:2)
equals(extract(data.frame(x = 1, y = ""), is.numeric), data.frame(x = 1))
equals(extract(list(x = 1, y = ""), is.numeric), list(x = 1))
equals(extract(list(x = 1, y = ""), "y"), list(y = ""))
equals(extract(list(xy = 1, zy = 2), "^z"), list(zy = 2))

equals(extract2(3:4, is.even), 4)
equals(extract2(as.list(1:10), ~ . %% 2 == 0), 2)
equals(extract2(1:10, 1), 1)
equals(extract2(data.frame(x = 1, y = ""), is.numeric), 1)
equals(extract2(list(x = 1, y = ""), is.numeric), 1)
equals(extract2(list(x = 1, y = ""), "y"), "")
equals(extract2(list(xy = 1, zy = 2), "^z"), 2)

equals(replace(c(1, 2, NA), ~ is.na(.), 3), 1:3)
equals(replace(c(1, 2, NA), rep(TRUE, 3), 3), rep(3, 3))
equals(replace(c(1, 2, NA), 3, 3), 1:3)
equals(replace(list(x = 1, y = 2), "x", 0), list(x = 0, y = 2))
equals(
  replace(list(x = 1, y = 2), "^x$", 0),
  list(x = 0, y = 2))
equals(
  replace(list(x = 1, y = 2), ~ . == 1, 0),
  list(x = 0, y = 2))
equals(
  replace(table(letters[1:2]), ~ . == 1, 2),
  table(rep(letters[1:2], 2)))
