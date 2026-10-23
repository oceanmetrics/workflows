# tests for libs/ga-head.ts, the analytics include of every notebook: the real _quarto.yml and libs/ rendered on a
# one-page project in a temp dir with GA_ID unset, malformed and set to a dummy ----

library(testthat)

# locate the repo root (works from the repo root or tests/testthat) ----
.root <- c(".", "../..")
.root <- normalizePath(.root[file.exists(file.path(.root, "_quarto.yml"))][1])

test_that("_quarto.yml runs the pre-render and includes its output in every page", {
  q <- yaml::read_yaml(file.path(.root, "_quarto.yml"))
  expect_equal(q$project$`pre-render`, "libs/ga-head.ts")
  expect_true("libs/ga-head.html" %in% q$format$html$`include-in-header`)
  expect_true("libs/ga-head.html" %in% readLines(file.path(.root, ".gitignore")))
})

# render a one-page copy of the project with GA_ID = ga (NA: unset); the page's html ----
render_page <- function(ga = NA) {
  d <- tempfile("ga-site-")
  dir.create(d)
  file.copy(file.path(.root, "_quarto.yml"), d)
  file.copy(file.path(.root, "libs"), d, recursive = TRUE)
  unlink(file.path(d, "libs", "ga-head.html"))
  writeLines(c("---", "title: a page", "---", "", "Some text."), file.path(d, "a.qmd"))
  # system2 runs through the shell: quote the value
  env <- if (is.na(ga)) "GA_ID=" else paste0("GA_ID=", shQuote(ga))
  out <- withr::with_dir(d, system2("quarto", c("render", "a.qmd", "--quiet"), env = env, stdout = TRUE, stderr = TRUE))
  html <- file.path(d, "_output", "a.html")
  if (!file.exists(html)) stop("quarto render failed: ", paste(out, collapse = "\n"))
  paste(readLines(html, warn = FALSE), collapse = "\n")
}

has_quarto <- nzchar(Sys.which("quarto"))

test_that("GA_ID unset: no analytics script, no id, no gtag", {
  skip_if_not(has_quarto, "no quarto on PATH")
  h <- render_page(NA)
  expect_match(h, "analytics off: no GA_ID at render", fixed = TRUE)
  expect_false(grepl("installGa|googletagmanager|gtag", h))
  expect_false(grepl("G-[A-Z0-9]{6,}", h))
})

test_that("GA_ID malformed: the same as unset (nothing can be injected through it)", {
  skip_if_not(has_quarto, "no quarto on PATH")
  h <- render_page('G-TEST000000"><script>alert(1)</script>')
  expect_false(grepl("installGa|alert", h))
})

test_that("GA_ID=G-TEST000000: the nav loader for the id, anonymized, pinned", {
  skip_if_not(has_quarto, "no quarto on PATH")
  h <- render_page("G-TEST000000")
  expect_match(h, 'import { installGa } from "https://cdn.jsdelivr.net/npm/@oceanmetrics/nav@0.2.0/dist/ga.js";', fixed = TRUE)
  expect_match(h, 'installGa("G-TEST000000", { anonymize: true, pageView: "load" });', fixed = TRUE)
  # the loader appends the gtag.js tag at run time: the page itself carries none
  expect_false(grepl("<script[^>]*googletagmanager", h))
})
