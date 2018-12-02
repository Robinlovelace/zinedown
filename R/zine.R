#' Create zines with RMarkdown
#'
#' This function can be used in the YAML of Rmd files
#' to make zines.
#'
#' @export
#' @param toc A Boolean (TRUE or FALSE) specifying whether table of contents should be created
#' @param toc_depth A positive integer
#' @param highlight Syntax highlighting style. Supported styles include "default", "tango", "pygments", "kate", "monochrome", "espresso", "zenburn", and "haddock". Pass NULL to prevent syntax highlighting.
#' @return A zine!
#' @examples
#' \dontrun{
#'  output: zinedown::zine_pdf
#' }
zine_pdf <- function(toc = TRUE, toc_depth = 3, highlight = "default", ...){

  base <- bookdown::pdf_book(template = "8up-zine.tex",
    toc = toc,
    toc_depth = toc_depth,
    highlight = highlight,
    keep_tex = TRUE,
    ...)

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  #base$knitr$opts_chunk$fig.align <- "center"

  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = fix_envs)
  on.exit(options(bookdown.post.late = old_opt))

  base

}
#' @rdname zine_pdf
#' @export
#' @return A gitbook webpage
#' @examples
#' \dontrun{
#'  output: zinedown::zine_gitbook
#' }
zine_gitbook <- function(...){

  base <- bookdown::gitbook(
    split_by = "chapter+number",
    config = list(toc = list(collapse = "section",
      before = '<li><a href="./"></a></li>',
      after = '<li><a href="https://github.com/rstudio/bookdown" target="blank">Published with bookdown</a></li>',
      ...)
    )
  )

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"

  base

}
#' @rdname zine_pdf
#' @export
#' @return A ebook version of the zine
#' @examples
#' \dontrun{
#'  output: zinedown::zine_epub
#' }
zine_epub <- function(...){

  base <- bookdown::epub_book(...)

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"

  base

}

fix_envs = function(x){
  beg_reg <- '^\\s*\\\\begin\\{.*\\}'
  end_reg <- '^\\s*\\\\end\\{.*\\}'
  i3 = if (length(i1 <- grep(beg_reg, x))) (i1 - 1)[grepl("^\\s*$", x[i1 - 1])]

  i3 = c(i3,
         if (length(i2 <- grep(end_reg, x))) (i2 + 1)[grepl("^\\s*$", x[i2 + 1])]
  )
  if (length(i3)) x = x[-i3]
  x
}
