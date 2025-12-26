#' Create figure box value
#'
#' @param name Name of the figure.
#' @param value Value
#'
#' @returns Typst code chunk used in Quarto document
#' @export
#'
cr_figure_value <- function(name = "men", value = "45.2%") {
glue::glue(
"```{{=typst}}
#let original = read(\"men.svg\", encoding: none)
#let color = original.replace(\"#2B80FF\", green.to-hex(),)
#grid(
columns: (5fr, 2fr),
grid(
  columns: (1fr,) * 25,
  gutter: 5pt,
  ..(grid.cell()[#image(bytes(original))], ) * 24,
    ..(grid.cell()[#image(bytes(color))], ) * 76,
      ),
    place(horizon + center,
          text(size:2em, fill: blue)[{value}])
    )
```"
)
}


