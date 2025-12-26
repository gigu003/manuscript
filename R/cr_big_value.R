#' Create big value typst code chunk
#'
#' @param text Description text.
#' @param value Big value.
#' @param text_size Size of the text.
#' @param value_size Size of the value.
#' @param height_top Height of the top box.
#' @param height_bottom Height of the bottom box.
#' @param width Width of the box.
#' @param color Color of the background.
#'
#' @returns Typst code chunk
#' @export
#'
cr_big_value <- function(text = "5 year survival",
                         value = "34.5%",
                         text_size = "15pt",
                         value_size = "40pt",
                         height_bottom = "5em",
                         height_top = "3em",
                         width = "10em",
                         color = "blue"
                         ) {
glue::glue(
"```{{=typst}}
#align(center)[
  #stack(
    dir: ttb,
    spacing: 0pt,
    box(
      width: {width},
      height: {height_top},
      inset: (x: 12pt, y: 10pt),
      radius: (top-left: 6pt, top-right: 6pt),
      fill: {color},
      stroke: {color},
    )[
      #text(size: {text_size}, weight: \"bold\", fill: white)[{text}]
    ],
    box(
      width: {width},
      height: {height_bottom},
      inset: (x: 12pt, y: 10pt),
      radius: (bottom-left: 6pt, bottom-right: 6pt),
      fill: white,
      stroke: {color},
    )[
      #align(center + horizon)[
        #text(size: {value_size}, weight: \"bold\", fill: {color})[{value}]
      ]
    ]
  )
]
```"
)
}
