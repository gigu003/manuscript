#' Create typst code chunk
#'
#' @param text Output text.
#' @param text_size Text size (default "20pt").
#' @param text_color Text color (default "luma(50)").
#' @param text_tracking Letter spacing (default "1pt").
#' @param margin Vertical margin (default "2em").
#' @param inset_x Horizontal padding (default "0.5em").
#' @param inset_y Vertical padding (default "0.3em").
#' @param outset_x Left outset (default "0.3em").
#' @param border_size Border thickness (default "0.5em").
#' @param border_color Border color (default "blue").
#'
#' @returns Typst code chunk
#' @export
#'
cr_bar_text <- function(text,
                        text_size = "20pt",
                        text_color = "luma(50)",
                        text_tracking = "1pt",
                        margin = "2em",
                        inset_x = "0.5em",
                        inset_y = "0.3em",
                        outset_x = "0.3em",
                        border_size = "0.5em",
                        border_color = "blue"
                        ){
glue::glue(
"```{{=typst}}
#grid(
columns: (auto, auto),
h({margin}),
block(
inset: (left: {inset_x}, right: {inset_x}, top: {inset_y}, bottom: {inset_y}),
outset: (left: {outset_x}),
stroke: (left: {border_size} + {border_color}),
[#text(size: {text_size},
       fill: {text_color},
       tracking: {text_tracking}
       )[{text}]
]
)
)
```")
}

#' Create typst code chunk
#'
#' @param data Data
#' @param type Type of summary block
#' @param lang Language used, `cn` for Chinese, `en` for English.
#' @param title_size The font size of the title.
#' @param label_size The font size of the value label.
#' @param value_size The font size of the value text.
#' @param fill The color filled in the box.
#'
#' @returns Typst summary block
#' @export
cr_summary_block <- function(
    data,
    type = "incidence",
    lang = "cn",
    title_size = "20pt",
    label_size = "10pt",
    value_size = "18pt",
    fill = "red"
    ){
  rank_des <- lang_variable[["rank"]][[lang]]
  no_cases_des <- ifelse(
    type == "incidence",
    lang_variable[["inci_no_cases"]][[lang]],
    lang_variable[["mort_no_cases"]][[lang]]
  )
  asr_cn2000_des <- lang_variable[["asr_cn2000"]][[lang]]
  type_des <- ifelse(type == "incidence",
                     lang_variable[["inci_title"]][[lang]],
                     lang_variable[["mort_title"]][[lang]]
                     )
  cr_des <- ifelse(type == "incidence",
                     lang_variable[["inci_cr"]][[lang]],
                     lang_variable[["mort_cr"]][[lang]]
  )

text <- glue::glue(
"{{set text(fill: white, tracking: 1pt)
set par(spacing: 0.8em)
let labels = (\"{rank_des}\", \"{no_cases_des}\", \"{cr_des}\", \"{asr_cn2000_des}\", )
let values = (\"{rank}\", \"{no_cases}\", \"{cr}\", \"{asr_cn2000}\", )
let setsize(sz, body) = text(size: sz, body)
setsize({title_size}, [{type_des}])
grid(
columns: (1fr, ) * 4,
align: center,
row-gutter: 1em,
column-gutter: 2pt,
..labels.map(x => setsize({label_size}, x)),
..values.map(x => setsize({value_size}, x)),
)}}", .data = data)

glue::glue(
"```{{=typst}}
#block(fill: {fill}, inset: 15pt, radius: 3pt, {text})
```")
}


#' Text surrounded by rectangle
#'
#' @param text Text.
#' @param fill Background color the rectangle.
#' @param text_color Color of the text.
#' @param text_size Size of the text.
#' @param width Width of the rectangle.
#' @param height Height of the rectangle.
#'
#' @returns Quarto typst code chunk which output text surrounded by rectangle.
#' @export
#'
#' @examples
#' cr_rect_text(text = "Incidence")
cr_rect_text <- function(text = "text",
                         fill = "blue",
                         text_color = "white",
                         text_size = "1em",
                         width = "auto",
                         height = "auto"
                         ) {
glue::glue(
"```{{=typst}}
#set par(first-line-indent: 0em, spacing: 1em, leading: 0.7em)
#set text(fill: {text_color}, size: {text_size})
#align(
horizon + center,
rect(width: {width},
     height: {height},
     fill: {fill},
     radius: 0.3em,
     inset: 0.9em,
     )[{text}]
)
```"
)
}

#' Create bar in original typst
#'
#' @param data data.frame
#' @param x_col Character, name of X axis column
#' @param y_col Character, name of Y axis column
#' @param height Height of the bar
#' @param spacing Spacing between the bars
#' @param bar_color Color of the bars
#' @param text_color Color of the text
#'
#' @return Typst code chunk in quarto document
#' @export
cr_bar <- function(data,
                   x_col, y_col,
                   height = "1em",
                   spacing = "0.4em",
                   bar_color = "red",
                   text_color = "luma(40)") {

  # 检查列是否存在
  if (!(x_col %in% names(data))) stop("x_col not found in data")
  if (!(y_col %in% names(data))) stop("y_col not found in data")

  # 按 x 降序排序
  ord <- order(data[[x_col]], decreasing = TRUE)
  x_vals <- data[[x_col]][ord]
  y_vals <- data[[y_col]][ord]

  # 缩放
  x_scaled <- x_vals / max(x_vals) * 10

  # 拼接 labels 和 values
  labels <- paste0("(", paste0("\"", y_vals, "\"", collapse = ", "), ",)")
  values <- paste0("(", paste(x_scaled, collapse = ", "), ")")

  # 返回 Typst 代码
  glue::glue(
    "```{{=typst}}
#set par(leading: 0em, spacing: 0em,)
#let values = {values}
#let labels = {labels}
#grid(
columns: (auto, auto),
column-gutter: 0.3em,
align: (right, left),
stack(
dir: ttb,
spacing: {spacing},
..(labels.map(x => box(height: {height},
                        align(horizon,text(fill:{text_color}, size: {height}*0.8)[#x])
                       )))
),
stack(
dir: ttb,
spacing: {spacing},
..(values.map(n => rect(width: (n*1em), height: {height}, fill: {bar_color})))
)
)
```"
  )
}
