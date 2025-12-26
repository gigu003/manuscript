#' Create a new Quarto document from a template
#'
#' This function creates a new Quarto document with specified parameters,
#' including YAML headers and content based on the document type. It handles
#' directory creation, file naming, and existing file checks.
#'
#' @param name Name of the Quarto document (default: "a new quarto document").
#' @param dir The directory of the document.
#' @param sub_dir The sub directory in the directory.
#' @param use_folder Create document in a dedicated folder (default: TRUE).
#' @param qmd_type Type of Quarto document (blog, slides, report, article; default: "blog").
#' @param prefix Naming prefix (NULL, "date", "rank", or custom string; default: "date").
#' @param include_yaml Include YAML header (default: TRUE).
#' @param title Title in the yaml header.
#' @param subtitle Subtitle in the yaml header.
#' @param lang Document language (en, cn, zh-cn; default: "cn").
#' @param author Document author (default: "Qiong Chen").
#' @param date Document date (default: current date).
#' @param overwrite Overwrite existing file (default: FALSE).
#'
#' @return Message indicating document creation path.
#' @export
#'
#' @examples
#' \dontrun{
#' use_qmd(name = "My Report", qmd_type = "report")
#' }
use_qmd <- function(name = "a new quarto document",
                    dir = "qsight",
                    sub_dir = NULL,
                    use_folder = TRUE,
                    qmd_type = "blog",
                    prefix = "date",
                    include_yaml = TRUE,
                    title = name,
                    subtitle = "subtitle",
                    lang = "cn",
                    author = "Qiong Chen",
                    date = Sys.Date(),
                    overwrite = FALSE) {
  # Validate parameters
  valid_qmd_types <- c("blog", "slides", "report", "article")
  if (!qmd_type %in% valid_qmd_types) {
    stop("Invalid qmd_type. Valid options: ", paste(valid_qmd_types, collapse = ", "))
  }

  valid_langs <- c("cn", "en", "zh-cn")
  if (!lang %in% valid_langs) {
    stop("Invalid lang. Valid options: ", paste(valid_langs, collapse = ", "))
  }

  # Set default output directories
  if (is.null(sub_dir)) {
    output_dir <- dir
  } else {
    output_dir <- fs::path(dir, sub_dir)
  }

  # Create output directory if needed
  if (!fs::dir_exists(output_dir)) {
    fs::dir_create(output_dir, recurse = TRUE)
  }

  cname <- gsub("[^a-zA-Z0-9_-]", "-", name)
  cname <- tolower(gsub("-+", "-", cname))

  # Generate prefix
  p_name <- switch(prefix,
                   "date" = format(Sys.time(), "%Y%m%d"),
                   "rank" = sprintf("%03d", list_qmd_no(output_dir) + 1),
                   prefix
  )

  # Apply prefix if specified
  if (!is.null(p_name)) {
    cname <- paste(p_name, cname, sep = "-")
  }

  # Create file path
  if (use_folder) {
    full_path <- file.path(output_dir, cname)
    if (!dir.exists(full_path)) {
      dir.create(full_path, recursive = TRUE)
    }
    qmd_path <- file.path(full_path, "index.qmd")
  } else {
    qmd_path <- file.path(output_dir, paste0(cname, ".qmd"))
  }

  header <- if (include_yaml) {
    yaml_header(
      title = title,
      subtitle = subtitle,
      lang = lang,
      author = author,
      date = Sys.Date())
    } else ""

  content <- qmd_content(qmd_type = qmd_type, lang = lang)

  # Write file with overwrite logic
  if (!file.exists(qmd_path) || overwrite) {
    writeLines(paste0(header, content), qmd_path)
    message("Created new Quarto document:\n  ", normalizePath(qmd_path))
    tryCatch(file.edit(qmd_path),
             error = function(e) message("Unable to open file automatically"))
  } else {
    message("Document already exists:\n  ", normalizePath(qmd_path))
  }

  invisible(qmd_path)
}


yaml_header <- function(
    title = "title",
    subtitle = "subtitle",
    lang = "cn",
    author = "Qiong Chen",
    date = Sys.Date(),
    date_format = "YYYYMMDD"
    ) {
glue::glue(
"---
title: \"{title}\"
subtitle: \"{subtitle}\"
lang: {lang}
author: \"{author}\"
date: {date}
date-modified: last-modified
date-format: \"{date_format}\"
---\n\n"
  )
}

qmd_content <- function(lang = "en", qmd_type = "article") {
  if (qmd_type == "article") {
    if (lang == "en") {
      content <- paste0(
        "## Introduction\n\n",
        "## Methods\n\n",
        "## Result\n\n",
        "## Discussion\n\n",
        "## References {{.unnumbered}}\n\n",
        "::: {{#refs}}\n:::\n" )
    } else if (lang == "zh-cn") {
      content <- paste0(
        "## \u524d\u8a00\n\n",
        "## \u65b9\u6cd5\n\n",
        "## \u7ed3\u679c\n\n",
        "## \u8ba8\u8bba\n\n",
        "## \u53c2\u8003\u6587\u732e {{.unnumbered}}\n\n",
        "::: {{#refs}}\n:::\n" ) }
  } else if (qmd_type == "supplement") {
    content <- paste0("## Supplementary material\n\n")
  } else {
    content <- paste0("## Welcome\n",
                      "This is a Quarto Markdown document created using the `use_qmd` function.\n",
                      "Content can be added here.\n")
  }
  return(content)
}


list_qmd_no <- function(directory = ".") {
  all_dirs <- list.dirs(directory, recursive = TRUE)
  exclude_dirs <- c("_extensions", "_site", ".quarto", ".Rproj.user")
  valid_dirs <- all_dirs[!grepl(paste(exclude_dirs, collapse = "|"), all_dirs)]
  index_dirs <- valid_dirs[sapply(valid_dirs, function(d) file.exists(file.path(d, "index.qmd")))]
  index_dirs <- index_dirs[index_dirs != directory]
  qmd_files <- list.files(directory, pattern = "\\.qmd$", full.names = TRUE)
  non_index_qmd <- qmd_files[!grepl("index\\.qmd$", qmd_files)]
  qmd_no <- length(index_dirs)+ length(non_index_qmd)
  return(qmd_no)
}

