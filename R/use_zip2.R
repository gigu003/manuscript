#' Compress files
#'
#' @param paths Paths of the files to be compressed
#' @param zipfile Name of the output file
#' @param move_to Location of the compressed file
#'
#'
#' @return Compressed file
#' @export
#'
use_zip2 <- function(paths, zipfile, move_to = "outputs") {
  # 检查每个文件夹路径是否存在
  if (!all(fs::dir_exists(paths))) {
    stop("One or more specified folders do not exist.")
  }

  # 如果输出文件夹不存在，创建该文件夹
  if (!fs::dir_exists(move_to)) {
    fs::dir_create(move_to)
  }

  # 获取所有文件的完整路径
  files <- list.files(paths, full.names = TRUE, recursive = TRUE)

  # 压缩文件到 zipfile
  utils::zip(zipfile, files)

  # 构建目标路径，将压缩包移动到指定目录
  new_path <- fs::path(move_to, zipfile)
  if (file.rename(zipfile, new_path)) {
    message("Folders successfully compressed into: ", new_path)
  } else {
    stop("Failed to move the ZIP file to the target directory.")
  }
}
