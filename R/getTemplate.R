#' @title getTemplate
#' @description download the CSV template
#' @param destfile the path to save the CSV template, Default: 'authorlist.template.csv'
#' @return the CSV template
#' @details This function downloads the CSV template to your computer
#' @examples
#' getTemplate(destfile = "~/Downloads/authorlist.template.csv")
#' @rdname getTemplate
#' @export
getTemplate <- function(destfile = "authorlist.template.csv"){
  download.file("https://github.com/qiang-yang-ecology/raw.authorlist/blob/main/authorlist.csv",
                destfile =  destfile)
}

# example of how to use the function
# getTemplate(destfile = "~/Downloads/authorlist.template.csv")
