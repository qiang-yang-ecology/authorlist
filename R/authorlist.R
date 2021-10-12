#' @title toAuthorlist
#' @description main function to produce the formatted author list and affiliation list in Word
#' @param source.csv the path to the CSV document including the coauthor information, Default: 'authorlist.csv'
#' @param target the path where you would like to save the produced Word document, Default: 'authorlist.docx'
#' @param type the type of the affiliation numbering, could be "numbered" or "alphabeta", Default: 'numbered'. If 'numbered', the affiliation will be marked with numbers, otherwise the affiliation will be marked with letters as PNAS papers.
#' @return the produced Word document
#' @details This is the main function to produce the formatted author list and affiliation list in Word.
#' @examples
#' toAuthorlist(source.csv = "~/Downloads/authorlist.csv",
#'            target = "~/Downloads/authorlist.docx",
#'            type="numbered")
#' @rdname toAuthorlist
#' @export
toAuthorlist <- function(source.csv = "authorlist.csv",
                         target = "authorlist.docx",
                         type="numbered"){
  authorlist <- read.csv(source.csv)
  authorlist %<>% as_tibble()
  df_authorlist <- authorlist %>%
    # gather all affiliations in one column
    gather(key = "key", value = "Affiliation", -Coauthor_Order, -`Coauthor_Name`) %>%
    # remove rows with no Affiliations
    filter(!is.na(Affiliation)) %>%
    filter(Affiliation != "") %>%
    arrange(Coauthor_Order, key)
  df_Affiliation_order <- df_authorlist %>%
    select(Affiliation) %>%
    distinct()
  df_Affiliation_order %<>%
    mutate(Affiliation_Order = 1:nrow(df_Affiliation_order))
  if(type == "numbered"){
    df_authorlist %<>% left_join(df_Affiliation_order)

    df_authorlist %<>%
      arrange(Coauthor_Order, Affiliation_Order) %>%
      group_by(Coauthor_Name, Coauthor_Order) %>%
      nest() %>%
      mutate(data = map(data,
                        function(dat) dat %>% pull(Affiliation_Order))) %>%
      mutate(
        Affiliation_numbers = map_chr(data,
                                      function(dat) paste0(dat, collapse = ","))) %>%
      select(-data)

    df_Affiliation_order %<>% arrange(Affiliation_Order)
    df_authorlist %<>% arrange(Coauthor_Order)

    properties1 <- fp_text(font.size=11, font.family = "Calibri")
    properties2 <- fp_text(font.size=11, font.family = "Calibri", vertical.align = "superscript")

    authorlist <- list()
    n_author <- n_distinct(df_authorlist$Coauthor_Name)
    for(i in 1:n_author){
      authorlist[[i*3-2]] <- ftext(df_authorlist$Coauthor_Name[[i]], properties1)
      authorlist[[i*3-1]] <- ftext(df_authorlist$Affiliation_numbers[[i]], properties2)
      authorlist[[i*3]] <- ftext(", ", properties1)
    }
    authorlist[[n_author*3]] <- ""
    authorlist.paragraph <- fpar(values = authorlist)

    affiliations <- list()
    n_affi <- n_distinct(df_Affiliation_order$Affiliation)
    for(i in 1:n_affi){
      v1 <- ftext(df_Affiliation_order$Affiliation_Order[[i]], properties2)
      v2 <- ftext(df_Affiliation_order$Affiliation[[i]], properties1)
      affiliations[[i]] <- fpar(v1, v2)
    }

    x <- read_docx()
    x <- body_add(x, authorlist.paragraph)
    x <- body_add(x, fpar(""))
    for(i in 1:n_affi){
      x <- body_add(x, affiliations[[i]])
    }
    print(x, target = target)
  }else{
    df_Affiliation_order %<>%
      mutate(Affiliation_Order_2 = map_chr(Affiliation_Order, function(i){
        letter_ <- letters[i%%26]
        n_letter_rep <- i%/%26+1
        output <- rep(letter_, n_letter_rep) %>% paste0(collapse = "")
        return(output)
      }))

    df_authorlist %<>% left_join(df_Affiliation_order)

    df_authorlist %<>%
      arrange(Coauthor_Order, Affiliation_Order) %>%
      group_by(Coauthor_Name, Coauthor_Order) %>%
      nest() %>%
      mutate(data = map(data,
                        function(dat) dat %>% pull(Affiliation_Order_2))) %>%
      mutate(
        Affiliation_numbers = map_chr(data,
                                      function(dat) paste0(dat, collapse = ","))) %>%
      select(-data)

    df_Affiliation_order %<>% arrange(Affiliation_Order)
    df_authorlist %<>% arrange(Coauthor_Order)

    properties1 <- fp_text(font.size=11, font.family = "Calibri")
    properties2 <- fp_text(font.size=11, font.family = "Calibri", vertical.align = "superscript")

    authorlist <- list()
    n_author <- n_distinct(df_authorlist$Coauthor_Name)
    for(i in 1:n_author){
      authorlist[[i*3-2]] <- ftext(df_authorlist$Coauthor_Name[[i]], properties1)
      authorlist[[i*3-1]] <- ftext(df_authorlist$Affiliation_numbers[[i]], properties2)
      authorlist[[i*3]] <- ftext(", ", properties1)
    }
    authorlist[[n_author*3]] <- ""
    authorlist.paragraph <- fpar(values = authorlist)

    affiliations <- list()
    n_affi <- n_distinct(df_Affiliation_order$Affiliation)
    for(i in 1:n_affi){
      v1 <- ftext(df_Affiliation_order$Affiliation_Order_2[[i]], properties2)
      v2 <- ftext(df_Affiliation_order$Affiliation[[i]], properties1)
      affiliations[[i]] <- fpar(v1, v2)
    }

    x <- read_docx()
    x <- body_add(x, authorlist.paragraph)
    x <- body_add(x, fpar(""))
    for(i in 1:n_affi){
      x <- body_add(x, affiliations[[i]])
    }
    print(x, target = target)
  }
}


