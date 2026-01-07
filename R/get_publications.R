#' Get publication references from ROAD database
#'
#' The \strong{\code{road_get_publications}} function retrieves publication 
#' references from the ROAD database. Entries are available in the \code{BibTex}
#' format or formatted according to bibliographic conventions, including books,
#' journal articles, book chapters, theses, and web pages.
#'
#' @param localities specifies the subset of localities for which publications 
#' should be retrieved. This parameter can be a string (a single locality name), 
#' a vector of strings (multiple locality names), or a data frame containing a 
#' \code{locality_id} column (e.g., the output of \code{road_get_localities()}). 
#' Defaults to \code{NULL}.
#' @param bibtex specifies if publication references should be formatted as \code{BibTeX}.
#' Defaults to \code{FALSE}.
#' 
#' @details With \code{localities = NULL} (the default), the function produces a 
#' complete list of all references compiled in ROAD, with each publication 
#' appearing only once. When \code{localities} is specified, duplicates may occur 
#' in the \code{Publication} column if a single source provides information for 
#' multiple sites.
#' 
#' All parameters are optional. If not used, omit them or set them to NULL.
#'
#' @return A data frame with two columns:
#' @return \code{Locality}: Is only returned, if the argument \code{localities}
#' is populated.
#' @return \code{Publication}: Items from ROAD's publication table, formatted as
#' reference or \code{BibTex}.
#'
#' @export
#'
#' @examples
#' if (curl::has_internet())
#' {
#' tryCatch({
#' result <- road_get_publications(localities = c("Apollo 11", "Berg Aukas"))
#' print(result)
#' }, error = function(e) {
#' message("Service unavailable.")
#' })
#' }
#' 
#' # Using result from road_get_localities
#' \donttest{
#'   locs <- road_get_localities(country = "Estonia")
#'   road_get_publications(localities = locs)
#' }
road_get_publications <- function (
   localities = NULL,
   bibtex = F
) 
{
  if (is.null(localities)) {
    query = "SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_source 
             WHERE (publication_source.id_source = edition.publication_source_id_source 
               and publication.edition_idedition = edition.idedition 
               and publication.edition_id_source = edition.publication_source_id_source)
             ORDER BY publication.author"
    data <- road_run_query(query)

    if (!bibtex) publications <- get_publications_as_text(data = data)
    else publications <- get_publications_as_bibtex(data = data)
    
    pV <- data.frame("Publication" = publications$Publication)
    return(pV)
  } # is.null(localities)
  
  publication_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(publication_df) <- c('Locality', 'Publication')
  
  # localities can be string, vector or data frame
  if ( is.string(localities))
  {
    publ <- get_publication_references(localities, bibtex = bibtex)
    publication_df <- rbind(publication_df, publ)
  }
  else
  {
    if (is.data.frame(localities)) 
       {
         locality_vector_tmp <- localities$locality_id
         locality_vector <- unique(locality_vector_tmp)
         for (locality in locality_vector) {
           publ <- get_publication_references(locality, bibtex = bibtex)
           publication_df <- rbind(publication_df, publ)
         }
       }
    else {
     # if localities is a vector
      locality_vector <- unique(localities)
      for (locality in locality_vector) {
        publ <- get_publication_references(locality, bibtex = bibtex)
        publication_df <- rbind(publication_df, publ)
      }
    }
  }

  return (publication_df)
}

#' Get formatted publication references for a single locality (internal helper function)
#'
#' Internal helper function that queries the ROAD database for all publications
#' associated with a single locality and formats them according to bibliographic standards
#' or as bibtex.
#' The function searches across multiple publication description tables including assemblages,
#' localities, human remains, paleofauna, geological layers, archaeological layers,
#' geostratigraphy, vegetation, and climate.
#'
#' @param locality A single locality identifier (string).
#' @param bibtex specifies if results - publication references, will be formatted as BibTeX.
#' Defaults to FALSE.
#'
#' @return A data frame with two columns:
#' \item{Locality}{The locality identifier}
#' \item{Publication}{The formatted publication reference}
#' 
#' @importFrom glue trim
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#'
#' @keywords internal
get_publication_references <- function (
    locality = NULL,
    bibtex = F
) 
{
  publications <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(publications) <- c('Locality', 'Publication')
  
  if (is.null(locality)) return(publications)
  
  query = paste0("SELECT * FROM (
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, publication_desc_assemblage.assemblage_idlocality AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_assemblage, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_assemblage.publication_idpublication = publication.idpublication and publication_desc_assemblage.publication_idedition = publication.edition_idedition and publication_desc_assemblage.publication_id_source = publication.edition_id_source and assemblage_idlocality = '", locality, "')
                        UNION
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, publication_desc_locality.locality_idlocality AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_locality, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_locality.publication_idpublication = publication.idpublication and publication_desc_locality.publication_idedition = publication.edition_idedition and publication_desc_locality.publication_id_source = publication.edition_id_source and locality_idlocality = '", locality, "')
                        UNION
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, publication_desc_humanremains.humanremains_idlocality AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_humanremains, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_humanremains.publication_idpublication = publication.idpublication and publication_desc_humanremains.publication_idedition = publication.edition_idedition and publication_desc_humanremains.publication_id_source = publication.edition_id_source and humanremains_idlocality = '", locality, "')
                        UNION
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, publication_desc_paleofauna.paleofauna_idlocality AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_paleofauna, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_paleofauna.publication_idpublication = publication.idpublication and publication_desc_paleofauna.publication_idedition = publication.edition_idedition and publication_desc_paleofauna.publication_id_source = publication.edition_id_source and paleofauna_idlocality = '", locality, "')
                        UNION
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, publication_desc_geolayer.geological_layer_idlocality AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_geolayer, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_geolayer.publication_idpublication = publication.idpublication and publication_desc_geolayer.publication_idedition = publication.edition_idedition and publication_desc_geolayer.publication_id_source = publication.edition_id_source and geological_layer_idlocality = '", locality, "')
                        UNION
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, publication_desc_archlayer.archaeologic_layer_idlocality AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_archlayer, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_archlayer.publication_idpublication = publication.idpublication and publication_desc_archlayer.publication_idedition = publication.edition_idedition and publication_desc_archlayer.publication_id_source = publication.edition_id_source and archaeologic_layer_idlocality = '", locality, "')
                        UNION
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, '", locality, "' AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_geostrat, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_geostrat.publication_idpublication = publication.idpublication and publication_desc_geostrat.publication_idedition = publication.edition_idedition and publication_desc_geostrat.publication_id_source = publication.edition_id_source and geostratigraphy_idgeostrat in (SELECT geostrat_idgeostrat FROM geostrat_desc_geolayer WHERE geolayer_idlocality = '", locality, "'))
                        UNION
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, '", locality, "' AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_vegetation, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_vegetation.publication_idpublication = publication.idpublication and publication_desc_vegetation.publication_idedition = publication.edition_idedition and publication_desc_vegetation.publication_id_source = publication.edition_id_source and vegetation_idvegetation in (SELECT idvegetation FROM vegetation WHERE plantremains_idlocality = '", locality, "'))
                        UNION
      SELECT DISTINCT edition.volume, edition.publication_year as year, publication_type, publication.title, publication.author, publication.pages, '", locality, "' AS idlocality, publication_source.title as source_title, publisher, publication_place, editor, publication.comments as comments, publication_source.comments as source_comments, url, access_date, doi FROM edition, publication, publication_desc_climate, publication_source WHERE (publication_source.id_source = edition.publication_source_id_source and publication.edition_idedition = edition.idedition and publication.edition_id_source = edition.publication_source_id_source and publication_desc_climate.publication_idpublication = publication.idpublication and publication_desc_climate.publication_idedition = publication.edition_idedition and publication_desc_climate.publication_id_source = publication.edition_id_source and climate_idclimate in (SELECT idclimate FROM climate WHERE assemblage_idlocality = '", locality, "'))
                        ) AS publications_all WHERE idlocality = '", locality, "' ORDER BY  idlocality, lower(author), year ASC")
  
  data <- road_run_query(query)
#return(data)  
  if (nrow(data) == 0)
  {
    publications[nrow(publications) + 1, ] <- c(locality, NA)
    return(publications)
  }
  
  # data is not empty
  if (!bibtex) publications <- get_publications_as_text(locality = locality, data = data)
  else publications <- get_publications_as_bibtex(locality = locality, data = data)
  
  return(publications)
}

# Get formatted publication references as text for a single locality (internal helper function)
#
# Internal helper function that formats all ROAD publications
# associated with a single locality and formats them as BibTeX entries. The function
# generates proper BibTeX entries for different publication types including books,
# book chapters (inbook), journal articles, theses (PhD, Master's, Bachelor's),
# and unpublished works.
#
# @param locality A single locality identifier (string).
# @param data a data frame with all publication data queried from ROAD database and
# associated with the locality.
#
# @return A data frame with two columns:
# \item{Locality}{The locality identifier}
# \item{Publication}{The as text formatted publication entry}
#' @keywords internal
get_publications_as_text <- function (
    locality = NULL,
    data = NULL
) 
{
  publications <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(publications) <- c('Locality', 'Publication')
  
  if (is.null(data)) return(publications)
  
  for (r in 1:nrow(data))   
  {
    publication <- ''
    author <- ' '
    title <- ' '
    year <- ' '
    source <- ''
    publisher <- ''
    publication_place <- ''
    editor <- ''
    url <- ''
    access_date <- ''
    publication_type <- ''
    comments <- ''
    source_comments <- ''
    isBook <- F
    isInBook <- F
    isPhdThesis <- F
    isMaThesis <- F
    isBaThesis <- F
    isJournalArticle <- F
    isWebPage <- F
    sco <- ''
    co <- ''
    
    if (!is.na(data[r,'publication_type'])) publication_type <- tolower(data[r,'publication_type'])
    if (!is.na(data[r,'comments'])) comments <- data[r,'comments']
    if (!is.na(data[r,'source_comments'])) source_comments <- data[r,'source_comments']
    
    sco <- tolower(substr(source_comments, 0, 4))
    co <- tolower(substr(comments, 0, 4))
    
    if (publication_type == 'book' || sco == 'book' || co == 'book' || tolower(trim(data[r,'title'])) == tolower(trim(data[r,'source_title'])))
      isBook <- T
    else 
      if (publication_type == 'inbook' || publication_type == 'book section' 
          || sco == 'inbo' || co == 'inbo')
        isInBook <- T
    else isJournalArticle <- T
    
    if (publication_type == 'phdthesis') isPhdThesis <- T
    if (publication_type == 'mathesis') isMaThesis <- T
    if (publication_type == 'bathesis') isBaThesis <- T
    if (publication_type == 'web page') isWebPage <- T
    
    # NNN
    title <- paste0(title, trim(data[r,'title']))
    if (substr_right(title, 1) != '.' && substr_right(title, 1) != '?' && substr_right(title, 1) != '!')
    {
      title <- paste0(" ", title, ".")
    }
    
    if (!is.na(data[r,'editor'])) editor <- trim(data[r,'editor'])
    if (!is.na(data[r,'year']) && data[r,'year'] != 0) year <- paste0(' ', data[r,'year'], '.')
    else year <- ' (n.d.)'
    
    author <- trim(data[r,'author'])
    if (!is.na(data[r,'publisher'])) publisher <- trim(data[r,'publisher'])
    if (!is.na(data[r,'publication_place'])) publication_place <- trim(data[r,'publication_place'])
    
    if (!is.na(data[r,'url'])) url <- data[r,'url']
    if (!is.na(data[r,'access_date'])) access_date <- data[r,'access_date']
    
    if (isWebPage)
    {
      if (publisher != '')
      {
        if (publication_place != '') source <- paste0(publisher, ', ', publication_place)
        else source <- publisher
      }
      if (url != '' && trim(source) != '') source <- paste0(source, '. ', url)
      else if (url != '') source <- url
      if (trim(source) != '' && access_date != '') source <- paste0(source, ' (accessed ', access_date, ').' )
      
      publication <- paste0(author, year, title, source)
      
    }
    else
    {
      if (editor != '' && isInBook)
      {
        editorTmp <- str_replace_all(editor, c(" and " = ", ", ".and " = ", ", ",and " = ", ", ", ," = ", "))
        if (grepl('.,', editorTmp, fixed = TRUE)) source <- paste0(source, ' In: ', editorTmp, ' (Eds.). ')
        else source <- paste0(source, ' In: ', editorTmp, ' (Ed.). ') 
        source <- paste0(source, trim(data[r,'source_title']))
      }
      else source <- paste0(source, trim(data[r,'source_title']))
      
      if (isPhdThesis || isMaThesis || isBaThesis) 
        if (substr_right(source, 1) != "." && substr_right(source, 1) != "?" && substr_right(source, 1) != "!" && trim(source) != "")
          source <- paste0(source, '.')
      if (isPhdThesis) source <- paste0(source, ' Doctoral Thesis')
      if (isMaThesis) source <- paste0(source, " Master's Thesis")
      if (isBaThesis) source <- paste0(source, " Bachelor's Thesis")
      
      if (publisher != '' && (isBook || isInBook || isPhdThesis || isMaThesis || isBaThesis))
      {
        if (trim(source) != '')
        {
          if (publication_place != '') source <- paste0(source, ', ', publisher, ', ', publication_place)
          else source <- paste0(source, ', ', publisher)
        }
        else
        {
          if (publication_place != '') source <- paste0(publisher, ', ', publication_place)
          else source <- publisher
        }
      }
      if (!is.na(data[r,'volume']) && !(isBook || isInBook || isPhdThesis || isMaThesis || isBaThesis))
        source <- paste0(trim(source), ' ', data[r,'volume'])
      
      if (!is.na(data[r,'pages']) && !(isBook || isPhdThesis || isMaThesis || isBaThesis))
      {
        if (isInBook) source <- paste0(source, ', pp. ', data[r,'pages'], '.')
        else source <- paste0(source, ', ', data[r,'pages'], '.')
      }
      
      if (substr_right(source, 1) != "." && trim(source) != "")
        source <- paste0(source, '.')
      
      source <- paste0(" ", source)
      
      authorTmp <- str_replace_all(author, c(" and " = ", ", ".and " = ", ", ",and " = ", "))
      
      if (isBook) publication <- paste0(authorTmp, ', ', year, source)
      else publication <- paste0(authorTmp, ', ', year, title, source)
    }
    
    if (is.null(locality)) publications[nrow(publications) + 1, ] <- c('-', publication) 
    else publications[nrow(publications) + 1, ] <- c(locality, publication)
    
    
  } #for (r in 1:nrow(data))
  
  return(publications)
}

# Get BibTeX formatted publication references for a single locality (internal helper function)
#
# Internal helper function that formats all ROAD publications
# associated with a single locality and formats them as BibTeX entries. The function
# generates proper BibTeX entries for different publication types including books,
# book chapters (inbook), journal articles, theses (PhD, Master's, Bachelor's),
# and unpublished works.
#
# @param locality A single locality identifier (string).
# @param data a data frame with all publication data queried from ROAD database and
# associated with the locality.
#
# @return A data frame with two columns:
# \item{Locality}{The locality identifier}
# \item{Publication}{The BibTeX formatted publication entry}
#
# @details The function automatically determines the correct BibTeX entry type
# (\code{@@book}, \code{@@inbook}, \code{@@article}, \code{@@phdthesis}, 
# \code{@@mastersthesis}, \code{@@bathesis}, \code{@@unpublished})
# based on publication metadata and includes all relevant fields such as author, title,
# journal/booktitle, publisher, year, pages, volume, DOI, and editor.
#
#' @keywords internal
get_publications_as_bibtex <- function (
    locality = NULL,
    data = NULL
) 
{ 
  publications <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(publications) <- c("Locality", "Publication")
  
  if (is.null(data)) return(publications)
  
  for (i in seq_len(nrow(data)))
  {
    row <- data[i, ]

    publication_type <- ""
    if (!is.na(row["publication_type"])) publication_type <- tolower(row["publication_type"])
    comments <- ""
    if (!is.na(row["comments"])) comments <- tolower(row["comments"])
    source_comments <- ""
    if (!is.na(row["source_comments"])) source_comments <- tolower(row["source_comments"])

    address <- NA # book, inbook
    if (!is.na(row["publication_place"])) address <- trimws(row["publication_place"])
    author <- NA
    if (!is.na(row["author"])) author <- trimws(row["author"])
    booktitle <- NA # inbook
    if (!is.na(row["source_title"])) booktitle <- sub("\\.$", "", trimws(row["source_title"]))
    doi <- NA
    if (!is.na(row["doi"])) doi <- trimws(row["doi"])
    editor <- NA # inbook
    if (!is.na(row["editor"])) editor <- trimws(row["editor"])
    journal <- NA # article
    if (!is.na(row["source_title"])) journal <- sub("\\.$", "", trimws(row["source_title"]))
    pages <- NA
    if (!is.na(row["pages"])) pages <- trimws(row["pages"])
    publisher <- NA # book, inbook
    if (!is.na(row["publisher"])) publisher <- trimws(row["publisher"])
    title <- NA
    if (!is.na(row["title"])) title <- sub("\\.$", "", trimws(row["title"]))
    volume <- NA # article, inbook
    if (!is.na(row["volume"])) volume <- trimws(row["volume"])
    year <- NA
    if (!is.na(row["year"]) && row["year"] != 0) year <- trimws(as.character(row["year"]))

    # Book
    if (
      publication_type == "book" || substr(comments, 0, 4) == "book" || substr(source_comments, 0, 4) == "book"
      || (!is.na(title) && tolower(title) == tolower(booktitle))
    ) {
      entry_type <- "book"
      type <- "Book"
    }

    # In book
    else if (publication_type == "inbook" || publication_type == "book section" || substr(comments, 0, 4) == "inbo" || substr(source_comments, 0, 4) == "inbo")
    {
      entry_type <- "inbook"
      type <- "Book Section"
    }

    # Journal article
    else if (publication_type == "journal article" || publication_type == "article")
    {
      entry_type <- "article"
      type <- "Journal Article"
    }

    else if (publication_type == "phdthesis")
    {
      entry_type <- "phdthesis"
      type <- "Thesis"
    }

    else if (publication_type == "mathesis")
    {
      entry_type <- "mastersthesis"
      type <- "Thesis"
    }

    else if (publication_type == "bathesis")
    {
      entry_type <- "bathesis"
      type <- "Thesis"
    }

    else if (publication_type == "unpublished work")
    {
      entry_type <- "unpublished"
      type <- "Unpublished Work"
    }

    else
    {
      entry_type <- "article"
      type <- "Journal Article"
    }

    struct_address <- ""
    if (!is.na(address)) struct_address <- paste0("address = {", address, "},")
    struct_author <- ""
    if (!is.na(author)) struct_author <- paste0("author = {", author, "},")
    struct_booktitle <- ""
    if (!is.na(booktitle)) struct_booktitle <- paste0("booktitle = {", booktitle, "},")
    struct_doi <- ""
    if (!is.na(doi)) struct_doi <- paste0("DOI = {", doi, "},")
    struct_editor <- ""
    if (!is.na(editor)) struct_editor <- paste0("editor = {", editor, "},")
    struct_journal <- ""
    if (!is.na(journal)) struct_journal <- paste0("journal = {", journal, "},")
    struct_pages <- ""
    if (!is.na(pages)) struct_pages <- paste0("pages = {", pages, "},")
    struct_publisher <- ""
    if (!is.na(publisher)) struct_publisher <- paste0("publisher = {", publisher, "},")
    struct_title <- ""
    if (!is.na(title)) struct_title <- paste0("title = {", sub("\\.$", "", title), "},")
    struct_volume <- ""
    if (!is.na(volume)) struct_volume <- paste0("volume = {", volume, "},")
    struct_year <- ""
    if (!is.na(year)) struct_year <- paste0("year = {", year, "},")

    struct_source <- ""
    if (entry_type == "inbook") struct_source = struct_booktitle
    else if (entry_type == "article") struct_source = struct_journal

    publication <- paste0(
      "@", entry_type, "{",
        struct_author,
        struct_title,
        struct_source,
        struct_pages,
        struct_year,
        struct_publisher,
        struct_address,
        struct_editor,
        struct_volume,
        struct_doi,
        "type = {", type, "}",
      "}"
    )

    if (is.null(locality)) publications[nrow(publications) + 1, ] <- c('-', publication) 
    else publications[nrow(publications) + 1, ] <- c(locality, publication)
    
  }

  return(publications)
}