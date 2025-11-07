road_get_publications <- function (
   localities = NULL
) 
{
  publication_df <- data.frame(matrix(ncol = 2, nrow = 1))
  colnames(publication_df) <- c('Locality', 'Publication')
  
  # localities can be string, vector or data frame
  # if localities is a vector
  for (locality in localities) {
    publ <- get_publication_reference(locality)
    new_row <- c(locality, publ)
    publication_df <- rbind(publication_df[1,], new_row)
  }
  return (publication_df)
}

get_publication_reference <- function (
    locality = NULL
) 
{      
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
    
    publications <- ''

    for (r in 1:nrow(data))   
     # for (c in 1:ncol(data))
     # for (c in colnames(data))
      # print(paste("Row", r, "and column",c, "have values of", data[r,c]))
    {
      publication <- ''
      author <- ''
      title <- ''
      year <- ''
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

      if (!is.na(data[r,'publication_type'])) publication_type <- tolower(data[r,'publication_type'])
      if (!is.na(data[r,'comments'])) comments <- data[r,'comments']
      if (!is.na(data[r,'source_comments'])) source_comments <- data[r,'source_comments']
      
      sco <- tolower(substr(source_comments, 0, 4))
      co <- tolower(substr(comments, 0, 4))
      
      if (publication_type == 'book' || sco == 'book' || co == 'book' 
          || tolower(trim(data[r,'title'])) == tolower(trim(data[r,'source_title'])))
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
      
      title <- paste0(' ', trim(data[r,'title']))
      
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
          editorTmp <- editor
          source <- paste0(source, ' In: ', editorTmp, ' (Eds.). ')
        }
        source <- paste0(source, trim(data[r,'source_title']))
        if (isPhdThesis || isMaThesis || isBaThesis) source <- paste0(source, '.')
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
        
        authorTmp <- author
        publication <- paste0(authorTmp, ', ', year, title, source)
      }
      
      print(paste("Row", r, "and", title))
      publications <- paste(publications, publication, sep = "\r")
    } #for (r in 1:nrow(data))
    
    # cat(publications)
    return(publications)
    #return(data)
}