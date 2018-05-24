#' @title read tables from wikipedia (not actually specific to wiki)
#'
#' @param url the url to read
#' @param index the index of the table among the pages tables
#'
#' @return a tbl read from the specified table
wiki_table_read = function(url, index){
  
  html.to_read = read_html(url)
  
  html.to_read %>% 
    html_nodes("table") %>% 
    .[[index]] %>% 
    html_table(header = TRUE) %>% 
    as.tbl %>% 
    setNames(., gsub(" ", "_", tolower(names(.))))
}


#' @title fuzzy matching between two candidate sets
#' 
#' @description The idea is that we're matching between the two complements of the sets,
#' we expect that the values not in the intersection have a corresponding value in the other complement.
#'
#' @param to_match The values to match left values not intersected
#' @param unmatched The values to match to right values not intersected
#' @param method The stringdist method
#'
#' @return a data frame with two columns for the values to be matched and their closest match in the unmatched data.
#'
#' @examples
match_to_candidates = function(to_match, unmatched, method = 'jw'){
  
  mat.dist = stringdistmatrix(a = to_match,
                              b = unmatched,
                              method = method)
  
  rownames(mat.dist) = to_match
  colnames(mat.dist) = unmatched
  
  df.match_dist = mat.dist %>% 
    as.data.frame() %>% 
    mutate(to_match = rownames(.)) %>% 
    gather(unmatched, distance, -to_match)
  
  df.matched = data_frame()
  
  while(dim(df.match_dist)[1] > 0){
    
    df.new_matches = df.match_dist %>%
      group_by(to_match) %>% 
      filter(distance == min(distance)) %>% 
      group_by(unmatched) %>%
      filter(distance == min(distance))
    
    df.matched = bind_rows(df.matched,
                           df.new_matches)
    
    df.match_dist = df.match_dist %>% 
      filter(!(to_match %in% df.new_matches$to_match)) %>%
      filter(!(unmatched %in% df.new_matches$unmatched))
    
  }
  
  return(df.matched)
}
