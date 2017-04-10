#setwd("C:/projects/graph/demo")

# get list of staff ids
  # read in raw html
  x <- readLines("https://www.sei-international.org/staff")
  x <- paste(x, collapse = ' ')
  x <-  strsplit(x,'>')[[1]]

  s <- grep('staffid', x, v = T)

# Extract employee names and ids 
  emps <- c()
  id <- c()

  for( a in s ){
      
      # Remove side panel  
      b <-strsplit(a, '<p.*?>')[[1]]
      ids <- gsub('.*?staffid=([0-9]{1,3})["].*', '\\1', b)
      n <- gsub('.*?>(.*?)<.*', '\\1', b)
      
      emps <- c(emps, n)
      id <- c(id, ids)
  
  }

  emps <- emps[-grep('\\t', emps)]
  id <- id[ -grep('\\t', id)]

# Extract publication list 
  E <- list()

  for (i in 1:length(id)){

    p <- emps[i]
    idz <- id[i]
    E[[p]] <- list(name = p, id = idz, 
    urlPage = paste0("https://www.sei-international.org/staff?staffid=", idz),
    urlPapers = paste0('https://www.sei-international.org/publications?author=', idz)
    )
  
  }

  # some helper functions

  getPapers <- function(eid){
    urlPapers = paste0('https://www.sei-international.org/publications?author=', eid)
    b <- readLines(urlPapers)
    b <- paste(b, collapse = ' ')

    #discard 'recent publications' sidebar
    e <- strsplit(b, 'sidebar')[[1]][1]
    if( length(grep('pid', e)) == 0) return (NULL)
    f <- strsplit(e, 'pid=')[[1]]
    pids <- gsub('(^[0-9].*?)["].*', '\\1', f[-1])
    pnames <- gsub('.*?>(.*?)<.*','\\1', f[-1])
    return(data.frame( pids = as.numeric(pids), pnames = as.character(pnames)))
  }

  getCenter <- function(eid){
    # get the center an employee works at
    urlPage = paste0("https://www.sei-international.org/staff?staffid=", eid)
    b <- readLines(urlPage)
    b <- paste(b, collapse = ' ')
    e <- gsub('.*?Centre:</strong> <a href=.*?>', '',b)
    e <- gsub('<.*', '', e)
    return(e)
  }


  for (e in names(E)){
    print(E[[e]]$name)
    E[[e]]$papers <- getPapers(E[[e]]$id)
  }

  for (e in names(E)){
    print(E[[e]]$name)
    E[[e]]$center <- getCenter(E[[e]]$id)
  }


# store data in a graph database
  library(RNeo4j)
  graph = startGraph("http://localhost:7474/db/data/")

  people = list()
  papers = list()
  rels  = list()

  for( person in E){
      cat(person$name, '\n');flush.console()
      
      # create a new person if (s)he doesn't exist
      if (is.null(people[[person$name]])) {      
          p <- createNode(graph, "Person", name = person$name, center = person$center, npubs = nrow(person$papers))
          people[[person$name]] <- p
      } else {
      
        p <-people[[person$name]]
      
      }
      if (is.null(person$papers)) next() 
      
      # create papers and add relationships between employees and papers
      for (pp in 1:nrow(person$papers)){
          paper <- person$papers[pp,]
          
          if (as.character(paper$pids) %in% names(papers)){
              pper <- papers[[as.character(paper$pids)]]
          } else {
              pper <- createNode(graph, "Paper", pid = as.character(paper$pids), name= as.character(paper$pname))
              papers[[as.character(paper$pids)]] <- pper
          }
          
          createRel(p, 'AUTHOR', pper)        
          
      }
  }

  # quick way to deal with special characters
  trans <- c(
  "Ã¡" = "á",
  "Ã©" = "é",
  "Ã…" = "Å",
  "Ã¶" = "ö",
  "Ã–"="Ö",
  'Ã¼' = "ü",
  "Å¸" = "ß",
  "Ã²" = "ò",
  "Ãµ" = "õ",
  "Å¡" = "š",
  "Ã¸"  = "ø", 
  "Ã¥" = "å",
  "Ã¤" = "ä",
   "Ã³"= "ó")

 #############

  # Create visualizations
 
  library(visNetwork)
  library(igraph)

  # Extract number of connections via papers
  query = "
  MATCH (p1:Person)-[:AUTHOR]->(:Paper)<-[:AUTHOR]-(p2:Person)
  WHERE p1.name < p2.name
  RETURN p1.name AS from, p2.name AS to, COUNT(*) AS weight
  "
  
  # edges are relationships, nodes are people
  edges = cypher(graph, query)
  nodes = data.frame(id=unique(c(edges$from, edges$to)))
  nodes$label = nodes$id
  
  #try to fix special characters
  for(j in names(trans)){
    nodes$label <- gsub(j, trans[j], nodes$label)
  }


  # merge with information on centers
  q2 = " MATCH (p1:Person) RETURN p1.name AS name, p1.center as center"
  centers = cypher(graph,q2)

  nodes$group <- centers$center[match(nodes$id, centers$name)]
  nodes$title <- paste0(nodes$label, '\n', nodes$group)

  #resize by pubs
  q3 = "
  MATCH (p1:Person)-[]->(p2:Paper)
  RETURN p1.name as label, COUNT(*) as Pubs
  "
  pubs = cypher(graph, q3)

  nodes$value <-pubs$Pubs[match(nodes$id, pubs$label)]

  # output visualization
  network <- visNetwork(nodes, edges) %>%
   visPhysics(solver = "forceAtlas2Based", 
              forceAtlas2Based = list(gravitationalConstant = -10), stabilization = TRUE) %>%
              visLegend() %>%
              visOptions(selectedBy = "group", 
               highlightNearest = TRUE, 
               nodesIdSelection = TRUE) 
               
  visSave(network, file = "sei_network.html")            
