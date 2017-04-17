  
# This script scrapes information about SEI employees, including their
# projects, papers, and the center they work at. 
  
  #setwd("C:/projects/graph/graph_demo")

  library(RNeo4j)
  library(visNetwork)
  library(igraph)
  
  reset_graph = TRUE # clear nodes and relationships before inputing data?  
  

# get list of staff ids
  # read in raw html
  #setInternet2(TRUE)
  h <- readLines("https://www.sei-international.org/staff")
  x <- paste(h, collapse = ' ')
  x <-  strsplit(x,'</p>')[[1]]

  s <- grep('staffid', x, v = T)
  z <- grep('jpg',s) # remove items with pictures
  if(length(z) > 0) s <- s[-z]
  
# Extract employee names and ids 
  emps <- c()
  ids <- c()

  for( a in s ){
      
      # Extracting desired text with regex  
      id <- gsub('.*?staffid=([0-9]{1,3})["].*', '\\1', a)
      name <- gsub('.*?<a .*?>(.*?)<.*', '\\1', a)

      emps <- c(emps, name)
      ids <- c(ids, id)
  
  }

  # Try to deal with special characters
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

  #try to fix special characters
  for(j in names(trans)){
    emps <- gsub(j, trans[j], emps)
  }   

# Extract publication list
# This process could be optimized of course...
 
  E <- list()

  for (i in 1:length(ids)){

    p <- emps[i]
    idz <- ids[i]
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

  getProjects <- function(eid){
    urlPage = paste0("https://www.sei-international.org/staff?staffid=", eid)
    b <- readLines(urlPage)
    b <- paste(b, collapse = ' ')
    k <-  strsplit(b,'</p>')[[1]]
    s <- grep('prid=', k, value = TRUE)
    pr_ids <- sapply(s, function(x) gsub('.*?prid=([0-9]{1,5}).*', '\\1', x))
    names(pr_ids) <- sapply(s, function(x) gsub('.*?projects[?].*?>(.*?)</a>.*', '\\1', x))
    return(pr_ids)
  
  }

  for (e in names(E)){
    print(E[[e]]$name)
    E[[e]]$papers <- getPapers(E[[e]]$id)
  }

  for (e in names(E)){
    print(E[[e]]$name)
    E[[e]]$center <- getCenter(E[[e]]$id)
  }
  
  for (e in names(E)){
    print(E[[e]]$name)
    E[[e]]$projects <- getProjects(E[[e]]$id)
  }

  # worth saving this information
  save(E, file = 'stored_data.RData')

# store data in a graph database
  
  #note: must have Neo4J running in background
  graph = startGraph("http://localhost:7474/db/data/")

  if(reset_graph){
  
    query = "MATCH (n) DETACH DELETE n"
    cypher(graph,query)
  }
  
  
  people = list()
  papers = list()
  projects = list()
  rels  = list()

  # Right. Creating a big ugly loop ... not the most elegant but easy to debug!
  for( person in E){
      cat(person$name, '\n');flush.console()
      
      # create a new person if (s)he doesn't exist
      if (is.null(people[[person$name]])) {      
          p <- createNode(graph, "Person", name = person$name, center = person$center, npubs = nrow(person$papers))
          people[[person$name]] <- p
      } else {
      
        p <-people[[person$name]]
      
      }
      if (!is.null(person$papers)){ 
      
          # create papers and add relationships between employees and papers
          for (pp in 1:nrow(person$papers)){
              paper <- person$papers[pp,]
              
              if (as.character(paper$pids) %in% names(papers)){
                  pper <- papers[[as.character(paper$pids)]]
              } else {
                  pper <- createNode(graph, "Paper", pid = as.character(paper$pids), name= as.character(paper$pname))
                  papers[[as.character(paper$pids)]] <- pper
              }
              
              r <- paste0(person$name,'_AUTHOR_',paper$pids)
              if (r %in% names(rels)) next()
              rels[[r]] <- createRel(p, 'AUTHOR', pper)        
              
          }
      }
      
      if (length(person$projects) > 0){
          # create projects and add relationships between employees and projects
          for (px in 1:length(person$projects)){
              project <- person$projects[px]
              
              if (as.character(project) %in% names(projects)){
                  prjct <- projects[[as.character(project)]]
              } else {
                  prjct <- createNode(graph, "Project", prid = as.character(project), name= names(person$projects[px]))
                  projects[[as.character(project)]] <- prjct
              }
              
              r <- paste0(person$name,'_AUTHOR_',project)
              if(r %in% names(rels)) next()
              rels[[r]] <- createRel(p, 'AUTHOR', prjct)        
              
              
          }
      }
    }  
      
  

  
 #############

  # Create visualizations
 
  
  # Extract number of connections via papers or projects
  query = "
  MATCH (p1:Person)-[:AUTHOR]->()<-[:AUTHOR]-(p2:Person)
  WHERE p1.name < p2.name
  RETURN p1.name AS from, p2.name AS to, COUNT(*) AS weight
  "
  
  # edges are relationships, nodes are people
  edges = cypher(graph, query)
  nodes = data.frame(id=unique(c(edges$from, edges$to)))
  nodes$label = nodes$id
  
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
  edges <- edges[order(edges$from),]
  nodes <- nodes[order(nodes$id),]
  network <- visNetwork(nodes, edges, height = "700px", width = "100%") %>%
   visPhysics(solver = "forceAtlas2Based", 
              forceAtlas2Based = list(gravitationalConstant = -10), stabilization = FALSE) %>%
              visLegend() %>%
              visOptions(selectedBy = "group", 
               highlightNearest = TRUE, 
               nodesIdSelection = TRUE) 
  
  
  visSave(network, file = "sei_network.html")            

  # who is the most connected person?
  library(igraph)
  net <- graph.data.frame(edges, directed =F) 
  which.max(degree(net))
  labs <- 0:36
  labs[which(0:35 %% 5 != 0)] <- NA
  png('hist.png')
  barplot(degree.distribution(net), names =labs, xlab = 'node degree', ylab = 'frequency', border = 'white')
  dev.off()