
netr # creates net
neti # prunes isolates, cutting vertices
nets # reciprocates it, proliferating edges

That order only

All functions work as intended
# symmetrize: limit to two-way users
Q_chk)

nets <- function(x) {
  x <- symmetrize(x , rule = "strong", return.as.edgelist=TRUE)
  x <- network(x, matrix.type = "edgelist")
}


Qs1 <- nets(Q_raw)
Qs1
> Qs1
 Network attributes:
  vertices = 2113 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 8691 
    missing edges= 0 
    non-missing edges= 8691 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 
Qs2 <- nets(Q_iso)
> Qs2 <- nets(Q_iso)
> Qs2
 Network attributes:
  vertices = 1107 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 8688 
    missing edges= 0 
    non-missing edges= 8688 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 
> 

x <- Q_raw
x <- symmetrize(x , rule = "strong", return.as.edgelist=TRUE)
x <- network(x, matrix.type = "edgelist")
> x
 Network attributes:
  vertices = 2113 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 8691 
    missing edges= 0 
    non-missing edges= 8691 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 

> x <- Qs2
> x <- symmetrize(x , rule = "strong", return.as.edgelist=TRUE)
> x <- network(x, matrix.type = "edgelist")
> x
 Network attributes:
  vertices = 1107 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 16134 
    missing edges= 0 
    non-missing edges= 16134 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 


# create graph object function is good

netr <- function(x) {
	net <- network(x, matrix.type = "edgelist")
}

> Q_raw <- netr(Qall_raw)
> Q_raw
 Network attributes:
  vertices = 2113 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 5690 
    missing edges= 0 
    non-missing edges= 5690 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 

# remove isolates is good

Q_chk <- network(Qall_raw, matrix.type="edgelist")
Q_chk
 Network attributes:
  vertices = 2113 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 5690 
    missing edges= 0 
    non-missing edges= 5690 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 

# remove isolates: remove disconnected users 

neti <- function(x) {
  delete.vertices(x, isolates(x))
}

> Q_iso <- neti(Q_chk)
> Q_iso
 Network attributes:
  vertices = 1107 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 5687 
    missing edges= 0 
    non-missing edges= 5687 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 

>  Q_iso <- delete.vertices(Q_chk, isolates(Q_chk))
>  Q_iso
 Network attributes:
  vertices = 1107 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 5687 
    missing edges= 0 
    non-missing edges= 5687 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 
> 

Order matters

> x <- Qall_raw %>% netr(.) %>% nets(.) %>% neti(.)
> x
 Network attributes:
  vertices = 533 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 8686 
    missing edges= 0 
    non-missing edges= 8686 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 
> 

x <- Qall_raw %>% netr(.) %>% neti(.) %>% nets(.)

> x
 Network attributes:
  vertices = 1107 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 8688 
    missing edges= 0 
    non-missing edges= 8688 

 Vertex attribute names: 
    vertex.names 

 Edge attribute names not shown 
