#!/bin/env Rscript

main <- function() { # !/usr/bin/env Rscript
    # get commandline arguments
    args <- commandArgs(trailingOnly = TRUE)

    # get filename from arguments if it exists
    if (length(args) == 0) { # default filename
        cat("Using default graph file 'matrix.txt'\n")
        filename <- "matrix.txt"
    } else if (length(args) > 1) {
        cat("Only one argument required.\n")
        cat("Superfluous positional arguments ignored.\n")
        filename <- args[1]
    } else {
        filename <- args[1]
    }

    # load graph
    graph <- load_graph(filename);

    # remove extention
    gv_filename <- sub("\\..*$", "", filename)

    # draw the graph
    graph_dot(graph, 1, paste0(gv_filename, "_graph.gv"));

    # set starting vertex as 1
    start_vertex <- 1

    # run bellman_ford
    ret <- bellman_ford(graph, start_vertex);

    # get paths
    paths <- paths_from_predecessor(ret$predecessor, start_vertex)
    for (i in seq(paths)) {
        target <- rev(paths[[i]])[1]
        # draw graph with path highlighted
        graph_path_dot(graph, paths[i], filename = paste0(
            gv_filename, "_graph_",
            start_vertex, "_to_", target,
            ".gv"
        ))
    }

    # get all-to-all paths
    # \/ NOT RUN \/
    while (0) {
    all_paths <- c()
    for (start_vertex in seq(graph)) {
        paths <- paths_from_predecessor(ret$predecessor, start_vertex)
        # graph out all paths
        for (i in seq(paths)) {
            target <- rev(paths[[i]])[1]
            graph_path_dot(graph, paths[i], filename = paste0(
                gv_filename, "_graph_",
                start_vertex, "_to_", target,
                ".gv"
            ))
        all_paths[[start_vertex]] <- paths
        }
    }
    } # /\ NOT RUN /\
}

bellman_ford <- function(graph, start) {
    # Return predecessor and distance vectors
    # using the bellman-ford-moore algorithm

    # declare distance and predecessor vectors
    distance <- c()
    predecessor <- c()

    # initialize distance and predecessor vectors
    for (i in seq(graph)) {
        distance <- append(distance, Inf)
        predecessor <- append(predecessor, Inf) # use Inf as null?
    }

    distance[start] <- 0
    predecessor[start] <- 0 # 0 means it's the starting vertex

    # run length - 1 times
    for (i in 1: -1 + length(graph)) {
        # use u and v as indexes to better
        # match the mathematical definition of the algorithm
        for (u in seq(graph)) {
            for (v in seq(graph)) {
                weight <- graph[u, v]
                if (distance[u] + weight < distance[v]) {
                    distance[v] <- distance[u] + weight
                    predecessor[v] <- u
                }
            }
        }
    }
    # check for negative cycles (not really needed since it's a given)
    # but it's done incase I make a mistake with the matrix file of the graph
    for (u in seq(graph)) {
        for (v in seq(graph)) {
            weight <- graph[u, v]
            if (distance[u] + weight < distance[v]) {
                print("Graph contains negative weight cycle")
                quit(status = -1)
            }
        }
    }

    ret <- list("distance" = distance, "predecessor" = predecessor)
    return(ret)
}

paths_from_predecessor <- function(predecessor, start) {
    # Return the shortest paths from start
    # to all other vertexes using a predecessor vector.

    paths <- c()

    # for every possible end vertex (target)
    for (target in seq(predecessor)) {
        path <- c()
        nxt <- target
        # repeatedly get it's predecessor and add it to the path
        # NOTE: the path is build backwards, from the end to the start
        repeat {
            if (nxt == 0) {
                break
            }
            path <- append(path, nxt)
            if (nxt == Inf) {
                break
            }
            # if we reach the starting vertex or the vertex is unreachable, stop
            nxt <- predecessor[nxt]
        }

        # reverse that path
        path <- rev(path)

        if (path[1] == Inf) {
            paths[[target]] <- Inf # vertex unreachable
        } else if (path[1] == target) {
            paths[[target]] <- NA # path to self
        } else {
            paths[[target]] <- path # normal path
        }
    }

    return(paths)
}

load_graph <- function(filename) {
    # Load a graph from a file
    # graph should be in square matrix form
    # with integer weights separated with spaces and rows with newlines
    # having non-negative weights
    # with 0 representing the absence of an edge

    # read graph from file
    graph <- read.table(filename, header = FALSE, sep = " ")

    # replace zeros with Inf since if the edge is missing
    # the distance is infinite
    for (i in seq(graph)) {
        for (j in seq(graph)) {
            if (graph[i, j] == 0) {
                graph[i, j] <- Inf
            }
        }
    }
    return(graph)
}

graph_dot <- function(graph, start, filename) {
    # Draw a graph.
    sink(filename) # write to file instead of stdout
    cat("digraph G {\n")
    cat('rankdir="LR"\n')
    cat("node [shape=circle style=filled fillcolor=skyblue fixedsize=true]\n")
    cat("start [shape=plaintext, fillcolor=none]\n")
    cat("start ->", start, ";\n")
    for (i in seq(graph)) {
        for (j in seq(graph)) {
            weight <- graph[i, j]
            if (is.finite(weight)) {
                cat(i, "->", j, '[label="', weight, '"];\n')
            }
        }
    }
    cat("}\n")
    sink()
}

path_dot <- function(path, filename) {
    # Draw a path.
    if (path[[1]] == Inf || is.na(path[[1]])) { # stop if path does not exist
        return()
    }
    sink(filename) # write to file instead of stdout
    cat("digraph G {\n")
    cat('rankdir="LR"\n')
    cat("node [shape=box style=filled fixedsize=true fillcolor=yellow]\n")
    cat("start [shape=plaintext, fillcolor=none]\n")
    cat("start ->", path[[1]][1], ";\n")
    l <- length(path[[1]])
    for (i in seq(from = 1, to = l - 1)) {
        if (!is.na(path[[1]][i + 1])) {
            cat(path[[1]][i], "->", path[[1]][i + 1], ";\n")
        }
    }
    cat("}\n")
    sink()
}

graph_path_dot <- function(graph, path, filename) {
    # Draw a graph with path colored red.
    if (path[[1]] == Inf || is.na(path[[1]])) { # stop if path does not exist
        return()
    }
    l <- length(path[[1]])
    sink(filename) # write to file instead of stdout
    cat("digraph G {\n")
    cat('rankdir="LR"\n')
    cat("node [shape=circle style=filled fillcolor=seagreen2 fixedsize=true]\n")
    cat("start [shape=plaintext, fillcolor=none]\n")
    cat("start ->", path[[1]][1], ";\n")
    for (i in seq(graph)) {
        for (j in seq(graph)) {
            weight <- graph[i, j]
            if (is.finite(weight)) {
                color <- FALSE
                # check if edge's vertex pair exists in the path
                for (k in seq(from = 1, to = l - 1)) {
                    if (path[[1]][k] == i && path[[1]][k + 1] == j) {
                        color <- TRUE # if yes color the edge
                    }
                }
                if (color) { # create the edge
                    cat(i, "->", j, '[label="', weight, '" color=red];\n')
                } else {
                    cat(i, "->", j, '[label="', weight, '"];\n')
                }
            }
        }
    }
    cat("}\n")
    sink()
}

main()
