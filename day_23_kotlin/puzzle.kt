#!/usr/bin/env kotlin

import java.io.File

fun readInputLines(path: String): List<String> {
    val file = File(path)
    if (!file.exists()) {
        throw Exception("Error: Could not read '$path'. File does not exist.")
    }
    return file.readLines()
        .map { it.trim() }
        .filter { it.isNotEmpty() }
}

fun buildGraph(lines: List<String>): MutableMap<String, MutableSet<String>> {
    val adjacency = mutableMapOf<String, MutableSet<String>>()

    for (line in lines) {
        val parts = line.split("-")
        if (parts.size == 2) {
            val a = parts[0]
            val b = parts[1]

            adjacency.putIfAbsent(a, mutableSetOf())
            adjacency.putIfAbsent(b, mutableSetOf())

            adjacency[a]!!.add(b)
            adjacency[b]!!.add(a)
        }
    }
    return adjacency
}

fun findAllTriangles(graph: Map<String, Set<String>>): Set<String> {
    val nodes = graph.keys.toList()
    val setOfThree = mutableSetOf<String>()

    for (i in nodes.indices) {
        for (j in (i + 1) until nodes.size) {
            for (k in (j + 1) until nodes.size) {
                val a = nodes[i]
                val b = nodes[j]
                val c = nodes[k]
                if (graph[a]?.contains(b) == true &&
                    graph[a]?.contains(c) == true &&
                    graph[b]?.contains(c) == true
                ) {
                    setOfThree.add(a + b + c)
                }
            }
        }
    }
    return setOfThree
}

fun countTrianglesContainingT(triangles: Set<String>): Int {
    var total = 0
    for (s in triangles) {
        val collected = buildString {
            if (s.length > 0) append(s[0])
            if (s.length > 2) append(s[2])
            if (s.length > 4) append(s[4])
        }
        if ('t' in collected) {
            total++
        }
    }
    return total
}

fun bronKerbosch(
    R: Set<String>,
    P: Set<String>,
    X: Set<String>,
    adjacency: Map<String, Set<String>>,
    cliques: MutableList<Set<String>>
) {
    if (P.isEmpty() && X.isEmpty()) {
        // R is a maximal clique
        cliques.add(R)
        return
    }

    val pivot = (P + X).firstOrNull() 
    val neighborsOfPivot = pivot?.let { adjacency[it] } ?: emptySet()

    val candidates = P - neighborsOfPivot
    var pLocal = P.toMutableSet()

    for (v in candidates) {
        val neighborsOfV = adjacency[v] ?: emptySet()

        val newR = R + v
        val newP = pLocal.intersect(neighborsOfV)
        val newX = X.intersect(neighborsOfV)

        bronKerbosch(newR, newP, newX, adjacency, cliques)

        pLocal.remove(v)
        // move v from P to X
        // we won't mutate X directly (like in typical pseudo-code),
        // but conceptually v is removed from P and placed in X.
    }
}

fun findMaximumClique(graph: Map<String, Set<String>>): List<String> {
    val allNodes = graph.keys

    val cliques = mutableListOf<Set<String>>()

    bronKerbosch(
        R = emptySet(),
        P = allNodes,
        X = emptySet(),
        adjacency = graph,
        cliques = cliques
    )

    val maxClique = cliques.maxByOrNull { it.size } ?: emptySet()
    return maxClique.sorted()
}

fun main() {
    try {
        val lines = readInputLines("input.txt")
        val graph = buildGraph(lines)
        val triangles = findAllTriangles(graph)
        val sumOfT = countTrianglesContainingT(triangles)
        println(sumOfT)
        val maxClique = findMaximumClique(graph)
        println(maxClique.joinToString(","))
    } catch (e: Exception) {
        System.err.println(e.message)
        kotlin.system.exitProcess(1)
    }
}
