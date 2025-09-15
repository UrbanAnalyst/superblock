/* Directed Graphs
 * ----------------------------------------------------------------------------
 * Author: Mark Padgham, modified from code by Shane Saunders
 */
#include "dgraph.h"

#include <Rcpp.h>
//#include <cstdio>

/*--- DGraph ----------------------------------------------------------------*/

/* --- Constructor ---
 * Creates a DGraph object containing n vertices.
 */
DGraph::DGraph(size_t n) : m_vertices(n)
{
    initVertices();
}

/* --- Destructor ---
*/
DGraph::~DGraph()
{
    clear();
}

// length of vertices
size_t DGraph::nVertices() const
{
  return static_cast <size_t> (m_vertices.size());
}

const std::vector<DGraphVertex>& DGraph::vertices() const
{
  return m_vertices;
}

/* --- clear() ---
 * Clears all edges from the graph.
 */
void DGraph::clear()
{
    DGraphEdge *edge, *nextEdge;
    for(size_t i = 0; i < m_vertices.size(); i++) {
        edge = m_vertices[i].outHead;

        while(edge) {
            nextEdge = edge->nextOut;
            delete edge;
            edge = nextEdge;
        }
    }
    initVertices();
}

void DGraph::initVertices()
{
    for(size_t i = 0; i < m_vertices.size(); i++) {
        m_vertices[i].outHead = m_vertices[i].outTail = nullptr;
        m_vertices[i].inHead = m_vertices[i].inTail = nullptr;
        m_vertices[i].outSize = m_vertices[i].inSize = 0;
    }
}

/* --- addNewEdge() ---
 * Adds a new edge from vertex 'source' to vertex 'target' with
 * with a corresponding distance of dist.
 */
void DGraph::addNewEdge(size_t source, size_t target,
        double dist, size_t edge_id)
{
    DGraphEdge *newEdge = new DGraphEdge;
    newEdge->source = source;
    newEdge->target = target;
    newEdge->edge_id = edge_id;
    newEdge->dist = dist;
    newEdge->nextOut = nullptr;
    newEdge->nextIn = nullptr;

    DGraphVertex *vertex = &m_vertices[source];
    if(vertex->outTail) {
        vertex->outTail->nextOut = newEdge;
    }
    else {
        vertex->outHead = newEdge;
    }
    vertex->outTail = newEdge;
    vertex->outSize++;

    vertex = &m_vertices[target];
    if(vertex->inTail) {
        vertex->inTail->nextIn = newEdge;
    }
    else {
        vertex->inHead = newEdge;
    }
    vertex->inTail = newEdge;
    vertex->inSize++;
}

bool DGraph::edgeExists(size_t v, size_t w) const
{
    /* Scan all existing edges from v to determine whether an edge to w exists.
    */
    const DGraphEdge *edge = m_vertices[v].outHead;
    while(edge) {
        if(edge->target == w) return true;
        edge = edge->nextOut;
    }
    return false;
}
