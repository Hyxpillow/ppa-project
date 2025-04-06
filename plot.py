import sys
import networkx as nx
import matplotlib.pyplot as plt

def load_from_file(filepath):
    graph = {}
    with open(filepath, 'r') as f:
        for line in f:
            u, v = map(int, line.strip().split())
            if u not in graph:
                graph[u] = []
            if v not in graph:
                graph[v] = []
            graph[u].append(v)
            graph[v].append(u)
    return graph

def load_communities_from_file(filepath):
    with open(filepath, "r") as f:
        line = f.read().strip()
        communities = eval(line)
    return communities

if len(sys.argv) != 2:
    print("Usage: python3 plot.py <GRAPH_FILE>")
    sys.exit(1)

graph_file = sys.argv[1]

graph = load_from_file("test-graph/" + graph_file)
communities = load_communities_from_file("test-graph-output/" + graph_file)

G = nx.Graph()
for node, neighbors in graph.items():
    for neighbor in neighbors:
        G.add_edge(node, neighbor)

node_colors = [communities[node] for node in G.nodes()]

pos = nx.spring_layout(G, seed=42)
nx.draw(G, pos, with_labels=False, node_color=node_colors, cmap=plt.cm.tab10)
plt.show()
