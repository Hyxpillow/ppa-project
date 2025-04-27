import networkx as nx
import matplotlib.pyplot as plt
from collections import deque
import copy

# 在全局定义固定位置
# 全局变量
fixed_pos = None
iteration = 0
max_Q = -1
best_communities = None

def calculate_modularity(graph, communities):
    """计算模块度"""
    G = nx.Graph(graph)
    m = G.number_of_edges()
    if m == 0:
        return 0.0
    
    degrees = dict(G.degree())
    Q = 0.0
    
    for community in communities:
        for i in community:
            for j in community:
                A_ij = 1 if G.has_edge(i, j) else 0
                Q += (A_ij - degrees[i]*degrees[j]/(2*m))
    
    return Q / (2*m)

def visualize_edge_betweenness(graph, communities):
    global fixed_pos, iteration, max_Q, best_communities
    
    G = nx.Graph(graph)
    all_nodes = set(graph.keys())
    for u in graph:
        for v in graph[u]:
            G.add_edge(u, v)
        if u not in G:
            G.add_node(u)
    
    # 只在第一次计算位置
    if fixed_pos is None:
        fixed_pos = nx.spring_layout(G, seed=42)
    
    # 计算当前模块度
    cur_Q = calculate_modularity(graph, communities)
    
    # 更新最大模块度
    global max_Q, best_communities
    if cur_Q > max_Q:
        max_Q = cur_Q
        best_communities = copy.deepcopy(communities)
    
    plt.figure(figsize=(12, 8))
    
    # 绘制所有节点
    node_colors = []
    for node in G.nodes():
        for i, comm in enumerate(communities):
            if node in comm:
                node_colors.append(i)
                break
    
    nx.draw_networkx_nodes(G, fixed_pos, node_color=node_colors, cmap=plt.cm.Set3, node_size=800)
    nx.draw_networkx_labels(G, fixed_pos)
    
    # 绘制边
    nx.draw_networkx_edges(G, fixed_pos)
    
    # 计算并显示边介数
    edge_betweenness = calculate_edge_betweenness(graph)
    edge_labels = {}
    for (u, v), eb in edge_betweenness.items():
        if G.has_edge(u, v):
            edge_labels[(u, v)] = f"{eb:.2f}"
    
    nx.draw_networkx_edge_labels(G, fixed_pos, edge_labels=edge_labels, font_color='red')
    
    # 在标题中显示迭代信息和模块度
    title = f"(Iteration: {iteration}, Current Q: {cur_Q:.4f}, Max Q: {max_Q:.4f})"
    plt.title(title, fontsize=12)
    
    plt.show()
    
    return edge_betweenness
def calculate_edge_betweenness(graph):
    edge_betweenness = {}
    nodes = list(graph.keys())
    
    for node in nodes:
        # BFS初始化
        visited = {n: False for n in nodes}
        distance = {n: -1 for n in nodes}
        paths = {n: 0 for n in nodes}
        parents = {n: [] for n in nodes}
        
        # 从当前节点开始
        queue = deque()
        queue.append(node)
        visited[node] = True
        distance[node] = 0
        paths[node] = 1
        
        # BFS遍历
        while queue:
            current = queue.popleft()
            
            for neighbor in graph.get(current, []):
                if not visited[neighbor]:
                    visited[neighbor] = True
                    distance[neighbor] = distance[current] + 1
                    queue.append(neighbor)
                
                if distance[neighbor] == distance[current] + 1:
                    paths[neighbor] += paths[current]
                    parents[neighbor].append(current)
        
        # 反向传播计算边介数
        node_credits = {n: 1 for n in nodes}
        stack = sorted(nodes, key=lambda x: -distance[x])
        
        for n in stack:
            for parent in parents[n]:
                edge = tuple(sorted((parent, n)))
                credit = node_credits[n] * (paths[parent] / paths[n])
                
                if edge not in edge_betweenness:
                    edge_betweenness[edge] = 0
                edge_betweenness[edge] += credit
                
                node_credits[parent] += credit
    
    # 因为是无向图，每条边被计算了两次，所以除以2
    for edge in edge_betweenness:
        edge_betweenness[edge] /= 2
    
    return edge_betweenness
def girvan_newman_algorithm(graph):
    global iteration, max_Q, best_communities
    
    G = nx.Graph(graph)
    communities = list(nx.connected_components(G))
    max_Q = calculate_modularity(graph, communities)
    best_communities = copy.deepcopy(communities)
    
    while G.number_of_edges() > 0:
        iteration += 1
        current_graph = {n: list(G.neighbors(n)) for n in G.nodes()}
        
        # 可视化当前状态
        edge_betweenness = visualize_edge_betweenness(current_graph, communities)
        
        # 找到介数最大的边
        max_edge = max(edge_betweenness.items(), key=lambda x: x[1])[0]
        print(f"Iteration {iteration}: Removing edge {max_edge} with betweenness {edge_betweenness[max_edge]:.2f}")
        
        # 移除边
        G.remove_edge(*max_edge)
        
        # 更新社区结构
        new_communities = list(nx.connected_components(G))
        if len(new_communities) > len(communities):
            communities = new_communities
            print(f"New communities found: {communities}")
    
    # 最终可视化
    iteration += 1
    current_graph = {n: list(G.neighbors(n)) for n in G.nodes()}
    visualize_edge_betweenness(current_graph, communities)
    
    print("\nAlgorithm finished!")
    print(f"Maximum modularity (Q) found: {max_Q:.4f}")
    print(f"Best communities: {best_communities}")
    
    return best_communities

# 示例图
graph = {0: [1, 2], 1: [0, 2], 2: [0, 1, 3], 3: [2, 4, 5], 4: [3, 5], 5: [3, 4]}

# 运行算法
communities = girvan_newman_algorithm(graph)