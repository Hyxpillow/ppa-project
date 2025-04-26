# 定义图结构
# graph = {0: [1, 2], 1: [0, 2, 3], 2: [0, 1, 3], 3: [1, 2]}
def load_from_file(filepath="test-graph/g1.txt"):
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

graph = load_from_file()

import networkx as nx
import matplotlib.pyplot as plt

def visualize_node_dQ(graph, communities, node, k_i, m):
    G = nx.Graph()
    for u, neighbors in graph.items():
        for v in neighbors:
            G.add_edge(u, v)

    pos = nx.spring_layout(G, seed=42)
    colors = [communities[n] for n in G.nodes]
    
    nx.draw(G, pos, with_labels=True, node_color=colors, cmap=plt.cm.Set3, node_size=800)
    nx.draw_networkx_nodes(G, pos, nodelist=[node], node_color='red', linewidths=0, node_size=800)
    current_comm = communities[node]
    for neighbor in graph[node]:
        target_comm = communities[neighbor]

        # ΔQ 计算
        k_i_in = sum(1 for n in graph[node] if communities[n] == target_comm)
        sigma_tot = sum(k_i[n] for n, c in communities.items() if c == target_comm)
        if current_comm == target_comm:
            sigma_tot -= k_i[node]
        delta_Q = k_i_in - ((sigma_tot * k_i[node]) / m)

        label_pos = pos[neighbor]
        plt.text(label_pos[0], label_pos[1]+0.05, f"ΔQ={delta_Q:.2f}", fontsize=12, color="red", ha='center')
    plt.show()

# 计算每个节点的度数(k_i)和总边数(m)
k_i = {}
m = 0
for node, neighbors in graph.items():
    k_i[node] = len(neighbors)
    m += len(neighbors)
m = m / 2  # 因为是无向图，每条边被计算了两次

print(f"节点度数(k_i): {k_i}")
print(f"总边数(m): {m}")

# 初始阶段：每个节点是独立社区
communities = {node: node for node in graph}
print(f"初始社区: {communities}")

# 使用化简公式计算模块度增益ΔQ
def calculate_delta_Q(node, current_community, target_community, communities):
    # 计算k_{i,in}：节点与目标社区的连接数
    k_i_in = sum(1 for neighbor in graph[node] if communities[neighbor] == target_community)
    
    # 计算Σtot：目标社区内所有节点的度数总和
    sigma_tot = sum(k_i[n] for n, comm in communities.items() if comm == target_community)
    if current_community == target_community:
        sigma_tot -= k_i[node]
    print("k_i=", k_i[node], "k_i_in=", k_i_in, "sigma_tot=", sigma_tot)
    # 使用化简公式计算ΔQ
    delta_Q = (k_i_in) - ((sigma_tot * k_i[node]) / m)
    
    return delta_Q

# 对每个节点进行考察
iteration = 0
changes_made = True

while changes_made:
    changes_made = False
    iteration += 1
    print(f"\n迭代 {iteration}:")
    
    for node in graph:
        visualize_node_dQ(graph, communities, node, k_i, m)
        current_community = communities[node]
        current_delta_Q = 0

        best_delta_Q = 0
        best_community = current_community
        
        # 获取邻居节点的社区（排除自己的社区）
        neighbor_communities = set(communities[neighbor] for neighbor in graph[node])
        # if current_community in neighbor_communities:
        #     neighbor_communities.remove(current_community)
        
        print(f"\n考察节点 {node}（当前在社区 {current_community}）:")
        
        # 考虑移动到每个邻居社区
        for target_community in neighbor_communities:
            # 使用化简公式计算移动到目标社区的ΔQ
            delta_Q = calculate_delta_Q(node, current_community, target_community, communities)
            
            print(f"  移动到社区 {target_community}，ΔQ = {delta_Q:.6f}")
            
            if delta_Q == best_delta_Q and current_community == target_community:
                best_delta_Q = delta_Q
                best_community = current_community

            if delta_Q > best_delta_Q:
                best_delta_Q = delta_Q
                best_community = target_community
            
        
        # 如果找到更好的社区，移动节点
        if best_delta_Q > 0 and not current_community == best_community:
            print(f"  -> 将节点 {node} 从社区 {current_community} 移动到社区 {best_community}，ΔQ = {best_delta_Q:.6f}")
            communities[node] = best_community
            changes_made = True
        else:
            print(f"  -> 节点 {node} 保持在社区 {current_community}")
    
    if not changes_made:
        print("\n没有节点需要移动,阶段1完成")

num_nodes = max(communities) + 1
community_list = [0] * num_nodes

for node, comm in communities.items():
    community_list[node] = comm

with open("comm.txt", "w") as f:
    f.write(str(community_list))

import networkx as nx
import matplotlib.pyplot as plt

# 创建图
G = nx.Graph()
for node, neighbors in graph.items():
    for neighbor in neighbors:
        G.add_edge(node, neighbor)

# 社区着色
colors = [communities[node] for node in G.nodes]

# 画图
plt.clf()
pos = nx.spring_layout(G, seed=42)  # 稍微美观点
nx.draw(G, pos, with_labels=True, node_color=colors, cmap=plt.cm.Set3, node_size=800)
plt.show()