import networkx as nx
import matplotlib.pyplot as plt
from collections import deque
import copy
import os
import imageio
import shutil

# 全局变量
fixed_pos = None
iteration = 0
max_Q = -1
best_communities = None
current_edge_contributions = {}
current_edge_contributions_acc = {}  # 累计贡献字典
frames = []  # 用于存储GIF帧的列表


# 创建临时文件夹存储帧
def setup_temp_directory():
    # 创建临时目录用于存储帧
    temp_dir = "temp_frames"
    if os.path.exists(temp_dir):
        shutil.rmtree(temp_dir)
    os.makedirs(temp_dir)
    return temp_dir


def visualize_edge_betweenness(graph, communities, temp_dir):
    global fixed_pos, iteration, max_Q, best_communities, current_edge_contributions, current_edge_contributions_acc

    G = nx.Graph(graph)
    all_nodes = set(graph.keys())
    for u in graph:
        for v in graph[u]:
            G.add_edge(u, v)
        if u not in G:
            G.add_node(u)

    if fixed_pos is None:
        fixed_pos = nx.spring_layout(G, seed=42)

    cur_Q = calculate_modularity(graph, communities)

    if cur_Q > max_Q:
        max_Q = cur_Q
        best_communities = copy.deepcopy(communities)

    full_edge_betweenness = calculate_edge_betweenness(graph)

    # 对每个节点作为起点进行可视化
    frame_count = 0
    for start_node in sorted(graph.keys()):
        plt.figure(figsize=(12, 8))

        # 绘制所有节点
        node_colors = []
        for node in G.nodes():
            for i, comm in enumerate(communities):
                if node in comm:
                    node_colors.append(i)
                    break

        nx.draw_networkx_nodes(G, fixed_pos, node_color=node_colors,
                               cmap=plt.cm.Set3, node_size=800)
        nx.draw_networkx_labels(G, fixed_pos)
        nx.draw_networkx_edges(G, fixed_pos)

        # 计算当前节点的边介数贡献
        node_eb_contribution = calculate_node_contribution(graph, start_node)

        # 创建两个标签字典
        acc_labels = {}
        current_labels = {}

        # 更新累计贡献并创建标签
        for edge in full_edge_betweenness:
            u, v = edge
            if G.has_edge(u, v):
                # 当前贡献
                current_val = node_eb_contribution.get(edge, 0)
                current_edge_contributions_acc[edge] = current_edge_contributions_acc.get(edge, 0)

                # 创建带颜色的标签
                acc_labels[edge] = f"{int(current_edge_contributions_acc[edge])}"
                current_labels[edge] = f"+{int(current_val)}"
                current_edge_contributions_acc[edge] = current_edge_contributions_acc.get(edge, 0) + current_val

        # 绘制累计贡献（黑色）
        nx.draw_networkx_edge_labels(
            G, fixed_pos,
            edge_labels=acc_labels,
            font_color='black',
            label_pos=0.4  # 调整标签位置
        )

        # 绘制当前贡献（红色）
        nx.draw_networkx_edge_labels(
            G, fixed_pos,
            edge_labels=current_labels,
            font_color='red',
            label_pos=0.6  # 调整标签位置
        )

        # 高亮显示当前起点节点
        nx.draw_networkx_nodes(G, fixed_pos, nodelist=[start_node],
                               node_color='red', node_size=1000)

        title = f"Iteration {iteration} - Start Node: {start_node}\n"
        title += f"Current Q: {cur_Q:.4f}, Max Q: {max_Q:.4f}"
        plt.title(title, fontsize=12)

        # 保存为图像文件
        frame_path = os.path.join(temp_dir, f"frame_{iteration:03d}_{frame_count:03d}.png")
        plt.savefig(frame_path, dpi=100, bbox_inches='tight')
        frames.append(frame_path)

        plt.close()
        frame_count += 1

    return full_edge_betweenness


def girvan_newman_algorithm(graph):
    global iteration, max_Q, best_communities, current_edge_contributions_acc, frames

    # 创建临时目录用于存储帧
    temp_dir = setup_temp_directory()

    G = nx.Graph(graph)
    communities = list(nx.connected_components(G))
    max_Q = calculate_modularity(graph, communities)
    best_communities = copy.deepcopy(communities)

    while G.number_of_edges() > 0:
        iteration += 1
        current_edge_contributions_acc = {}  # 重置累计贡献
        current_graph = {n: list(G.neighbors(n)) for n in G.nodes()}

        edge_betweenness = visualize_edge_betweenness(current_graph, communities, temp_dir)

        max_edge = max(edge_betweenness.items(), key=lambda x: x[1])[0]
        print(f"Iteration {iteration}: Removing edge {max_edge} with betweenness {edge_betweenness[max_edge]}")

        G.remove_edge(*max_edge)

        new_communities = list(nx.connected_components(G))
        if len(new_communities) > len(communities):
            communities = new_communities
            print(f"New communities found: {communities}")

    # 最终可视化
    iteration += 1
    current_graph = {n: list(G.neighbors(n)) for n in G.nodes()}
    visualize_edge_betweenness(current_graph, communities, temp_dir)

    print("\nAlgorithm finished!")
    print(f"Maximum modularity (Q) found: {max_Q:.4f}")
    print(f"Best communities: {best_communities}")

    # 创建GIF
    create_gif(frames, "girvan_newman_animation.gif")

    # 清理临时文件
    shutil.rmtree(temp_dir)

    return best_communities


def create_gif(frame_paths, output_path, duration=1.0):
    """创建GIF动画"""
    with imageio.get_writer(output_path, mode='I', duration=duration) as writer:
        for frame_path in frame_paths:
            image = imageio.imread(frame_path)
            writer.append_data(image)

    print(f"GIF animation saved to {output_path}")


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
                Q += (A_ij - degrees[i] * degrees[j] / (2 * m))

    return Q / (2 * m)


def calculate_node_contribution(graph, start_node):
    """计算单个节点作为起点时的边介数贡献"""
    nodes = list(graph.keys())
    edge_contribution = {}

    # BFS初始化
    visited = {n: False for n in nodes}
    distance = {n: -1 for n in nodes}
    paths = {n: 0 for n in nodes}
    parents = {n: [] for n in nodes}

    # 从当前节点开始
    queue = deque()
    queue.append(start_node)
    visited[start_node] = True
    distance[start_node] = 0
    paths[start_node] = 1

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

    # 反向传播计算贡献
    node_credits = {n: 1 for n in nodes}
    stack = sorted(nodes, key=lambda x: -distance[x])

    for n in stack:
        for parent in parents[n]:
            edge = tuple(sorted((parent, n)))
            credit = node_credits[n] * (paths[parent] / paths[n])

            if edge not in edge_contribution:
                edge_contribution[edge] = 0
            edge_contribution[edge] += credit

            node_credits[parent] += credit

    # 无向图每条边只计算一次（不除以2）
    return edge_contribution


def calculate_edge_betweenness(graph):
    """完整的边介数计算（聚合所有节点的贡献）"""
    edge_betweenness = {}
    nodes = list(graph.keys())

    for node in nodes:
        node_contribution = calculate_node_contribution(graph, node)
        for edge, val in node_contribution.items():
            if edge not in edge_betweenness:
                edge_betweenness[edge] = 0
            edge_betweenness[edge] += val

    # 无向图，每条边被计算了两次，所以除以2
    for edge in edge_betweenness:
        edge_betweenness[edge] /= 2

    return edge_betweenness


# 使用方法示例
if __name__ == "__main__":
    # 示例图
    graph = {0: [1, 2], 1: [0, 2], 2: [0, 1, 3], 3: [2, 4, 5], 4: [3, 5], 5: [3, 4]}

    # 运行算法并生成GIF
    communities = girvan_newman_algorithm(graph)