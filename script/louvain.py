import networkx as nx
import matplotlib.pyplot as plt
import os
import imageio
import shutil


# 定义图结构加载函数
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


# 创建临时文件夹存储帧
def setup_temp_directory():
    temp_dir = "temp_frames_louvain"
    if os.path.exists(temp_dir):
        shutil.rmtree(temp_dir)
    os.makedirs(temp_dir)
    return temp_dir


# 保存每个节点考察的过程为帧
def visualize_node_dQ(graph, communities, node, k_i, m, temp_dir, frame_count):
    """可视化节点移动过程并保存为帧"""
    G = nx.Graph()
    for u, neighbors in graph.items():
        for v in neighbors:
            G.add_edge(u, v)

    plt.figure(figsize=(10, 8))
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
        plt.text(label_pos[0], label_pos[1] + 0.05, f"ΔQ={delta_Q:.2f}", fontsize=12, color="red", ha='center')

    # 添加标题
    plt.title(f"考察节点 {node}（当前在社区 {current_comm}）")

    # 保存为帧
    frame_path = os.path.join(temp_dir, f"frame_{frame_count:04d}.png")
    plt.savefig(frame_path, dpi=100, bbox_inches='tight')
    plt.close()

    return frame_path


# 可视化最终社区
def visualize_final_communities(graph, communities, temp_dir, frame_count):
    """可视化最终社区分配并保存为帧"""
    G = nx.Graph()
    for node, neighbors in graph.items():
        for neighbor in neighbors:
            G.add_edge(node, neighbor)

    plt.figure(figsize=(10, 8))
    pos = nx.spring_layout(G, seed=42)  # 使用固定的布局
    colors = [communities[node] for node in G.nodes]

    nx.draw(G, pos, with_labels=True, node_color=colors, cmap=plt.cm.Set3, node_size=800)
    plt.title("最终社区划分")

    # 保存为最终帧
    frame_path = os.path.join(temp_dir, f"frame_{frame_count:04d}.png")
    plt.savefig(frame_path, dpi=100, bbox_inches='tight')
    plt.close()

    return frame_path


# 创建GIF动画
def create_gif(frame_paths, output_path, duration=1.0):
    """创建GIF动画"""
    with imageio.get_writer(output_path, mode='I', duration=duration) as writer:
        for frame_path in frame_paths:
            image = imageio.imread(frame_path)
            writer.append_data(image)

    print(f"GIF动画已保存至 {output_path}")


def calculate_delta_Q(node, current_community, target_community, communities, graph, k_i, m):
    """计算模块度增益ΔQ"""
    # 计算k_{i,in}：节点与目标社区的连接数
    k_i_in = sum(1 for neighbor in graph[node] if communities[neighbor] == target_community)

    # 计算Σtot：目标社区内所有节点的度数总和
    sigma_tot = sum(k_i[n] for n, comm in communities.items() if comm == target_community)
    if current_community == target_community:
        sigma_tot -= k_i[node]

    # 使用化简公式计算ΔQ
    delta_Q = (k_i_in) - ((sigma_tot * k_i[node]) / m)

    return delta_Q


def main():
    """主函数"""
    # 创建临时目录用于存储帧
    temp_dir = setup_temp_directory()
    frames = []  # 用于存储所有帧的路径
    frame_count = 0

    # 加载图结构
    try:
        graph = load_from_file()
    except:
        # 如果文件不存在，使用默认图
        print("使用默认图结构")
        graph = {0: [1, 2], 1: [0, 2, 3], 2: [0, 1, 3], 3: [1, 2]}

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

    # 对每个节点进行考察
    iteration = 0
    changes_made = True

    # 绘制初始状态
    initial_frame = visualize_final_communities(graph, communities, temp_dir, frame_count)
    frames.append(initial_frame)
    frame_count += 1

    while changes_made:
        changes_made = False
        iteration += 1
        print(f"\n迭代 {iteration}:")

        for node in graph:
            current_community = communities[node]
            best_delta_Q = 0
            best_community = current_community

            # 获取邻居节点的社区
            neighbor_communities = set(communities[neighbor] for neighbor in graph[node])

            print(f"\n考察节点 {node}（当前在社区 {current_community}）:")

            # 生成可视化帧
            frame = visualize_node_dQ(graph, communities, node, k_i, m, temp_dir, frame_count)
            frames.append(frame)
            frame_count += 1

            # 考虑移动到每个邻居社区
            for target_community in neighbor_communities:
                # 计算移动到目标社区的ΔQ
                delta_Q = calculate_delta_Q(node, current_community, target_community, communities, graph, k_i, m)

                print(f"  移动到社区 {target_community}，ΔQ = {delta_Q:.6f}")

                if delta_Q == best_delta_Q and current_community == target_community:
                    best_delta_Q = delta_Q
                    best_community = current_community

                if delta_Q > best_delta_Q:
                    best_delta_Q = delta_Q
                    best_community = target_community

            # 如果找到更好的社区，移动节点
            if best_delta_Q > 0 and best_community != current_community:
                print(
                    f"  -> 将节点 {node} 从社区 {current_community} 移动到社区 {best_community}，ΔQ = {best_delta_Q:.6f}")
                communities[node] = best_community
                changes_made = True

                # 添加移动后的状态帧
                frame = visualize_final_communities(graph, communities, temp_dir, frame_count)
                frames.append(frame)
                frame_count += 1
            else:
                print(f"  -> 节点 {node} 保持在社区 {current_community}")

        if not changes_made:
            print("\n没有节点需要移动,阶段1完成")

    # 将社区结果保存到文件
    num_nodes = max(communities) + 1
    community_list = [0] * num_nodes

    for node, comm in communities.items():
        community_list[node] = comm

    with open("comm.txt", "w") as f:
        f.write(str(community_list))

    # 生成最终可视化帧
    final_frame = visualize_final_communities(graph, communities, temp_dir, frame_count)
    frames.append(final_frame)

    # 创建GIF
    create_gif(frames, "louvain_animation.gif", duration=0.75)

    # 清理临时文件
    shutil.rmtree(temp_dir)

    return communities


if __name__ == "__main__":
    main()