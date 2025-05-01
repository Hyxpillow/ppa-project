import random

num_communities = 8
nodes_per_community = 20
edges = set()
current_node = 0

# 构造每个社区为稠密图（非完全图，避免爆炸）
for i in range(num_communities):
    nodes = list(range(current_node, current_node + nodes_per_community))
    for u in nodes:
        for _ in range(10):  # 每个点连10条边到同社区随机点
            v = random.choice(nodes)
            if u != v:
                edges.add(tuple(sorted((u, v))))
    current_node += nodes_per_community

# 添加跨社区连接：每个社区随机挑几个点连接其他社区
community_reps = [i * nodes_per_community for i in range(num_communities)]
for i in range(num_communities):
    for j in range(i + 1, num_communities):
        u = random.choice(range(i * nodes_per_community, (i + 1) * nodes_per_community))
        v = random.choice(range(j * nodes_per_community, (j + 1) * nodes_per_community))
        edges.add(tuple(sorted((u, v))))

# 写入 txt 文件
with open("meGraph.txt", "w") as f:
    for u, v in sorted(edges):
        f.write(f"{u} {v}\n")
