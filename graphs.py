# -*- coding: utf-8; show-trailing-whitespace: t; indent-tabs-mode: nil -*-
from pygraphviz import AGraph

context = []


class Node(object):
    def __init__(self, name):
        self.name = name
        self.sub_pages = []
        context.append(self)

    def __repr__(self):
        d = "%s" % self.name
        if self.sub_pages:
            d += "%s" % self.sub_pages
        return d

g = AGraph(directed=True, strict=False, rankdir="LR")

a = Node("a")
dub_a = Node("a")
b = Node("b")
c = Node("c")
d = Node("d")

ab, ad = Node("b"), Node("d")
bc = Node("c")
ca = Node("a")

a.sub_pages = [ab, ad]
b.sub_pages = [bc]
c.sub_pages = [ca]

abg, abh = Node("g"), Node("h")
ade = Node("e")

ab.sub_pages = [abg, abh]
ad.sub_pages = [ade]

# root = [a,b,c,d]
print context

for i, n in enumerate(context):

    # node_name = "%s %d" % (n.name, i)
    node_name = "%s" % (n.name,)
    node_label = "%s" % (n.name,)
    g.add_node(node_name)
    node = g.get_node(node_name)
    node.attr["shape"] = "record"
    node.attr["label"] = "<f0> %s|<f1> text" % node_label

    for j, sn in enumerate(n.sub_pages):
        sub_node_name = "%s" % (sn.name,)
        sub_node_label = "%s" % (sn.name,)
        g.add_edge(node_name, sub_node_name, headport="f0", tailport="f1")
        # sub_node_name = "%s %d" % (sn.name, i, j)
        # g.subgraph(nbunch=[j.name for j in n.sub_pages].append(node_name),
        #            name="cluster %d" % i,
        #            label="cluster %d" % i)

# g.add_node(1)
# g.add_node(2)
# g.add_node(3)
# g.add_node(4)

# g.add_edge(1,3)

# g.node_attr['color']='red'
# g.node_attr['style']='filled'
# g.graph_attr['label']='a graph'

# g1 = g.subgraph(nbunch=[1,2],
#                 name="clusterX1",
#                 style='filled',
#                 color='lightgrey',
#                 label='cluster 1 label')

# # this doesn't work correctly
# #g1.node_attr['style']='filled'
# #g1.node_attr['color']='blue'

# attributes={}
# attributes.update(style='filled',
#                   color='yellow',
#                   label='cluster 2 label')

# g2 = g.subgraph(nbunch=[3,4],name="cluster2",**attributes)

g.draw("graph/zzz.png", prog="dot")
g.write("graph/zzz.dot")
