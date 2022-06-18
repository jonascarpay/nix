import sys
import re
import json
from dataclasses import dataclass
from typing import List


count: int = 0


@dataclass
class Node:
    id: int
    text: str
    floating: bool
    children: List["Node"]


def parse_json(x: dict) -> Node:
    global count
    id = count
    count += 1
    floating = x.get("type") == "floating_con"
    layout = x.get("layout")
    if layout:
        return Node(
            id, layout, floating, [parse_json(child) for child in x.get("nodes", [])]
        )
    else:
        text = x.get("name")
        return Node(id, f"{text:30}..." if len(text) > 30 else text, floating, [])


def write_node(node: Node):
    style = ", style=dashed" if node.floating else ""
    print(f'  node_{node.id} [label="{node.text[:30]}"{style}];')
    for child in node.children:
        write_node(child)
        print(f"  node_{node.id} -> node_{child.id};")


stdin_filtered = "".join([line for line in sys.stdin if not re.match(r"^\s*//", line)])
toplevel = [
    parse_json(json.loads(line))
    for line in stdin_filtered.split("\n\n")
    if len(line) > 0
]

print("digraph{")
for node in toplevel:
    write_node(node)
print("}")
