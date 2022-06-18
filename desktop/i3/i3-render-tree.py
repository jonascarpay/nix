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
    layout: bool
    children: List["Node"]


def parse_json(x: dict) -> Node:
    global count
    id = count
    count += 1
    floating = x.get("type") == "floating_con"
    layout = x.get("layout")
    if layout:
        return Node(
            id,
            layout,
            floating,
            True,
            [parse_json(child) for child in x.get("nodes", [])],
        )
    else:
        text = x.get("name")
        return Node(id, text, floating, False, [])


def write_node(node: Node):
    style = ""
    if node.floating:
        style += ", style=dashed"
    if node.layout:
        style += ", shape=box"
    text = node.text if len(node.text) < 30 else f"{node.text[:30].strip()}..."
    print(f'  node_{node.id} [label="{text}"{style}];')
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
