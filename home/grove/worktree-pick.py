#!/usr/bin/env python3
from __future__ import annotations

import subprocess
import sys
from pathlib import Path


def run_command(cmd: list[str]) -> str:
    try:
        out = subprocess.run(cmd, check=True, text=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        print(e.stderr, file=sys.stderr)
        sys.exit(e.returncode)
    else:
        return out.stdout


root = Path(run_command(["git", "rev-parse", "--git-common-dir"])).absolute().parent


def get_worktrees() -> list[tuple[Path, str, str | None]]:
    blocks = run_command(["git", "worktree", "list", "--porcelain", "-z"]).split("\0\0")
    res = []
    for block in blocks:
        worktree = None
        head = None
        branch = None
        for line in block.split("\0"):
            match line.split(" ", maxsplit=1):
                case "worktree", abspath:
                    worktree = Path(abspath)
                case "HEAD", commit:
                    head = commit
                case "branch", name:
                    branch = name
        if worktree is not None and head is not None:
            res.append((worktree, head, branch))

    return res


for path, head, branch in get_worktrees():
    if branch is not None:
        branch_abbr = run_command(["git", "rev-parse", "--abbrev-ref", branch]).strip()
        branch_str = f"[{branch_abbr}]"
    else:
        branch_str = None
    print(f"{str(path.relative_to(root))!s:<31} {branch_str:<20} ({head[:7]})")
