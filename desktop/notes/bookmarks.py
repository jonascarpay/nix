#!/usr/bin/env python

from typing import Dict, List
import os
import re
import subprocess
import sys

regex_named_url: re.Pattern = re.compile(r"\[([^\]]+)\]\(([^\)]+)\)")
regex_bare_url: re.Pattern = re.compile(r"<(\S+)>")

urls: Dict[str, str] = dict()
note_root: str = sys.argv[1]
history_file: str = sys.argv[2]
max_file_display_len = 16

for root, _, files in os.walk(note_root):
    for file in files:
        if not file.endswith(".md"):
            continue
        full = os.path.join(root, file)
        file_clean, _ = os.path.splitext(file)
        if len(file_clean) > max_file_display_len:
            file_display = file_clean[: max_file_display_len - 1] + "…"
        else:
            file_display = file_clean + (max_file_display_len - len(file_clean)) * " "
        with open(full) as note:
            contents = note.read()
            for title, url in regex_named_url.findall(contents):
                urls[url] = f"{file_display} {title}"
            for url in regex_bare_url.findall(contents):
                urls[url] = f"{file_display} {url}"


urls_sorted: List[str] = subprocess.run(
    [
        "frecently",
        "view",
        history_file,
        "-ar",
    ],
    input="\n".join(urls),
    encoding="ascii",
    capture_output=True,
).stdout.splitlines()

pick = subprocess.run(
    ["dmenu", "-i", "-sr", "-p", " ", "-ix"],
    input="\n".join([urls[url] for url in urls_sorted]),
    encoding="utf8",
    capture_output=True,
)

pick_ix = int(pick.stdout)
pick_url = urls_sorted[pick_ix]

subprocess.run(["frecently", "bump", history_file, pick_url])
subprocess.run(["xdg-open", pick_url])
