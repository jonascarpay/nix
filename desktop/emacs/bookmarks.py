#!/usr/bin/env python

from typing import Dict, List
import os
import re
import subprocess
import sys

# https://orgmode.org/worg/dev/org-syntax.html#orgc4026b3
regex_named_url: re.Pattern = re.compile(
    r"""
    \[
        \[
            ( \S+ :// [^\]]+ )
        \]
        \[
            ( [^\]]+ )
        \]
    \]
    """,
    re.VERBOSE,
)
regex_bare_url_angle: re.Pattern = re.compile(
    r"""
        <
            ( \S+ :// [^>]+ )
        >
    """,
    re.VERBOSE,
)
regex_bare_url_square: re.Pattern = re.compile(
    r"""
    \[
        \[
            ( \S+ :// [^\]] )
        \]
    \]
    """,
    re.VERBOSE,
)

urls: Dict[str, str] = dict()
note_root: str = os.path.realpath(sys.argv[1])
history_file: str = sys.argv[2]
max_file_display_len = 16

for root, dirs, files in os.walk(note_root, topdown=True):
    dirs[:] = [d for d in dirs if not d.startswith(".")]
    for file in [f for f in files if f.endswith(".org")]:
        full = os.path.join(root, file)
        file_clean, _ = os.path.splitext(file)
        if len(file_clean) > max_file_display_len:
            file_display = file_clean[: max_file_display_len - 1] + "…"
        else:
            file_display = file_clean + (max_file_display_len - len(file_clean)) * " "
        with open(full) as note:
            contents = note.read()
            for url, title in regex_named_url.findall(contents):
                urls[url] = f"{file_display} {title}"
            for url in regex_bare_url_square.findall(contents):
                urls[url] = f"{file_display} {url}"
            for url in regex_bare_url_angle.findall(contents):
                urls[url] = f"{file_display} {url}"


urls_sorted: List[str] = subprocess.run(
    [
        "frecently",
        "view",
        history_file,
        "-ar",
    ],
    input="\n".join(urls),
    encoding="utf8",
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
