import re
import sys
import os
import datetime
import subprocess
from typing import Iterable
from dataclasses import dataclass

# - [ ] [YYYY-MM-DD[ HH:MM][ <[+[+]]nn uu[!|?]>]]text
regex: re.Pattern = re.compile(
    r"""^
        -\s\[\s\]\s # leading "- [ ] "
        (?P<datetime>
            (?P<year>\d\d\d\d)-
            (?P<month>\d\d)-
            (?P<day>\d\d)
            (?P<time>\s
                (?P<hour>\d\d):
                (?P<minute>\d\d)
            )?
            (?P<increment>
                \s<
                    ( (?P<fromtoday>\+)
                    | (?P<fromnow>\+\+)
                    )?
                    (?P<amount>\d+)\s
                    (?P<unit>days?|hours?|weeks?|months?|years?)
                    ( (?P<logtext>!)
                    | (?P<logquiet>\?)
                    )?
                >
            )?
        )?
        (?P<text>.*)
    $""",
    re.VERBOSE,
)

note_root: str = sys.argv[1]
command: str = sys.argv[2]
datetime.datetime

now = datetime.datetime.now()


@dataclass
class Todo:
    path: str
    file: str
    line: int
    text: str


def todos() -> Iterable[Todo]:
    for root, _, files in os.walk(note_root):
        for file in files:
            if not file.endswith(".md"):
                continue
            full = os.path.join(root, file)
            file_clean, _ = os.path.splitext(file)
            with open(full) as note:
                for (linenr, line) in enumerate(note):
                    m = regex.match(line)
                    if m:
                        d = m.groupdict()
                        timestamp = datetime.datetime(
                            year=int(d.get("year") or 1990),
                            month=int(d.get("month") or 9),
                            day=int(d.get("day") or 9),
                            hour=int(d.get("hour") or 0),
                            minute=int(d.get("minute") or 0),
                        )
                        if timestamp < now:
                            yield (Todo(full, file_clean, linenr + 1, d["text"]))


todolist = list(todos())
if command == "count":
    print(len(todolist))
elif command == "open":
    pick = subprocess.run(
        ["dmenu", "-sr", "-p", " ", "-ix"],
        input="\n".join([f"{todo.file}:{todo.text}" for todo in todolist]),
        encoding="utf8",
        capture_output=True,
    )
    todo = todolist[int(pick.stdout)]
    # Popen here forks, run doesn't
    subprocess.Popen(
        ["st"]
        + sys.argv[3:]
        + ["-d", note_root, "-e", "vim", f"+{todo.line}", todo.path]
    )
else:
    print(f"Unknown command: {command}")
