import re
import sys
from pathlib import Path
from subprocess import run

[_, frecently, history_file, project_root_str] = sys.argv

known_dirs = run([frecently, "view", history_file], check=True, capture_output=True).stdout.decode().splitlines()

for d in known_dirs:
    if not Path(d).exists():
        run([frecently, "delete", history_file, d], check=True)

git_dirs = run(["fd", "--glob", "--hidden", ".git", project_root_str], capture_output=True, check=True).stdout

project_dirs = re.compile(b"/[^/]+/?\n").sub(b"\n", git_dirs)


frecently_out = run(
    [frecently, "view", history_file, "--augment"],
    capture_output=True,
    check=True,
    input=project_dirs,
)

dir_table: list[tuple[Path, str]] = []
max_display_dir_len = 0
project_root: Path = Path(project_root_str)
home: Path = Path.home()

for path_str in frecently_out.stdout.decode().splitlines():
    badges = []
    path = Path(path_str)
    if (path / ".git").exists():
        badges.append(" ")
    if (path / "flake.nix").exists():
        badges.append(" ")
    if (path / "Cargo.toml").exists():
        badges.append(" ")
    if (path / "pyproject.toml").exists():
        badges.append(" ")

    if path.is_relative_to(project_root) and path != project_root:
        display = str(path.relative_to(project_root))
    elif path.is_relative_to(home):
        display = str(path).replace(str(home), "~")
    else:
        display = str(path)

    max_display_dir_len = max(max_display_dir_len, len(display))

    dir_table.append((path, f"{''.join(badges)}\t{display}"))

fuzzel_out = run(
    [
        "fuzzel",
        "--dmenu",
        "--no-sort",
        "--index",
        "--width",
        str(max_display_dir_len + 8),
        "--prompt",
        " ",
    ],
    check=True,
    capture_output=True,
    input="\n".join(disp for _, disp in dir_table).encode(),
)

out = dir_table[int(fuzzel_out.stdout)][0]

run([frecently, "bump", history_file, out], check=True)

print(out)
