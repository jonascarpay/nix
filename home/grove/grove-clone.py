#!/usr/bin/env python3
import argparse
import re
import subprocess
import sys
import tempfile
from pathlib import Path


def run_command(cmd: list[str]) -> None:
    try:
        subprocess.run(cmd, check=True, text=True, capture_output=False)
    except subprocess.CalledProcessError as e:
        print(e.stderr, file=sys.stderr)
        sys.exit(e.returncode)


def url_to_dirname(url: str) -> str:
    mat = re.search(r"([^/:]+?)(?:\.git)?$", url)
    if mat is None:
        raise ValueError(f"Couldn't parse URL {url}")  # noqa: EM102, TRY003
    return mat.group(1)


def clone(
    url: str,
    dirname: Path,
    extra_args: list[str],
) -> None:
    with tempfile.TemporaryDirectory() as tmp_str:
        print(f"Setting up grove in {dirname}")

        tmp_root = Path(tmp_str)
        tmp_git_dir = tmp_root / ".git"

        run_command(["git", "clone", *extra_args, url, str(tmp_root)])
        run_command(["git", f"--git-dir={tmp_git_dir}", "config", "--bool", "core.bare", "true"])

        print("Clone successful, moving into place")
        dirname.mkdir(parents=True, exist_ok=False)
        final_git_dir = dirname / ".git"
        tmp_git_dir.rename(final_git_dir)


def main() -> None:
    """
    Main execution function.
    """
    parser = argparse.ArgumentParser(
        description="Clone a repository into a new grove.",
        epilog="Extra arguments are passed directly to 'git clone'.",
    )
    parser.add_argument("url", help="The URL of the git repository to clone.")
    parser.add_argument("dir", nargs="?", help="The optional name for the destination directory.")
    args, extra_clone_args = parser.parse_known_args()

    dirname = Path(args.dir if args.dir is not None else url_to_dirname(args.url))

    clone(args.url, dirname, extra_clone_args)


if __name__ == "__main__":
    main()
