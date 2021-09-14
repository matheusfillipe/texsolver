#!/usr/bin/env python3
import sys
import traceback

from texsuggest import solve


def main():
    if len(sys.argv) == 0:
        sys.stderr("Pass a latex formated string to evaluate")
        return
    try:
        for r in solve(" ".join(sys.argv[1:])):
            print(r)
    except Exception as e:
        traceback.print_tb(e.__traceback__)
        sys.stderr.write(str(e))


if __name__ == "__main__":
    main()
