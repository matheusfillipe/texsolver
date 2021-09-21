#!/usr/bin/env python3
import sys
import traceback

from texsuggest import solve


def main():
    if len(sys.argv) == 0:
        sys.stderr.write("Pass a latex formated string to evaluate")
        return
    if sys.argv[1] == "-wak":  # Wolfram alpha backend
        try:
            from wolframalpha import Client
        except ModuleNotFoundError:
            sys.stderr.write("Run pip install wolframalpha")
            return
        if len(sys.argv) < 4:
            sys.stderr.write("Pass in the wolframalpha key after -wak and the query after it")
            return
        client = Client(sys.argv[2])
        res = client.query(" ".join(sys.argv[3:]))
        for pod in res.pods:
            for sub in pod.subpods:
                print(sub.plaintext)
        return
    try:
        for r in solve(" ".join(sys.argv[1:])):
            print(r)
    except Exception as e:
        traceback.print_tb(e.__traceback__)
        sys.stderr.write(str(e))


if __name__ == "__main__":
    main()
