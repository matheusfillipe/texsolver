## TexSolver

A simple streamlit test webapp that reads a latex input and gives possible results from it.
The goal is to automate a bit of the math writing with latex. It uses sympy for the solving of various expressions types.

### DEMO
`demo.py` is a streamlit proof of concept for this idea. 

You will need to install [streamlit](https://streamlit.io/) and run it with:
```sh
streamlit run demo.py
```

`api.py` is a simple flask api with a single endpoint.

`cli.py` is cli that takes a latex math expression as input and returns possible completions as lines.

## Examples
![image](https://raw.githubusercontent.com/matheusfillipe/texsovler/main/images/1.jpg)
![image](https://raw.githubusercontent.com/matheusfillipe/texsovler/main/images/2.jpg)
![image](https://raw.githubusercontent.com/matheusfillipe/texsovler/main/images/3.jpg)
