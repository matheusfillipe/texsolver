import re

import streamlit as st
import sympy as sp
from sympy import latex, solve
from sympy.parsing.latex import parse_latex
from sympy.plotting import plot as spplot

SYMPY_SOLVERS = ["simplify", "expand", "doit"]  # , "doit_numerically"]
st.title = "TexSolver"
st.set_page_config(page_title="TexSolver")
hide_streamlit_style = """
<style>
#MainMenu {visibility: hidden;}
footer {visibility: hidden;}
</style>

"""
st.markdown(hide_streamlit_style, unsafe_allow_html=True)
st.set_option('deprecation.showPyplotGlobalUse', False)

def call(obj, method):
    return getattr(obj, method)()


def printex(tex):
    st.latex(f"{tex}")
    f"""
    ```latex
    {tex}
    ```
    """


def plot(se, sse):
    if se.endswith("="):
        le = se + sse
    else:
        le = se + "=" + sse
    printex(le)


def solveEq(sol, sym):
    if len(sol) > 0:
        if len(sol) == 1:
            printex(sym + r"=" + latex(sol[0]))
        else:
            for i, s in enumerate(sol):
                printex(sym + r"_" + str(i) + "=" + latex(s))


def processEquality(se):
    # st.write("Solving Equality")
    symbs = list(se.free_symbols)
    sols = []
    for sym in symbs:
        sol = solve(se, sym)
        solveEq(sol, str(sym))
        if sol:
            sols.append(sol)
    return sols


def processExpression(se):
    # st.write("Solving Expression")
    sols = [latex(call(se, m)) for m in SYMPY_SOLVERS]
    se = latex(se)
    usedSols = [se]
    for sol in sols:
        if not sol in usedSols:
            plot(se, sol)
        usedSols.append(sol)
    return usedSols


def processFunction(eq):
    exp = eq.split("=")
    func = parse_latex(exp[-1])
    symbs = list(func.free_symbols)
    var = []
    key = 100
    value = func
    function = exp[0]
    for symb in symbs:
        v = st.number_input("Evaluate " + str(symb) + " at:", key=key)
        function = function.replace(str(symb), str(v))
        var.append(v)
        value = value.subs(symb, v)
        key += 1
    printex(f"{function}={round(value,4)}")
    if len(symbs) == 1:  ##TODO multi dimentional functions
        p = spplot(func, xlabel=str(symbs[0]), ylabel=exp[0])
        st.pyplot()


def texSolve(eq):
    try:
        se = parse_latex(eq)
        st.latex(f"{eq}")
    except Exception as e:
        """Invalid expression!

        Please use math latex: https://en.wikibooks.org/wiki/LaTeX/Mathematics
        """
        return

    "## Result:"
    "\n"
    funcRE = re.search(r"^\s*[a-zA-Z]\([a-z,]+\)\s*=.*$", eq)
    if funcRE and funcRE.group():  # simple function
        processFunction(eq)

    elif type(se) == sp.core.relational.Equality:
        if len(processEquality(se)) == 0:
            exp = eq.split("=")
            usedSols = []
            for sol in processExpression(parse_latex(exp[0])):
                lsol = latex(sol)
                if not sol in usedSols:
                    usedSols.append(sol)
                    try:
                        processEquality(parse_latex(sol + "=" + "".join(exp[1])))
                    except:
                        pass

    else:
        processExpression(se)


"# TexSolver"

"\n"
"Simply enter with a valid latex formula bellow (without the '$$') and press Ctrl+Enter when you see what you want to get!"

eq = st.text_area("Input:")

if len(eq) > 0:
    try:
        texSolve(eq)
    except Exception as e:
        e
