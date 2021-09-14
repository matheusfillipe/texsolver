import random
import re
import string
import tempfile

import sympy as sp
from sympy import latex
from sympy.parsing.latex import LaTeXParsingError, parse_latex
from sympy.plotting import plot as spplot

SYMPY_SOLVERS = ["simplify", "expand", "doit"]

def solution2tex(sol, sym):
    if len(sol) > 0:
        if len(sol) == 1:
            return [sym + r" = " + latex(sol[0])]
        else:
            return [sym + r"_" + str(i) + " = " + latex(s) for i, s in enumerate(sol)]

def formated_result(se, sse):
    if se.endswith("="):
        le = se + sse
    else:
        le = se + " = " + sse
    return le

def process_expression(sympy_expression):
    """process_equality.

    :param sympy_expression: sympy.expression
    :returns: list of results
    """
    def call(obj, method):
        return getattr(obj, method)()

    sols = [latex(call(sympy_expression, m)) for m in SYMPY_SOLVERS]
    sympy_expression = latex(sympy_expression)
    usedSols = [sympy_expression]
    solutions = []
    for sol in sols:
        if not sol in usedSols:
            solutions.append(formated_result(sympy_expression, sol))
        usedSols.append(sol)
    return solutions

def process_equality(sympy_expression):
    """process_equality.

    :param sympy_expression: sympy.expression with an equal sign on it
    :returns: list of results
    """
    symbols = list(sympy_expression.free_symbols)
    solutions = []
    for symbol in symbols:
        solution = sp.solve(sympy_expression, symbol)
        if solution:
            solutions += solution2tex(solution, str(symbol))
    return solutions

def plot_function(eq):
    """plot_function.

    :param eq: str
        Function expression
    :returns: str
        Temporary path for image or None
    """
    exp = eq.split("=")
    func = parse_latex(exp[-1])
    symbs = list(func.free_symbols)
    if len(symbs) == 1:  # TODO multi dimentional functions
        p = spplot(func, show=False, xlabel=str(symbs[0]), ylabel=exp[0])
        path = f"{tempfile.gettempdir()}/{''.join(random.choice(string.ascii_lowercase) for i in range(10))}.png"
        p.save(path)
        return [path]


def solve(latex_string):
    """Takes a math string in latex format and generates a list of completions.

    :param latex_string: str
        A latex notation math expression with or without equal sign
    :returns: list
        A list of strings of possible completions for the input expression
    :raises: LaTeXParsingError
    """
    # TODO account that all of these can return results
    sympy_expression = parse_latex(latex_string)
    function = re.search(r"^\s*[a-zA-Z]\([a-z,]+\)\s*=.*$", latex_string)
    if function and function.group():  # function definition
        return plot_function(latex_string)

    sols = []
    if type(sympy_expression) == sp.core.relational.Equality:
        equality_result = process_equality(sympy_expression)
        if equality_result:
            return equality_result
        exp = latex_string.split("=")
        usedSols = []
        for sol in process_expression(parse_latex(exp[0])):
            if "=" in sol:
                sol = sol.split("=")[-1].strip()
            if sol not in usedSols:
                try:
                    solv_eq = sol + " = " + exp[1]
                    sols += process_equality(parse_latex(solv_eq))
                    usedSols.append(sol)
                except NotImplementedError:
                    pass

    if sols:
        return sols + process_expression(sympy_expression)
    return process_expression(sympy_expression)
