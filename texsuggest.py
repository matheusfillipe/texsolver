import re
import tempfile

import sympy as sp
from sympy import latex, solve
from sympy.parsing.latex import LaTeXParsingError, parse_latex
from sympy.plotting import plot as spplot

SYMPY_SOLVERS = ["simplify", "expand", "doit"]

def process_function(eq):
    """process_function.

    :param eq: str
        Function expression
    :returns: str
        Temporary path for image or None
    """
    exp = eq.split("=")
    func = parse_latex(exp[-1])
    symbs = list(func.free_symbols)
    if len(symbs) == 1:  # TODO multi dimentional functions
        p = spplot(func, xlabel=str(symbs[0]), ylabel=exp[0])
        with tempfile.TemporaryDirectory() as tmpdir:
            p.save(f"{tempdir}/plot.png")


def solver(latex_string):
    """Takes a math string in latex format and generates a list of completions.

    :param latex_string: str
        A latex notation math expression with or without equal sign
    :returns: list
        A list of strings of possible completions for the input expression
    :raises: LaTeXParsingError
    """
    sympy_expression = parse_latex(latex_string)
    function = re.search(r"^\s*[a-zA-Z]\([a-z,]+\)\s*=.*$", eq)
    if function and function.group():  # function definition
        process_function(eq)

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
