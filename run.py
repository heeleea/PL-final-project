from parser import Parser
from context import Context
from lexical_analysis import LexicalAnalysis
from semantical_analysis import SemanticalAnalysis, Number
from symbol_table import SymbolTable


global_symbol_table = SymbolTable()
global_symbol_table.set("TRUE", Number.true)
global_symbol_table.set("FALSE", Number.false)
global_symbol_table.set("NULL", Number.null)


def run(input, file_name):
    lexer = LexicalAnalysis(input, file_name)
    tokens, error = lexer.create_token_stream()
    if error:
        return None, error

    parse = Parser(tokens)
    ast = parse.create_ats()
    if ast.error:
        return None, ast.error

    semantical_analysis = SemanticalAnalysis()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = semantical_analysis.transverse(ast.node, context)

    return result.value, result.error
