from parser import Parser
from context import Context
from lexical_analysis import LexicalAnalysis
from semantical_analysis import SemanticalAnalysis, Number, BuiltInFunctions
from symbol_table import SymbolTable


global_symbol_table = SymbolTable()
global_symbol_table.set("TRUE", Number.true)
global_symbol_table.set("FALSE", Number.false)
global_symbol_table.set("NULL", Number.null)
global_symbol_table.set("MATH_PI", Number.math_PI) #not sure its relevant?
global_symbol_table.set("PRINT", BuiltInFunctions.print)
global_symbol_table.set("PRINT_RETURN", BuiltInFunctions.print_return)
global_symbol_table.set("INPUT", BuiltInFunctions.input)
global_symbol_table.set("INPUT_INT", BuiltInFunctions.input_int)
global_symbol_table.set("CLEAR", BuiltInFunctions.clear)
global_symbol_table.set("CLS", BuiltInFunctions.clear)
global_symbol_table.set("IS_NUM", BuiltInFunctions.is_number)
global_symbol_table.set("IS_STR", BuiltInFunctions.is_string)
global_symbol_table.set("IS_LIST", BuiltInFunctions.is_list)
global_symbol_table.set("IS_FUN", BuiltInFunctions.is_function)
global_symbol_table.set("APPEND", BuiltInFunctions.append)
global_symbol_table.set("POP", BuiltInFunctions.pop)
global_symbol_table.set("EXTEND", BuiltInFunctions.extend)


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
