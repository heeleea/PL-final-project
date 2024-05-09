from entities.built_in_functions import BuiltInFunctions


class BuiltInFunctionRunner:
    pass


BuiltInFunctionRunner.print = BuiltInFunctions("print")
BuiltInFunctionRunner.print_return = BuiltInFunctions("print_return")
BuiltInFunctionRunner.input = BuiltInFunctions("input")
BuiltInFunctionRunner.input_int = BuiltInFunctions("input_int")
BuiltInFunctionRunner.clear = BuiltInFunctions("clear")
BuiltInFunctionRunner.is_number = BuiltInFunctions("is_number")
BuiltInFunctionRunner.is_string = BuiltInFunctions("is_string")
BuiltInFunctionRunner.is_list = BuiltInFunctions("is_list")
BuiltInFunctionRunner.is_function = BuiltInFunctions("is_function")
BuiltInFunctionRunner.append = BuiltInFunctions("append")
BuiltInFunctionRunner.pop = BuiltInFunctions("pop")
BuiltInFunctionRunner.extend = BuiltInFunctions("extend")
