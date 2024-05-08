import os

from entities.base_function import BaseFunction
from validators.run_time_validator import RuntimeValidator
from built_ins.variables import NumberRunner
from error import CostumedRunTimeError
from entities.string import String
from entities.number import Number
from entities.list import List


def define_args(*arg_names):
    def decorator(func):
        func.arg_names = arg_names
        return func
    return decorator


class BuiltInFunctions(BaseFunction):
    def __init__(self, name):
        super().__init__(name)

    def execute(self, args):
        validator = RuntimeValidator()
        execution_context = self.generate_new_context()

        method_name = f'execute_{self.name}'
        method = getattr(self, method_name, self.method_not_found)

        validator.register(self.check_and_populate_args(method.arg_names, args, execution_context))
        if validator.error:
            return validator

        return_value = validator.register(method(execution_context))
        if validator.error:
            return validator

        return validator.success(return_value)

    def get_copy(self):
        copy = BuiltInFunctions(self.name)
        copy.set_context(self.context)
        copy.set_position(self.start_position, self.end_position)
        return copy

    def __repr__(self):
        return f"<built-in function {self.name}>"

    @define_args('value')
    def execute_print(self, execution_context):
        validator = RuntimeValidator()
        value = execution_context.symbol_table.get('value')
        print(str(value))
        return validator.success(NumberRunner.null)

    @define_args('value')
    def execute_print_return(self, execution_context):
        validator = RuntimeValidator()
        value = execution_context.symbol_table.get('value')
        return validator.success(String(str(value)))

    @staticmethod
    @define_args()
    def execute_input(execution_context):
        validator = RuntimeValidator()
        text = input()
        return validator.success(String(text))

    @define_args()
    def execute_input_int(self, execution_context):
        validator = RuntimeValidator()
        no_number = True
        number = None

        while no_number:
            text = input()

            try:
                number = int(text)
                no_number = False

            except ValueError:
                print(f"'{text}' must be an integer. Try Again!")

        return validator.success(Number(number))

    @define_args()
    def execute_clear(self, execution_context):
        validator = RuntimeValidator()
        os_name = os.name

        if os_name == "nt":
            os.system('cls')  # Windows
        else:
            os.system('clear')  # For Unix-like

        return validator.success(NumberRunner.null)

    @define_args('value')
    def execute_is_number(self, execution_context):
        validator = RuntimeValidator()
        value = execution_context.symbol_table.get('value')
        is_number = isinstance(value, Number)
        result = NumberRunner.true if is_number else NumberRunner.false
        return validator.success(result)

    @define_args('value')
    def execute_is_string(self, execution_context):
        validator = RuntimeValidator()
        value = execution_context.symbol_table.get('value')
        is_string = isinstance(value, String)
        result = NumberRunner.true if is_string else NumberRunner.false
        return validator.success(result)

    @define_args('value')
    def execute_is_list(self, execution_context):
        validator = RuntimeValidator()
        value = execution_context.symbol_table.get('value')
        is_list = isinstance(value, List)
        result = NumberRunner.true if is_list else NumberRunner.false
        return validator.success(result)

    @define_args('value')
    def execute_is_function(self, execution_context):
        validator = RuntimeValidator()
        value = execution_context.symbol_table.get('value')
        is_function = isinstance(value, BaseFunction)
        result = NumberRunner.true if is_function else NumberRunner.false
        return validator.success(result)

    @define_args('list', 'value')
    def execute_append(self, execution_context):
        validator = RuntimeValidator()

        list_ = execution_context.symbol_table.get('list')
        value = execution_context.symbol_table.get('value')

        if not isinstance(list_, List):
            error = CostumedRunTimeError("First argument must be a list", self.start_position, self.end_position, execution_context)
            return validator.failure(error)

        list_.elements.append(value)
        return validator.success(NumberRunner.null)

    @define_args('list', 'index')
    def execute_pop(self, execution_context):
        validator = RuntimeValidator()

        list_ = execution_context.symbol_table.get('list')
        index = execution_context.symbol_table.get('index')

        if not isinstance(list_, List):
            error = CostumedRunTimeError("First argument must be a list", self.start_position, self.end_position, execution_context)
            return validator.failure(error)

        if not isinstance(index, Number):
            error = CostumedRunTimeError("Second argument must be a number", self.start_position, self.end_position, execution_context)
            return validator.failure(error)

        try:
            element = list_.elements.pop(index.value)
        except IndexError:
            error = CostumedRunTimeError("Index is out of bound", self.start_position, self.end_position, execution_context)
            return validator.failure(error)

        return validator.success(element)

    @define_args('list_a', 'list_b')
    def execute_extend(self, execution_context):
        validator = RuntimeValidator()

        list_a = execution_context.symbol_table.get('list_a')
        list_b = execution_context.symbol_table.get('list_b')

        if not isinstance(list_a, List):
            error = CostumedRunTimeError("First argument must be a list", self.start_position, self.end_position, execution_context)
            return validator.failure(error)

        if not isinstance(list_b, List):
            error = CostumedRunTimeError("Second argument must be a list", self.start_position, self.end_position, execution_context)
            return validator.failure(error)

        list_a.elements.extend(list_b.elements)
        return validator.success(NumberRunner.null)

    def method_not_found(self, node, context):
        raise Exception(f"Method {self.name} is not defined")
