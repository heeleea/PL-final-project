from entities.context import Context
from entities.symbol_table import SymbolTable
from error import CostumedRunTimeError
from validators.run_time_validator import RuntimeValidator
from entities.value import Value


class BaseFunction(Value):
    def __init__(self, name):
        super().__init__()
        self.name = name or "<anonymous>"

    def generate_new_context(self):
        new_context = Context(self.name, self.context, self.start_position)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        return new_context

    def check_args(self, arg_names, args):
        validator = RuntimeValidator()

        if len(args) > len(arg_names):
            error = CostumedRunTimeError(f"{len(args) - len(arg_names)} too many args passed into '{self.name}'", self.start_position, self.end_position, self.context)
            return validator.failure(error)

        if len(args) < len(arg_names):
            error = CostumedRunTimeError(f"{len(arg_names) - len(args)} too few args passed into '{self.name}'", self.start_position, self.end_position, self.context)
            return validator.failure(error)

        return validator.success(None)

    @staticmethod
    def populate_args(arg_names, args, execution_context):
        for i in range(len(args)):
            arg_name = arg_names[i]
            arg_value = args[i]
            arg_value.set_context(execution_context)
            execution_context.symbol_table.set(arg_name, arg_value)

    def check_and_populate_args(self, arg_names, args, execution_context):
        validator = RuntimeValidator()
        validator.register(self.check_args(arg_names, args))

        if validator.error:
            return validator

        self.populate_args(arg_names, args, execution_context)

        return validator.success(None)
