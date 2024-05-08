from error import CostumedRunTimeError
from constans.token_names import ComparisonOperator
from entities.value import Value


class Number(Value):

    def __init__(self, value):
        super().__init__()
        self.value = value

    def added_to(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value + new_number.value)
            number.set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def subbed_by(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value - new_number.value)
            number.set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def multiplied_by(self, new_number):
        if isinstance(new_number, Number):
            number = Number(self.value * new_number.value)
            number.set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def divided_by(self, new_number):
        if isinstance(new_number, Number):
            if new_number.value == 0:
                return None, CostumedRunTimeError('Division By Zero', new_number.start_position, new_number.end_position, self.context)

            number = Number(self.value / new_number.value)
            number.set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def powered_by(self, power):
        if isinstance(power, Number):
            number = Number(self.value ** power.value).set_context(self.context)
            return number, None

        return None, Value.illegal_operation(self.start_position, power.end_position)

    def get_comparison(self, new_number, operator):
        operations = {
            '==': lambda x, y: int(x == y),
            '!=': lambda x, y: int(x != y),
            '<': lambda x, y: int(x < y),
            '>': lambda x, y: int(x > y),
            '<=': lambda x, y: int(x <= y),
            '>=': lambda x, y: int(x >= y),
            'AND': lambda x, y: int(x and y),
            'OR': lambda x, y: int(x or y)
        }

        if isinstance(new_number, Number):
            operation = operations.get(operator) or operator
            if operation:
                result = operation(self.value, new_number.value)
                number = Number(result)
                number.set_context(self.context)
                return number, None

        return None, Value.illegal_operation(self.start_position, new_number.end_position)

    def equals(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.COMPARISON.value)

    def not_equals(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.NOT_EQUALS.value)

    def less_than(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.LESS_THAN.value)

    def greater_than(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.GREATER_THAN.value)

    def less_than_equals(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.LESS_THAN_EQUALS.value)

    def greater_than_equals(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.GREATER_THAN_EQUALS.value)

    def logical_and(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.AND.value)

    def logical_or(self, new_number):
        return self.get_comparison(new_number, ComparisonOperator.OR.value)

    def logical_not(self):
        comparison_result = 1 if self.value == 0 else 0
        number = Number(comparison_result)
        number.set_context(self.context)
        return number, None

    def get_copy(self):
        copy = Number(self.value)
        copy.set_position(self.start_position, self.end_position)
        copy.set_context(self.context)
        return copy

    def is_true(self):
        return self.value != 0

    def __repr__(self):
        return str(self.value)
