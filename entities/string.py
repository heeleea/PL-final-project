from entities.value import Value
from entities.number import Number


class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def added_to(self, additional_string):

        if isinstance(additional_string, String):
            return String(self.value + additional_string.value).set_context(self.context), None

        else:
            return None, Value.illegal_operation(self, additional_string)

    def multiplied_by(self, number):

        if isinstance(number, Number):
            new_string_value = self.value * number.value
            new_string = String(new_string_value)
            new_string.set_context().set_context(self.context)
            return new_string, None

        else:
            return None, Value.illegal_operation(self, number)

    def is_true(self):
        return len(self.value) > 0

    def get_copy(self):
        copy = String(self.value)
        copy.set_context(self.context)
        copy.set_position(self.start_position, self.end_position)
        return copy

    def __str__(self):
        return self.value

    def __repr__(self):
        return f'"{self.value}"'
