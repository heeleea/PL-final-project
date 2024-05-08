from error import CostumedRunTimeError
from entities.basic_position import BasicPosition
from validators.run_time_validator import RuntimeValidator


class Value(BasicPosition):
    def __init__(self):
        super().__init__()
        self.context = None
        self.set_position()
        self.set_context()

    def set_position(self, start_position=None, end_position=None):
        self.start_position = start_position
        self.end_position = end_position
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, other):
        return None, self.illegal_operation(other)

    def subbed_by(self, other):
        return None, self.illegal_operation(other)

    def multiplied_by(self, other):
        return None, self.illegal_operation(other)

    def divided_by(self, other):
        return None, self.illegal_operation(other)

    def powered_by(self, other):
        return None, self.illegal_operation(other)

    def equals(self, other):
        return None, self.illegal_operation(other)

    def not_equals(self, other):
        return None, self.illegal_operation(other)

    def greater_than(self, other):
        return None, self.illegal_operation(other)

    def greater_than_equals(self, other):
        return None, self.illegal_operation(other)

    def logical_or(self, other):
        return None, self.illegal_operation(other)

    def logical_not(self):
        return None, self.illegal_operation()

    def execute(self, args):
        validator = RuntimeValidator()
        validator.failure(self.illegal_operation())
        return validator

    def get_copy(self):
        raise Exception('No copy method defined')

    @staticmethod
    def is_true():
        return False

    def illegal_operation(self, other=None):
        if not other:
            other = self

        error = CostumedRunTimeError('Illegal operation', self.start_position, other.end_position, self.context)
        return error
