from print_utils import print_error_with_arrows


class Error:
    def __init__(self, name, details, start_position, end_position):
        self.name = name
        self.details = details
        self.start_position = start_position
        self.end_position = end_position

    def to_string(self):
        result = f"Error: {self.name}: {self.details}\n"
        result += f"File: {self.start_position.file_name}, Line {self.start_position.line + 1}"
        add_arrows = print_error_with_arrows(self.start_position.file_text, self.start_position, self.end_position)
        result += f"\n\n{add_arrows}"
        return result


class IllegalCharError(Error):
    def __init__(self, details, start_position, end_position):
        super().__init__(name='Illegal Character',
                         details=details,
                         start_position=start_position,
                         end_position=end_position)


class ExpectedCharError(Error):
    def __init__(self, details, start_position, end_position):
        super().__init__(name='Expected Character',
                         details=details,
                         start_position=start_position,
                         end_position=end_position)


class InvalidSyntaxError(Error):
    def __init__(self, details, start_position, end_position):
        super().__init__(name='Invalid Syntax',
                         details=details,
                         start_position=start_position,
                         end_position=end_position)


class CostumedRunTimeError(Error):
    def __init__(self, details, start_position, end_position, context):
        super().__init__(name='Runtime Error',
                         details=details,
                         start_position=start_position,
                         end_position=end_position)
        self.context = context

    def to_string(self):
        result = self.generate_traceback()
        result += f"  Error: {self.name}: {self.details}\n"
        add_arrows = print_error_with_arrows(self.start_position.file_text, self.start_position, self.end_position)
        result += f"\n\n{add_arrows}"
        return result

    def generate_traceback(self):
        result = ''
        position = self.start_position
        context = self.context

        while context:
            result = f"  File {self.start_position.file_name}, line {str(position.line + 1)}, in {context.display_name}\n{result}"
            position = context.parent_entry_position
            context = context.parent

        return f"Traceback (most recent call last):\n{result}"


def error_message_generator(signs):
    """ Generated an error message stating that one provided signs was expected."""
    if len(signs) == 1:
        formatted_signs = f"'{signs[0]}'"
    else:
        formatted_signs = ', '.join(f"'{sign}'" for sign in signs[:-1]) + f" or '{signs[-1]}'"
    return f"Expected {formatted_signs}."
