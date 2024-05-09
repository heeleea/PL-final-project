from entities.value import Value
from entities.number import Number


class List(Value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements

    def added_to(self, new_element):
        updated_list = self.get_copy()
        updated_list.elements.append(new_element)

        return updated_list, None

    def subbed_by(self, index):
        if isinstance(index, Number):
            updated_list = self.get_copy()
            try:
                updated_list.elements.pop(index.value)
                return updated_list, None
            except:
                error_message = 'Elements at this index could not be removed from the list because index is out of bounds'
                error = RuntimeError(index.start_position,
                                     index.end_position,
                                     error_message,
                                     self.context)
                return None, error

        else:
            return None, Value.illegal_operation(self, index)

    def multiplied_by(self, new_list):
        if isinstance(new_list, List):
            updated_list = self.get_copy()
            updated_list.elements.extend(new_list.elements)
            return updated_list, None

        else:
            return None, Value.illegal_operation(self, new_list)

    def divided_by(self, index):
        if isinstance(index, Number):
            try:
                return self.elements[index.value], None

            except IndexError:
                error_message = 'Elements at this index could not be retrieved from the list because index is out of bounds'
                error = RuntimeError(index.start_position,
                                     index.end_position,
                                     error_message,
                                     self.context)
                return None, error

        else:
            return None, Value.illegal_operation(self, index)

    def get_copy(self):
        copied_list = List(self.elements)
        copied_list.set_position((self.start_position, self.end_position))
        copied_list.set_context(self.context)
        return copied_list

    def __str__(self):
        return ', '.join(map(str, self.elements))

    def __repr__(self):
        return f"[{', '.join(map(str, self.elements))}]"
