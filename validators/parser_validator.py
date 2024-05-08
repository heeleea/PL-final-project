class ParserValidator:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0
        self.to_reverse_count = 0

    def register(self, result):
        self.advance_count += result.advance_count

        if result.error:
            self.error = result.error

        return result.node

    def try_register(self, validator):
        if validator.error:
            self.to_reverse_count = validator.advance_count
            return None

        return self.register(validator)

    def register_advancement(self):
        self.advance_count += 1

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error

        return self
