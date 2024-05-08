from entities.base_function import BaseFunction
from validators.run_time_validator import RuntimeValidator
from semantical_analysis import SemanticalAnalysis
from built_ins.variables import NumberRunner
#
#
# class Function(BaseFunction):
#     def __init__(self, name, arg_names, body, should_return_null):
#         super().__init__(name)
#         self.arg_names = arg_names
#         self.body = body
#         self.should_return_null = should_return_null
#
#     def execute(self, args):
#         validator = RuntimeValidator()
#         semantical_analysis = SemanticalAnalysis()
#         execution_context = self.generate_new_context()
#
#         validator.register(self.check_and_populate_args(self.arg_names, args, execution_context))
#         if validator.error:
#             return validator
#
#         value = validator.register(semantical_analysis.transverse(self.body, execution_context))
#         if validator.error:
#             return validator
#
#         result = NumberRunner.null if self.should_return_null else value
#         return validator.success(result)
#
#     def get_copy(self):
#         copy = Function(self.name, self.body, self.arg_names, self.should_return_null)
#         copy.set_context(self.context)
#         copy.set_position(self.start_position, self.end_position)
#         return copy
#
#     def __repr__(self):
#         return f"<function {self.name}>"
