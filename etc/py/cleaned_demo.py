x = 1
"4: not comment, even if ignored by compiler"
'5: not comment, even if ignored by compiler'
x = """6: not comment, because used as string value""""""
123"""
x = \
\
"'''8: not comment''' because is part of string"
x = "9: # not comment because is part of string"
