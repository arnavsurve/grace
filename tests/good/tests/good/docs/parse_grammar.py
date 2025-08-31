import json

from lark import Lark, Transformer


class ToJSON(Transformer):
    def __default__(self, data, children, meta):
        return {data: children}


parser = Lark.open("grammar.lark", parser="lalr", start="program")

with open("../examples/hello.grc", "r") as f:
    grace_src = f.read()

tree = parser.parse(grace_src)
ast = ToJSON().transform(tree)

print(json.dumps(ast, indent=2))
