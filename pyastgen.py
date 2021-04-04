import ast
import json
from ast2json import ast2json
import argparse

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input", type=str,
                    help="the file to parse")
    parser.add_argument("-o", "--output", type=str,
                    help="the file to write the json ast to, stdout by default")
    args = parser.parse_args();
    print(args);

    code = ast2json(ast.parse(open(args.input).read()))
    ast = json.dumps(code, indent=4);
    if args.output is not None:
        f = open(args.output, "w");
        f.write(ast);
        f.close();
    else:
        print(ast)
