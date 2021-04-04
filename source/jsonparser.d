module jsonparser;

import std.array : array, empty, front;
import std.exception : enforce;
import std.format;
import std.algorithm.searching : canFind, endsWith;
import std.algorithm.iteration : filter, uniq;
import std.algorithm.sorting : sort;
import std.json;
import std.conv;
import std.range : ElementType;
import std.stdio;
import std.meta;
import std.traits;
import std.typecons;

import aggregateprinter;

import ast;

string[] filterClassMembers(string[] mems) {
	return mems.filter!(mem =>
				!canFind(["toString", "toHash", "opCmp", "opEquals"
					, "Monitor", "factory"], mem))
			.array
			.sort
			.uniq
			.array;
}

string prepareMemName(string mem) {
	return mem.endsWith("_")
		? mem[0 .. $ - 1]
		: mem;
}

template allMembersImpl(T) {
	static if(is(BaseClassesTuple!T == AliasSeq!(Object))) {
		enum allMembersImpl = [__traits(allMembers, T)];
	} else {
		enum allMembersImpl = [__traits(allMembers, T)] 
			~ allMembersImpl!(BaseClassesTuple!(T)[0]);
	}
}

template allMembers(T) {
	enum allMembers = filterClassMembers(allMembersImpl!T);
}

T fromJson(T, string mem)(const(JSONValue) jv) {
	//writefln("%s %s", T.stringof, mem);
	enforce(jv.type == JSONType.object
		, format("Trying to parse an '%s', expected object got '%s'"
			, T.stringof, jv.toPrettyString()));
	const(JSONValue)* val = mem in jv;
	static if(is(T : Nullable!F, F...)) {
		return val is null || val.type == JSONType.null_
			? T.init
			: nullable(fromJson!(F[0], mem)(jv));
	} else {
		enforce(val !is null, format("Counldn't find '%s' of type '%s' in '%s'"
				, mem, T.stringof, jv.toPrettyString()));

		static if(is(T == enum)) {
			return parse!T(*val);
		} else static if(isIntegral!T) {
			return (*val).get!long();
		} else static if(isSomeString!T) {
			enforce(val.type == JSONType.string, format(
					"Expected string got '%s'", val.toPrettyString()));
			return (*val).get!string();
		} else static if(is(T == bool)) {
			return (*val).get!bool();
		} else static if(isArray!T) {
			enforce((*val).type == JSONType.array, format(
					"Expected an array got '%s'", (*val).toPrettyString()));
			alias ET = ElementType!T;

			T arr;
			foreach(it; (*val).arrayNoRef()) {
				arr ~= parse!ET(it);
			}
			return arr;
		} else static if(is(T == JSONValue)) {
			return *val;
		} else static if(is(T == Expr)) {
			return parseExpr(*val);
		} else static if(is(T == class)) {
			writeln("Parse class");
			return parse!T(*val);
		} else {
			static assert(false, T.stringof);
		}
	}
}

T parse(T)(const(JSONValue) jv) if(is(T == enum)) {
	enforce(jv.type == JSONType.object
		, format("Trying to parse an '%s', expected object got '%s'"
			, T.stringof, jv.toPrettyString()));
	const(JSONValue)* type = "_type" in jv;
	enforce(type !is null, format("Parsing '%s' no element named '_type' in '%s'"
		, T.stringof, jv.toPrettyString()));
	enforce(type.type == JSONType.string, format("Expecting string got '%s'"
		, type.toPrettyString()));

	return to!T(type.get!string());
}

T parse(T)(const(JSONValue) jv) if(is(T == class)) {
	enforce(jv.type == JSONType.object
		, format("Trying to parse an '%s', expected object got '%s'"
			, T.stringof, jv.toPrettyString()));

	T ret = new T();
	static foreach(string memP; allMembers!T) {{
		enum mem = prepareMemName(memP);
		alias RT = typeof(__traits(getMember, ret, memP));
		__traits(getMember, ret, memP) = fromJson!(RT, mem)(jv);
	}}

	return ret;
}

template isExpr(T) {
	enum isExpr = is(T : Expr);
}

Expr parseExpr(JSONValue jv) {
	enforce(jv.type == JSONType.object
		, format("Trying to parse an 'Expr', expected object got '%s'"
			, jv.toPrettyString()));

	const(JSONValue)* value = "value" in jv;
	//enforce(value !is null, "value not found");
	//enforce(value.type == JSONType.object, format("value not a object but '%s'"
	//		, value.toPrettyString()));

	if(value is null || value.type != JSONType.object) {
		return parseExprValue(jv);
	} else {
		auto ret = new Expr();
		ret.value = nullable(parseExprValue(*value));
		return ret;
	}
}

Expr parseExprValue(JSONValue jv) {
	enforce(jv.type == JSONType.object
		, format("Trying to parse an 'Expr', expected object got '%s'"
			, jv.toPrettyString()));

	const(JSONValue)* type = "_type" in jv;
	enforce(type !is null, "_type not found");
	enforce(type.type == JSONType.string, format("_type not a string but '%s'"
			, type.toPrettyString()));
	string tName = type.get!string();
	if(type !is null) {
		foreach(exp; __traits(allMembers, ast)) {
			static if(is(__traits(getMember, ast, exp) == class)
					&& anySatisfy!(isExpr, BaseClassesTuple!(__traits(getMember, ast, exp))))
			{{
				if(tName == __traits(getMember, ast, exp).stringof) {
					return parse!(__traits(getMember, ast, exp))(jv);
				}
			}}
		}
	}

	return null;
}

unittest {
	JSONValue jv = parseJSON(
`{
	"_type": "Expr",
	"col_offset": 8,
	"end_col_offset": 18,
	"end_lineno": 21,
	"lineno": 21,
	"value": {
		"_type": "Call",
		"args": [
			{
				"_type": "Name",
				"col_offset": 14,
				"ctx": {
					"_type": "Load"
				},
				"end_col_offset": 17,
				"end_lineno": 21,
				"id": "ast",
				"lineno": 21
			}
		],
		"col_offset": 8,
		"end_col_offset": 18,
		"end_lineno": 21,
		"func": {
			"_type": "Name",
			"col_offset": 8,
			"ctx": {
				"_type": "Load"
			},
			"end_col_offset": 13,
			"end_lineno": 21,
			"id": "print",
			"lineno": 21
		},
		"keywords": [],
		"lineno": 21
	}
}`);
	Expr e = parseExpr(jv);
	assert(e !is null);
}

unittest {
	JSONValue jv = parseJSON(
`
        {
            "_type": "Import",
            "col_offset": 0,
            "end_col_offset": 10,
            "end_lineno": 1,
            "lineno": 1,
            "names": [
                {
                    "_type": "alias",
                    "asname": null,
                    "name": "ast"
                }
            ]
        }`);
	auto e = parse!Import(jv);
}

unittest {
	import std.file : readText;
	JSONValue jv = parseJSON(readText("out.json"));
	Module e = parse!Module(jv);
	assert(e !is null);
}



unittest {
	JSONValue jv = parseJSON(`{
    "_type": "ImportFrom",
    "col_offset": 0,
    "end_col_offset": 29,
    "end_lineno": 3,
    "level": 0,
    "lineno": 3,
    "module": "ast2json",
    "names": [
        {
            "_type": "alias",
            "asname": null,
            "name": "ast2json"
        }
    ]
}`);
	auto e = parse!ImportFrom(jv);
}

unittest {
	auto jv = parseJSON(`
{
    "_type": "BinOp",
    "col_offset": 6,
    "end_col_offset": 19,
    "end_lineno": 1,
    "left": {
        "_type": "BinOp",
        "col_offset": 6,
        "end_col_offset": 13,
        "end_lineno": 1,
        "left": {
            "_type": "Constant",
            "col_offset": 6,
            "end_col_offset": 8,
            "end_lineno": 1,
            "kind": null,
            "lineno": 1,
            "n": 10,
            "s": 10,
            "value": 10
        },
        "lineno": 1,
        "op": {
            "_type": "Mult"
        },
        "right": {
            "_type": "Constant",
            "col_offset": 11,
            "end_col_offset": 13,
            "end_lineno": 1,
            "kind": null,
            "lineno": 1,
            "n": 20,
            "s": 20,
            "value": 20
        }
    },
    "lineno": 1,
    "op": {
        "_type": "Add"
    },
    "right": {
        "_type": "Constant",
        "col_offset": 16,
        "end_col_offset": 19,
        "end_lineno": 1,
        "kind": null,
        "lineno": 1,
        "n": 5.2,
        "s": 5.2,
        "value": 5.2
    }
}`);
	auto e = parse!BinOp(jv);
	assert(e.left !is null);
	assert(e.right !is null);
	auto r = cast(Constant)e.right;
	assert(r !is null);
	assert(r.value.type == JSONType.float_);
}
