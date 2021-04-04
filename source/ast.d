import std.typecons : Nullable;

alias Identifier = string;

class Node {
	long lineno; 
	long col_offset; 
	Nullable!long end_lineno; 
	Nullable!long end_col_offset;
}

 // mod = Module(stmt* body, type_ignore* type_ignores)

class Module : Node {
	Stmt[] body_;
	TypeIgnore[] type_ignores;
	this(Stmt[] body_, TypeIgnore[] type_ignores) {
		this.body_ = body_;
		this.type_ignores = type_ignores;
	}
}

//		| Interactive(stmt* body)
class Interactive : Node {
	Stmt[] body_;
	this(Stmt[] body_) {
		this.body_ = body_;
	}
}
//		| Expression(expr body)
class Expression : Node {
	Expr body_;
	this(Expr body_) {
		this.body_ = body_;
	}
}
//		| FunctionType(expr* argtypes, expr returns)
class FunctionType : Node {
	Expr[] argtypes;
	Expr returns;
	this(Expr[] argtypes, Expr returns) {
		this.argtypes = argtypes;
		this.returns = returns;
	}
}

class Stmt : Node {
}
	// stmt = FunctionDef(identifier name, arguments args,
	// 				   stmt* body, expr* decorator_list, expr? returns,
	// 				   string? type_comment)

class FunctionDef : Stmt {
	Identifier name;
	Arguments args;
	Stmt[] body_;
	Expr[] decorator_list;
	Nullable!(Expr) returns;
	Nullable!(string) type_comment;

	this(Identifier name, Arguments args, Stmt[] body_, Expr[] decorator_list,
			Nullable!(Expr) returns, Nullable!(string) type_comment) 
	{
		this.name = name;
		this.args = args;
		this.body_ = body_;
		this.decorator_list = decorator_list;
		this.returns = returns;
		this.type_comment = type_comment;
	}
}

	// | AsyncFunctionDef(identifier name, arguments args,
	//   				 stmt* body, expr* decorator_list, expr? returns,
	//   				 string? type_comment)
class AsyncFunctionDef : Stmt {
	Identifier name;
	Arguments args;
	Stmt[] body_;
	Expr[] decorator_list;
	Nullable!(Expr) returns;
	Nullable!(string) type_comment;

	this(Identifier name, Arguments args, Stmt[] body_, Expr[] decorator_list,
			Nullable!(Expr) returns, Nullable!(string) type_comment) 
	{
		this.name = name;
		this.args = args;
		this.body_ = body_;
		this.decorator_list = decorator_list;
		this.returns = returns;
		this.type_comment = type_comment;
	}
}

	//  | ClassDef(identifier name,
	//	 expr* bases,
	//	 keyword* keywords,
	//	 stmt* body,
	//	 expr* decorator_list)
class ClassDef : Stmt {
	Expr[] bases;
	Keyword[] keywords;
	Stmt[] body_;
	Expr[] decorator_list;

	this(Expr[] bases, Keyword[] keywords, Stmt[] body_, Expr[] decorator_list) {
		this.bases = bases;
		this.keywords = keywords;
		this.body_ = body_;
		this.decorator_list = decorator_list;

	}
}
	// | Return(expr? value)
class Return : Stmt {
	Nullable!Expr value;
	this(Nullable!Expr value) {
		this.value = value;
	}
}

	// | Delete(expr* targets)
class Delete : Stmt {
	Expr[] targets;
	this(Expr[] targets) {
		this.targets = targets;
	}
}
	// | Assign(expr* targets, expr value, string? type_comment)
class Assign : Stmt {
	Expr[] targets;
	Expr value;
	Nullable!string type_comment;
	this(Expr[] targets, Expr value, Nullable!string type_comment) {
		this.targets = targets;
		this.value = value;
		this.type_comment = type_comment;
	}
}
	// | AugAssign(expr target, operator op, expr value)
class AugAssign : Stmt {
	Expr targets;
	Operator op;
	Expr value;
	this(Expr targets, Operator op, Expr value) {
		this.targets = targets;
		this.op = op;
		this.value = value;
	}
}
	// -- 'simple' indicates that we annotate simple name without parens
	// | AnnAssign(expr target, expr annotation, expr? value, int simple)
class AnnAssign : Stmt {
	Expr targets;
	Expr annotation;
	Nullable!Expr value;
	int simple;
	this(Expr targets, Expr annotation, Nullable!Expr value, int simple) {
		this.targets = targets;
		this.annotation = annotation;
		this.value = value;
		this.simple = simple;
	}
}

	//-- use 'orelse' because else is a keyword in target languages
	// | For(expr target, expr iter, stmt* body, stmt* orelse, string? type_comment)
class For : Stmt {
	Expr targets;
	Expr iter;
	Stmt[] body_;
	Stmt[] orelse;
	Nullable!string type_comments;
	this(Expr targets, Expr iter, Stmt[] body_, Stmt[] orelse, Nullable!string type_comments) {
		this.targets = targets;
		this.iter = iter;
		this.body_ = body_;
		this.orelse = orelse;
		this.type_comments = type_comments;
	}
}
	// | AsyncFor(expr target, expr iter, stmt* body, stmt* orelse, string? type_comment)
class AsyncFor : Stmt { 
	Expr targets;
	Expr iter;
	Stmt[] body_;
	Stmt[] orelse;
	Nullable!string type_comments;
	this(Expr targets, Expr iter, Stmt[] body_, Stmt[] orelse, Nullable!string type_comments) {
		this.targets = targets;
		this.iter = iter;
		this.body_ = body_;
		this.orelse = orelse;
		this.type_comments = type_comments;
	}
}

	// | While(expr test, stmt* body, stmt* orelse)
class While : Stmt {
	Expr test;
	Stmt[] body_;
	Stmt[] orelse;
	this(Expr test, Stmt[] body_, Stmt[] orelse) {
		this.test = test;
		this.body_ = body_;
		this.orelse = orelse;
	}
}
	//| If(expr test, stmt* body, stmt* orelse)
class If : Stmt {
	Expr test;
	Stmt[] body_;
	Stmt[] orelse;
	this(Expr test, Stmt[] body_, Stmt[] orelse) {
		this.test = test;
		this.body_ = body_;
		this.orelse = orelse;
	}

}

	// | With(withitem* items, stmt* body, string? type_comment)
class With : Stmt {
	Withitem[] items;
	Stmt[] body_;
	Stmt[] orelse;
	this(Withitem[] items, Stmt[] body_, Stmt[] orelse) {
		this.items = items;
		this.body_ = body_;
		this.orelse = orelse;
	}
}
	// | AsyncWith(withitem* items, stmt* body, string? type_comment)
class AsyncWith : Stmt {
	Withitem[] items;
	Stmt[] body_;
	Stmt[] orelse;
	this(Withitem[] items, Stmt[] body_, Stmt[] orelse) {
		this.items = items;
		this.body_ = body_;
		this.orelse = orelse;
	}

}

	// | Raise(expr? exc, expr? cause)
class Raise : Stmt {
	Nullable!Expr exc;
	Nullable!Expr cause;
	this(Nullable!Expr exc, Nullable!Expr cause) {
		this.exc = exc;
		this.cause = cause;
	}
}
	// | Try(stmt* body, excepthandler* handlers, stmt* orelse, stmt* finalbody)
class Try : Stmt {
	Stmt[] body_;
	ExceptHandler[] handlers;
	Stmt[] orelse;
	Stmt[] finalbody;
	this(Stmt[] body_, ExceptHandler[] handlers, Stmt[] orelse, Stmt[] finalbody) {
		this.body_ = body_;
		this.handlers = handlers;
		this.orelse = orelse;
		this.finalbody = finalbody;
	}
}
	// | Assert(expr test, expr? msg)
class Assert : Stmt {
	Stmt test;
	Nullable!Expr msg;
	this(Stmt test, Nullable!Expr msg) {
		this.test = test;
		this.msg = msg;
	}
}

	// | Import(alias* names)
class Import : Stmt {
	Alias[] names;
	this(Alias[] names) {
		this.names = names;
	}
}
		  //| ImportFrom(identifier? module, alias* names, int? level)
class ImportFrom : Stmt {
	Nullable!Identifier module_;
	Alias[] names;
	Nullable!long level;
	this(Nullable!Identifier module_, Alias[] names, Nullable!long level) {
		this.module_ = module_;
		this.names = names;
		this.level = level;
	}
}

		// | Global(identifier* names)
class Global : Stmt {
	Identifier[] names;
	this(Identifier[] names) {
		this.names = names;
	}
}
	//  | Nonlocal(identifier* names)
class Nonlocal : Stmt {
	Identifier[] names;
	this(Identifier[] names) {
		this.names = names;
	}

}
	// | Expr(expr value)
	// | Pass | Break | Continue
class Pass : Stmt {}
class Break : Stmt {}
class Continue : Stmt {}

		  //-- col_offset is the byte offset in the utf8 string the parser uses
		  //attributes (int lineno, int col_offset, int? end_lineno, int? end_col_offset)

		  //-- BoolOp() can use left & right?

class Expr : Stmt {
}

	//expr = BoolOp(boolop op, expr* values)
class BoolOp : Expr {
	bool op;
	Expr[] values;
	this(bool op, Expr[] values) {
		this.op = op;
		this.values = values;
	}
}
	// | NamedExpr(expr target, expr value)
class NamedExpr : Expr {
	Expr target;
	Expr value;
	this(Expr target, Expr value) {
		this.target = target;
		this.value = value;
	}
}
	// | BinOp(expr left, operator op, expr right)
class BinOp : Expr {
	Expr left;
	Operator op;
	Expr right;
	this(Expr left, Operator op, Expr right) {
		this.left = left;
		this.op = op;
		this.right = right;
	}
}
	// | UnaryOp(unaryop op, expr operand)
class UnaryOp : Expr {
	Unaryop op;
	Expr operand;
	this(Unaryop op, Expr operand) {
		this.op = op;
		this.operand = operand;
	}
}
	// | Lambda(arguments args, expr body)
class Lamdba : Expr {
	Arguments args;
	Expr body_;
	this(Arguments args, Expr body_) {
		this.args = args;
		this.body_ = body_;
	}
}
	// | IfExp(expr test, expr body, expr orelse)
class IfExp : Expr {
	Expr test;
	Expr body_;
	Expr orelse;
	this(Expr test, Expr body_, Expr orelse) {
		this.test = test;
		this.body_ = body_;
		this.orelse = orelse;
	}
}

	// | Dict(expr* keys, expr* values)
class Dict : Expr {
	Expr[] keys;
	Expr[] values;
	this(Expr[] keys, Expr[] values) {
		this.keys = keys;
		this.values = values;
	}
}
	// | Set(expr* elts)
class Set : Expr {
	Expr[] elts;
	this(Expr[] elts) {
		this.elts = elts;
	}
}
	// | ListComp(expr elt, comprehension* generators)
class LstComp : Expr {
	Expr elt;
	Comprehension[] generators;
	this(Expr elt, Comprehension[] generators) {
		this.elt = elt;
		this.generators = generators;
	}
}
	// | SetComp(expr elt, comprehension* generators)
class SetComp : Expr {
	Expr elt;
	Comprehension[] generators;
	this(Expr elt, Comprehension[] generators) {
		this.elt = elt;
		this.generators = generators;
	}
}
	// | DictComp(expr key, expr value, comprehension* generators)
class DictComp : Expr {
	Expr key;
	Expr value;
	Comprehension[] generators;
	this(Expr key, Expr value, Comprehension[] generators) {
		this.key = key;
		this.value = value;
		this.generators = generators;
	}
}
	// | GeneratorExp(expr elt, comprehension* generators)
class GeneratorComp : Expr {
	Expr elt;
	Comprehension[] generators;
	this(Expr elt, Comprehension[] generators) {
		this.elt = elt;
		this.generators = generators;
	}
}
		 //-- the grammar constrains where yield expressions can occur
	// | Await(expr value)
class Await : Expr {
	Expr value;
	this(Expr value) {
		this.value = value;
	}
}
	// | Yield(expr? value)
class Yield : Expr {
	Nullable!Expr value;
	this(Nullable!Expr value) {
		this.value = value;
	}
}
	// | YieldFrom(expr value)
class YieldFrom : Expr {
	Expr value;
	this(Expr value) {
		this.value = value;
	}
}
		 //-- need sequences for compare to distinguish between
		 //-- x < 4 < 3 and (x < 4) < 3

	// | Compare(expr left, cmpop* ops, expr* comparators)
class Compare : Expr {
	Expr left;
	Cmpop[] ops;
	Expr[] comparators;
	this(Expr left, Cmpop[] ops, Expr[] comparators) {
		this.left = left;
		this.ops = ops;
		this.comparators = comparators;
	}
}
	// | Call(expr func, expr* args, keyword* keywords)
class Call : Expr {
	Expr func;
	Expr[] args;
	Keyword[] keywords;
	this(Expr func, Expr[] args, Keyword[] keywords) {
		this.func = func;
		this.args = args;
		this.keywords = keywords;
	}
}
	// | FormattedValue(expr value, int? conversion, expr? format_spec)
class FormattedValue : Expr {
	Expr value;
	Nullable!(long) conversion;
	Nullable!Expr format_spec;
	this(Expr value, Nullable!(long) conversion, Nullable!Expr format_spec) {
		this.value = value;
		this.conversion = conversion;
		this.format_spec = format_spec;
	}
}
	// | JoinedStr(expr* values)
class JoinedStr : Expr {
	Expr[] values;
	this(Expr[] values) {
		this.values = values;
	}
}

	// | Constant(constant value, string? kind)
class Constant : Expr {
	Constant value;
	Nullable!string kind;
	this(Constant value, Nullable!string kind) {
		this.value = value;
		this.kind = kind;
	}
}

	// -- the following expression can appear in assignment context

	// | Attribute(expr value, identifier attr, expr_context ctx)
class Attribute : Expr {
	Expr value;
	Identifier attr;
	Expr_Context ctx;
	this(Expr value, Identifier attr, Expr_Context ctx) {
		this.value = value;
		this.attr = attr;
		this.ctx = ctx;
	}
}
	// | Subscript(expr value, expr slice, expr_context ctx)
class Subscript : Expr {
	Expr value;
	Expr slice;
	Expr_Context ctx;
	this(Expr value, Expr slice, Expr_Context ctx) {
		this.value = value;
		this.slice = slice;
		this.ctx = ctx;
	}
}
	// | Starred(expr value, expr_context ctx)
class Starred : Expr {
	Expr value;
	Expr_Context ctx;
	this(Expr value, Expr_Context ctx) {
		this.value = value;
		this.ctx = ctx;
	}
}
	// | Name(identifier id, expr_context ctx)
class Name : Expr {
	Identifier id;
	Expr_Context ctx;
	this(Identifier id, Expr_Context ctx) {
		this.id = id;
		this.ctx = ctx;
	}
}
	// | List(expr* elts, expr_context ctx)
class List : Expr {
	Expr[] elts;
	Expr_Context ctx;
	this(Expr[] elts, Expr_Context ctx) {
		this.elts = elts;
		this.ctx = ctx;
	}
}
	// | Tuple(expr* elts, expr_context ctx)
class Tuple : Expr {
	Expr[] elts;
	Expr_Context ctx;
	this(Expr[] elts, Expr_Context ctx) {
		this.elts = elts;
		this.ctx = ctx;
	}
}

	// -- can appear only in Subscript

	// | Slice(expr? lower, expr? upper, expr? step)
class Slice : Expr {
	Nullable!Expr lower;
	Nullable!Expr upper;
	Nullable!Expr step;
	this(Nullable!Expr lower, Nullable!Expr upper, Nullable!Expr step) {
		this.lower = this.lower;
		this.upper = this.upper;
		this.step = this.step;
	}
}

	// -- col_offset is the byte offset in the utf8 string the parser uses

	//expr_context = Load | Store | Del
enum Expr_Context 
	{ Load
	, Store 
	, Del
	}

//	boolop = And | Or
enum Boolop 
	{ And
	, Or
	}

	//operator = Add | Sub | Mult | MatMult | Div | Mod | Pow | LShift
	//			 | RShift | BitOr | BitXor | BitAnd | FloorDiv
enum Operator  
	{ Add 
	, Sub 
	, Mult 
	, MatMult 
	, Div 
	, Mod 
	, Pow 
	, LShift
	, RShift 
	, BitOr 
	, BitXor 
	, BitAnd 
	, FloorDiv
	}

	//unaryop = Invert | Not | UAdd | USub
enum Unaryop 
	{ Invert 
	, Not 
	, UAdd 
	, USub
	}

	//cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
enum Cmpop 
	{ Eq 
	, NotEq 
	, Lt 
	, LtE 
	, Gt 
	, GtE 
	, Is 
	, IsNot 
	, In 
	, NotIn
	}

	//comprehension = (expr target, expr iter, expr* ifs, int is_async)
class Comprehension : Expr {
	Expr target;
	Expr iter;
	Expr[] ifs;
	long is_async;

	this(Expr target, Expr iter, Expr[] ifs, long is_async) {
		this.target = target;
		this.iter = iter;
		this.ifs = ifs;
		this.is_async = is_async;
	}
}

//	excepthandler = ExceptHandler(expr? type, identifier? name, stmt* body)
//					attributes (int lineno, int col_offset, int? end_lineno, int? end_col_offset)
class ExceptHandler : Expr {
	Nullable!Expr type;
	Nullable!Identifier name;
	Stmt[] body_;

	this(Nullable!Expr type, Nullable!Identifier name, Stmt[] body_) {
		this.type = type;
		this.name = name;
		this.body_ = body_;
	}
}

	// arguments = (arg* posonlyargs, arg* args, arg? vararg, arg* kwonlyargs,
	// 			 expr* kw_defaults, arg? kwarg, expr* defaults)
class Arguments : Expr {
	Arg[] posonlyargs;
	Arg[] args;
	Nullable!Arg vararg;
	Arg[] kwonlyargs;
	Expr[] kw_defaults;
	Nullable!Arg kwarg;
	Expr[] defaults;

	this(Arg[] posonlyargs, Arg[] args, Nullable!Arg vararg, Arg[] kwonlyargs,
			Expr[] kw_defaults, Nullable!Arg kwarg, Expr[] defaults) 
	{
		this.posonlyargs = posonlyargs;
		this.args = args;
		this.vararg = vararg;
		this.kwonlyargs = kwonlyargs;
		this.kw_defaults = kw_defaults;
		this.kwarg = kwarg;
		this.defaults = defaults;
	}
}

	// arg = (identifier arg, expr? annotation, string? type_comment)
	// 	   attributes (int lineno, int col_offset, int? end_lineno, int? end_col_offset)
class Arg : Expr {
	Identifier arg;
	Nullable!Expr annotation;
	Nullable!string type_comment;

	this(Identifier arg, Nullable!Expr annotation, Nullable!string type_comment) {
		this.arg = arg;
		this.annotation = annotation;
		this.type_comment = type_comment;
	}
}

//	-- keyword arguments supplied to call (NULL identifier for **kwargs)
//	keyword = (identifier? arg, expr value)
//			   attributes (int lineno, int col_offset, int? end_lineno, int? end_col_offset)
class Keyword : Expr {
	Nullable!Identifier arg;
	Expr value;

	this(Nullable!Identifier arg, Expr value) {
		this.arg = arg;
		this.value = value;
	}
}

//	-- import name with optional 'as' alias.
//	alias = (identifier name, identifier? asname)

class Alias : Node {
	Identifier name;
	Nullable!Identifier asname;
	this(Identifier name, Nullable!Identifier asname) {
		this.name = name;
		this.asname = asname;
	}
}

	// withitem = (expr context_expr, expr? optional_vars)
class Withitem : Node {
	Expr context_expr;
	Nullable!Expr optional_vars;
	this(Expr context_expr, Nullable!Expr optional_vars) {
		this.context_expr = context_expr;
		this.optional_vars = optional_vars;
	}
}

	// type_ignore = TypeIgnore(int lineno, string tag)
class TypeIgnore : Node {
	long lineno;
	string tag;
	this(long lineno, string tag) {
		this.lineno = lineno;
		this.tag = tag;
	}
}
