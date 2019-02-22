import ast
import functools
import operator

from vyper.types import (
    BaseType,
)

from .expr import (
    Expr
)


def _check_all(value, checker_fns):
    return all(
        fn(value)
        for fn
        in checker_fns
    )


def check_all(*checker_fns):
    return functools.partial(_check_all, checker_fns=checker_fns)


def _check_any(value, checker_fns):
    return any(
        fn(value)
        for fn
        in checker_fns
    )


def check_any(*checker_fns):
    return functools.partial(_check_any, checker_fns=checker_fns)


def is_ast_for(value):
    return isinstance(value, ast.For)


def is_ast_call(value):
    return isinstance(value, ast.Call)


def is_ast_name(value):
    return isinstance(value, ast.Name)


def _check_attr(value, attr, check_fn):
    try:
        value_attr = getattr(value, attr)
    except AttributeError:
        return False
    else:
        return check_fn(value_attr)


def check_attr(attr, check_fn):
    return functools.partial(_check_attr, attr=attr, check_fn=check_fn)


def check_nested_attr(attrs, check_fn):
    if not attrs:
        raise ValueError("Must provide at least one attribute")
    return functools.reduce(
        lambda fn, attr: check_attr(attr, fn),
        reversed(attrs),
        check_fn,
    )


def apply_transform(transform_fn):
    def outer(fn):
        @functools.wraps(fn)
        def inner(value):
            return fn(transform_fn(value))
        return inner
    return outer


def check_in(seq):
    return functools.partial(operator.contains, seq)


def check_equals(other):
    return functools.partial(operator.eq, other)


def is_ast_binop(value):
    return isinstance(value, ast.BinOp)


def is_ast_binop_add(value):
    return is_ast_binop(value) and isinstance(value.op, ast.Add)


def _is_expr_basetype(value, context):
    expr = Expr.parse_value_expr(value, context)
    return isinstance(expr.typ, BaseType)


def is_expr_basetype(context):
    return functools.partial(_is_expr_basetype, context=context)


def _is_expr_literal(value, context):
    expr = Expr.parse_value_expr(value, context)
    return expr.typ.is_literal


def is_expr_literal(context):
    return functools.partial(_is_expr_literal, context=context)


def _is_expr_of_type(value, context, _type):
    expr = Expr.parse_value_expr(value, context)
    return expr.typ.typ == _type


def check_expr_type_is_uint256(context):
    return functools.partial(_is_expr_of_type, context=context, _type='uint256')


def check_expr_type_is_int128(context):
    return functools.partial(_is_expr_of_type, context=context, _type='int128')


def check_length_equal(length):
    return apply_transform(len)(check_equals(length))


is_for_in_range_stmt = check_all(
    is_ast_for,
    check_attr('iter', is_ast_call),
    check_nested_attr(('iter', 'func'), is_ast_name),
    check_attr('target', is_ast_name),
    check_nested_attr(('iter', 'func', 'id'), check_equals('range')),
    check_nested_attr(('iter', 'args'), apply_transform(len)(check_in({1, 2}))),
)


def compliment(fn):
    @functools.wraps(fn)
    def inner(*args, **kwargs):
        return not fn(*args, **kwargs)
    return inner


def _is_expr_literal_integer(value, context):
    return check_all(
        is_expr_basetype(context),
        is_expr_literal(context),
        check_any(
            check_expr_type_is_uint256(context),
            check_expr_type_is_int128(context),
        ),
    )(value)


def is_expr_literal_integer(context):
    return functools.partial(_is_expr_literal_integer, context=context)


def _is_expr_non_literal_integer(value, context):
    return check_all(
        is_expr_basetype(context),
        compliment(is_expr_literal(context)),
        check_any(
            check_expr_type_is_uint256(context),
            check_expr_type_is_int128(context),
        ),
    )(value)


def is_expr_non_literal_integer(context):
    return functools.partial(_is_expr_non_literal_integer, context=context)


def _is_expr_any_integer(value, context):
    return check_all(
        is_expr_basetype(context),
        check_any(
            check_expr_type_is_uint256(context),
            check_expr_type_is_int128(context),
        ),
    )(value)


def is_expr_any_integer(context):
    return functools.partial(_is_expr_any_integer, context=context)


def _is_expr_eval_to_integer(value, context):
    return check_all(
        is_ast_binop,
    )(value)


def is_expr_eval_to_integer(context):
    return functools.partial(_is_expr_eval_to_integer, context=context)


def _check_at_index(items, index, check_fn):
    try:
        value = items[index]
    except IndexError:
        return False
    else:
        return check_fn(value)


def check_at_index(index, check_fn):
    return functools.partial(_check_at_index, index=index, check_fn=check_fn)


def is_range_with_one_literal_value(value, context):
    return check_nested_attr(
        ('iter', 'args'),
        check_all(
            check_length_equal(1),
            check_at_index(0, is_expr_literal_integer(context)),
        ),
    )(value)


def is_range_with_one_non_literal_value(value, context):
    return check_nested_attr(
        ('iter', 'args'),
        check_all(
            check_length_equal(1),
            check_at_index(0, is_expr_non_literal_integer(context)),
        ),
    )(value)


def is_range_with_two_literal_values(value, context):
    return check_nested_attr(
        ('iter', 'args'),
        check_all(
            check_length_equal(2),
            check_at_index(0, is_expr_literal_integer(context)),
            check_at_index(1, is_expr_literal_integer(context)),
        ),
    )(value)


def is_range_with_value_and_expression(value, context):
    return check_nested_attr(
        ('iter', 'args'),
        check_all(
            check_length_equal(2),
            check_at_index(0, is_expr_non_literal_integer(context)),
            check_at_index(1, is_expr_literal_integer(context)),
        ),
    )(value)


def is_for_with_constant():
    # Type 1 for, e.g. for i in range(10): ...
    if num_of_args == 1:
        arg0_val = self._get_range_const_value(arg0)
        start = LLLnode.from_list(0, typ='int128', pos=getpos(self.stmt))
        rounds = arg0_val

def is_for_with_two_valid_args():
    arg1 = self.stmt.iter.args[1]
    if not isinstance(arg1, ast.BinOp) or not isinstance(arg1.op, ast.Add):
        raise StructureException(
            (
                "Two-arg for statements must be of the form `for i "
                "in range(start, start + rounds): ...`"
            ),
            arg1,
        )

    if ast.dump(arg0) != ast.dump(arg1.left):
        raise StructureException(
            (
                "Two-arg for statements of the form `for i in "
                "range(x, x + y): ...` must have x identical in both "
                "places: %r %r"
            ) % (
                ast.dump(arg0),
                ast.dump(arg1.left)
            ),
            self.stmt.iter,
        )

    rounds = self._get_range_const_value(arg1.right)
    start = Expr.parse_value_expr(arg0, self.context)
