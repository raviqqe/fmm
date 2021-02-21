use super::attempt::{many, many1};
use crate::ir::*;
use crate::types::{self, CallingConvention, Type};
use combine::parser::char::{alpha_num, char as character, digit, letter, spaces, string};
use combine::parser::combinator::{lazy, no_partial, not_followed_by};
use combine::parser::regex::find;
use combine::parser::sequence::between;
use combine::{from_str, value, Parser};
use lazy_static::lazy_static;
use std::str::FromStr;

lazy_static! {
    static ref FLOAT_REGEX: regex::Regex =
        regex::Regex::new(r"^-?([123456789][0123456789]*|0)(\.[0123456789]+)?").unwrap();
}

pub fn instruction<'a>() -> impl Parser<&'a str, Output = Instruction> {
    choice!(call().map(Instruction::from))
}

fn call<'a>() -> impl Parser<&'a str, Output = Call> {
    parentheses((
        keyword("call"),
        function_type(),
        expression(),
        many(expression()),
        identifier(),
    ))
    .map(|(_, type_, function, arguments, name)| Call::new(type_, function, arguments, name))
}

fn expression<'a>() -> impl Parser<&'a str, Output = Expression> {
    lazy(|| {
        no_partial(choice!(
            primitive().map(Expression::from),
            record().map(Expression::from),
            undefined().map(Expression::from),
            union().map(Expression::from),
            variable().map(Expression::from),
        ))
    })
    .boxed()
}

fn primitive<'a>() -> impl Parser<&'a str, Output = Primitive> {
    choice!(
        choice!(
            keyword("true").map(|_| true),
            keyword("false").map(|_| false)
        )
        .map(Primitive::Boolean),
        float().map(Primitive::Float32),
        float().map(Primitive::Float64),
        integer().map(Primitive::Integer8),
        integer().map(Primitive::Integer32),
        integer().map(Primitive::Integer64),
        integer().map(Primitive::PointerInteger),
    )
}

fn record<'a>() -> impl Parser<&'a str, Output = Record> {
    parentheses((keyword("record"), record_type(), many(expression())))
        .map(|(_, record_type, expressions)| Record::new(record_type, expressions))
}

fn undefined<'a>() -> impl Parser<&'a str, Output = Undefined> {
    parentheses((keyword("undefined"), type_())).map(|(_, type_)| Undefined::new(type_))
}

fn union<'a>() -> impl Parser<&'a str, Output = Union> {
    parentheses((keyword("union"), union_type(), integer(), expression()))
        .map(|(_, type_, index, expressions)| Union::new(type_, index, expressions))
}

fn variable<'a>() -> impl Parser<&'a str, Output = Variable> {
    identifier().map(Variable::new)
}

fn type_<'a>() -> impl Parser<&'a str, Output = Type> {
    lazy(|| {
        no_partial(choice!(
            function_type().map(Type::from),
            pointer_type().map(Type::from),
            primitive_type().map(Type::from),
            record_type().map(Type::from),
            union_type().map(Type::from),
        ))
    })
    .boxed()
}

fn function_type<'a>() -> impl Parser<&'a str, Output = types::Function> {
    parentheses((
        sign("\\"),
        calling_convention(),
        sign("("),
        many(type_()),
        sign(")"),
        type_(),
    ))
    .map(|(_, calling_convention, _, arguments, _, result)| {
        types::Function::new(arguments, result, calling_convention)
    })
}

fn pointer_type<'a>() -> impl Parser<&'a str, Output = types::Pointer> {
    parentheses((sign("*"), type_())).map(|(_, type_)| types::Pointer::new(type_))
}

fn record_type<'a>() -> impl Parser<&'a str, Output = types::Record> {
    parentheses((keyword("record"), many(type_()))).map(|(_, types)| types::Record::new(types))
}

fn union_type<'a>() -> impl Parser<&'a str, Output = types::Union> {
    parentheses((keyword("union"), many(type_()))).map(|(_, types)| types::Union::new(types))
}

fn calling_convention<'a>() -> impl Parser<&'a str, Output = CallingConvention> {
    choice!(
        keyword("direct").map(|_| CallingConvention::Direct),
        keyword("tail").map(|_| CallingConvention::Tail),
    )
}

fn primitive_type<'a>() -> impl Parser<&'a str, Output = types::Primitive> {
    choice!(
        keyword("i1").map(|_| types::Primitive::Boolean),
        keyword("f32").map(|_| types::Primitive::Float32),
        keyword("f64").map(|_| types::Primitive::Float64),
        keyword("i8").map(|_| types::Primitive::Integer8),
        keyword("i32").map(|_| types::Primitive::Integer32),
        keyword("i64").map(|_| types::Primitive::Integer64),
        keyword("isize").map(|_| types::Primitive::PointerInteger),
    )
}

fn parentheses<'a, O, P: Parser<&'a str, Output = O>>(
    parser: P,
) -> impl Parser<&'a str, Output = O> {
    between(sign("("), sign(")"), parser)
}

fn identifier<'a>() -> impl Parser<&'a str, Output = String> {
    token(
        (
            choice!(letter(), character('_')),
            many(choice!(alpha_num(), character('_'))),
        )
            .map(|(head, tail): (char, String)| [head.into(), tail].concat()),
    )
}

fn float<'a, T: FromStr>() -> impl Parser<&'a str, Output = T>
where
    T::Err: std::error::Error,
{
    let regex: &'static regex::Regex = &FLOAT_REGEX;

    token(from_str(find(regex)))
}

fn integer<'a, T: FromStr>() -> impl Parser<&'a str, Output = T>
where
    T::Err: std::error::Error,
{
    token(from_str(many1::<String, _, _>(digit())))
}

fn keyword<'a>(name: &'static str) -> impl Parser<&'a str, Output = ()> {
    token(string(name).skip(not_followed_by(alpha_num()))).with(value(()))
}

fn sign<'a>(sign: &'static str) -> impl Parser<&'a str, Output = ()> {
    token(string(sign)).with(value(()))
}

fn token<'a, O, P: Parser<&'a str, Output = O>>(parser: P) -> impl Parser<&'a str, Output = O> {
    spaces().with(parser)
}
