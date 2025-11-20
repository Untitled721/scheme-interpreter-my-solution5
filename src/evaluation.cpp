

/**
 * @file evaluation.cpp
 * @brief Expression evaluation implementation for the Scheme interpreter
 * @author luke36
 *
 * This file implements evaluation methods for all expression types in the Scheme
 * interpreter. Functions are organized according to ExprType enumeration order
 * from Def.hpp for consistency and maintainability.
 */

#include "value.hpp"
#include "expr.hpp"
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
#include <climits>

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Value Fixnum::eval(Assoc &e) { // evaluation of a fixnum
    return IntegerV(n);
}

Value RationalNum::eval(Assoc &e) { // evaluation of a rational number
    return RationalV(numerator, denominator);
}

Value StringExpr::eval(Assoc &e) { // evaluation of a string
    return StringV(s);
}

Value True::eval(Assoc &e) { // evaluation of #t
    return BooleanV(true);
}

Value False::eval(Assoc &e) { // evaluation of #f
    return BooleanV(false);
}

Value MakeVoid::eval(Assoc &e) { // (void)
    return VoidV();
}

Value Exit::eval(Assoc &e) { // (exit)
    return TerminateV();
}

Value Unary::eval(Assoc &e) { // evaluation of single-operator primitive
    return evalRator(rand->eval(e));
}

Value Binary::eval(Assoc &e) { // evaluation of two-operators primitive
    return evalRator(rand1->eval(e), rand2->eval(e));
}

//****
Value Variadic::eval(Assoc &e) { // evaluation of multi-operator primitive
    // TODO: TO COMPLETE THE VARIADIC CLASS
    std::vector<Value> evaluated_args;
    for (const auto &arg_expr : rands) {
        evaluated_args.push_back(arg_expr->eval(e));
    }
    return evalRator(evaluated_args);

}

Value Var::eval(Assoc &e) { // evaluation of variable
    // TODO: TO identify the invalid variable
    //当前环境查找，找到返回
    // if (matched_value.get() != nullptr) {
    //     return matched_value;
    // }

    if (x.empty()) {
            throw RuntimeError("Empty expression");
        }
    if (std::isdigit(x[0]) || x[0] == '.' || x[0] == '@') {
            throw RuntimeError("Invalid expression");
        }
        // 在现有的检查后面添加：
    for (char c : x) {
        if (c == '\'' || c == '"' || c == '`'|| c == '#') {
            throw RuntimeError("Invalid expression");
        }
    }
    Value matched_value = find(x, e);
    if (matched_value.get()!=nullptr) {
        return matched_value;
    }
        //内置函数，创建闭包返回

        if (primitives.count(x)) {
            std::cout << "Found primitive: " << x << std::endl;
            static std::map<ExprType, std::pair<Expr, std::vector<std::string>>> primitive_map = {
                {E_VOID,     {new MakeVoid(), {}}},
                {E_EXIT,     {new Exit(), {}}},
                {E_BOOLQ,    {new IsBoolean(new Var("parm")), {"parm"}}},
                {E_INTQ,     {new IsFixnum(new Var("parm")), {"parm"}}},
                {E_NULLQ,    {new IsNull(new Var("parm")), {"parm"}}},
                {E_PAIRQ,    {new IsPair(new Var("parm")), {"parm"}}},
                {E_PROCQ,    {new IsProcedure(new Var("parm")), {"parm"}}},
                {E_SYMBOLQ,  {new IsSymbol(new Var("parm")), {"parm"}}},
                {E_STRINGQ,  {new IsString(new Var("parm")), {"parm"}}},
                {E_DISPLAY,  {new Display(new Var("parm")), {"parm"}}},

               {E_PLUS,     {new Plus(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},  // 改为二元 Plus
               {E_MINUS,    {new Minus(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}}, // 改为二元 Minus
               {E_MUL,      {new Mult(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},  // 改为二元 Mult
               {E_DIV,      {new Div(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},   // 改为二元 Div
               {E_MODULO,   {new Modulo(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
               {E_EXPT,     {new Expt(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
               {E_EQQ,      {new IsEq(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},  // 改为二元 IsEq
               {E_LT,       {new Less(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},  // 添加比较运算符
               {E_LE,       {new LessEq(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
               {E_EQ,       {new Equal(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
               {E_GE,       {new GreaterEq(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
               {E_GT,       {new Greater(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
               {E_CONS,     {new Cons(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},  // 添加 cons
               {E_CAR,      {new Car(new Var("parm")), {"parm"}}},                               // 添加 car
               {E_CDR,      {new Cdr(new Var("parm")), {"parm"}}},                               // 添加 cdr
               {E_NOT,      {new Not(new Var("parm")), {"parm"}}},                               // 添加 not
               {E_SETCAR,   {new SetCar(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}}, // 添加 set-car!
               {E_SETCDR,   {new SetCdr(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}}, // 添加 set-cdr!
             };

            auto it = primitive_map.find(primitives[x]);
            //TOD0:to PASS THE parameters correctly;
            //COMPLETE THE CODE WITH THE HINT IN IF SENTENCE WITH CORRECT RETURN VALUE
            if (it != primitive_map.end()) {
                //TODO
                return ProcedureV(it->second.second, it->second.first,e);
            }
        }

    throw RuntimeError("Variable " + x + " not defined");
}

Value Plus::evalRator(const Value &rand1, const Value &rand2) { // +
    //TODO: To complete the addition logic
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 + n2;
        return IntegerV(result);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());

        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int num2 = r2->numerator;
        int den2 = r2->denominator;

        int new_num = num1 * den2 + num2 * den1;
        int new_den = den1 * den2;

        //*********
        if (new_den < 0) {
            new_num = -new_num;
            new_den = -new_den;
        }

        return RationalV(new_num, new_den);
    }
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num2 = r2->numerator;
        int den2 = r2->denominator;
        int new_num = n1 * den2 + num2;
        return RationalV(new_num, den2);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int new_num = n2 * den1 + num1;
        return RationalV(new_num, den1);
    }
    throw(RuntimeError("Wrong typename"));
}



Value Minus::evalRator(const Value &rand1, const Value &rand2) { // -
    //TODO: To complete the substraction logic
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 - n2;
        return IntegerV(result);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int num2 = r2->numerator;
        int den2 = r2->denominator;
        int new_num = num1 * den2 - num2 * den1;
        int new_den = den1 * den2;
        if (new_den < 0) {
            new_num = -new_num;
            new_den = -new_den;
        }
        return RationalV(new_num, new_den);
    }
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num = r2->numerator;
        int den = r2->denominator;
        int new_num = n1 * den - num;
        return RationalV(new_num, den);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int num = r1->numerator;
        int den = r1->denominator;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
         int new_num = num - n2 * den;;
        return RationalV(new_num, den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Mult::evalRator(const Value &rand1, const Value &rand2) { // *
    //TODO: To complete the Multiplication logic
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 * n2;
        return IntegerV(result);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int num2 = r2->numerator;
        int den2 = r2->denominator;
        int new_num = num1 * num2;
        int new_den = den1 * den2;
        if (new_den < 0) {
            new_num = -new_num;
            new_den = -new_den;
        }
        return RationalV(new_num, new_den);
    }
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num = r2->numerator;
        int den = r2->denominator;
        int new_num = n * num;
        return RationalV(new_num, den);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int num = r1->numerator;
        int den = r1->denominator;
        int n = dynamic_cast<Integer*>(rand2.get())->n;
        int new_num = n * num;
        return RationalV(new_num, den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Div::evalRator(const Value &rand1, const Value &rand2) { // /
    //TODO: To complete the dicision logic
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        if (n2 != 0) {
            if (n2 < 0) {
                n1 = -n1;
                n2 = -n2;
            }
            return RationalV(n1, n2);
        }else {
            throw(RuntimeError("Wrong typename"));
        }
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int num2 = r2->numerator;
        int den2 = r2->denominator;
        int new_num = num1 * den2;
        int new_den = den1 * num2;
        if (new_den != 0) {
            if (new_den < 0) {
                new_num = -new_num;
                new_den = -new_den;
            }
            return RationalV(new_num, new_den);
        }
        else {
            throw(RuntimeError("Wrong typename"));
        }

    }
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num = r2->numerator;
        int den = r2->denominator;
        int new_num = n * den;
        int new_den = num;
        if (new_den != 0) {
            if (new_den < 0) {
                new_num = -new_num;
                new_den = -new_den;
            }
            return RationalV(new_num, new_den);
        }else {
            throw(RuntimeError("Wrong typename"));
        }
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int num = r1->numerator;
        int den = r1->denominator;
        int n = dynamic_cast<Integer*>(rand2.get())->n;
        int new_den = den * n;
        if (new_den != 0) {
            if (new_den < 0) {
                num = -num;
                new_den = -new_den;
            }
            return RationalV(num, new_den);
        }
        else {
            throw(RuntimeError("Wrong typename"));
        }
    }
    throw(RuntimeError("Wrong typename"));
}

Value Modulo::evalRator(const Value &rand1, const Value &rand2) { // modulo
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int dividend = dynamic_cast<Integer*>(rand1.get())->n;
        int divisor = dynamic_cast<Integer*>(rand2.get())->n;
        if (divisor == 0) {
            throw(RuntimeError("Division by zero"));
        }
        return IntegerV(dividend % divisor);
    }
    throw(RuntimeError("modulo is only defined for integers"));
}

Value add(const Value &rand1, const Value &rand2) {
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 + n2;
        return IntegerV(result);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());

        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int num2 = r2->numerator;
        int den2 = r2->denominator;

        int new_num = num1 * den2 + num2 * den1;
        int new_den = den1 * den2;

        if (new_den < 0) {
            new_num = -new_num;
            new_den = -new_den;
        }

        return RationalV(new_num, new_den);
    }
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num2 = r2->numerator;
        int den2 = r2->denominator;
        int new_num = n1 * den2 + num2;
        return RationalV(new_num, den2);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int new_num = n2 * den1 + num1;
        return RationalV(new_num, den1);
    }
    throw(RuntimeError("Wrong typename"));
}
Value PlusVar::evalRator(const std::vector<Value> &args) { // + with multiple args
    //TODO: To complete the addition logic
    if (args.empty()) {
        return IntegerV(0);
    }

    Value result = args[0];
    for (int i = 1; i < args.size(); ++i) {
        result = add(result, args[i]);
    }
    return result;
}

Value minu(const Value &rand1, const Value &rand2) {
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 - n2;
        return IntegerV(result);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int num2 = r2->numerator;
        int den2 = r2->denominator;
        int new_num = num1 * den2 - num2 * den1;
        int new_den = den1 * den2;
        if (new_den < 0) {
            new_num = -new_num;
            new_den = -new_den;
        }
        return RationalV(new_num, new_den);
    }
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num = r2->numerator;
        int den = r2->denominator;
        int new_num = n1 * den - num;
        return RationalV(new_num, den);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int num = r1->numerator;
        int den = r1->denominator;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int new_num = num - den*n2;
        return RationalV(new_num, den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value MinusVar::evalRator(const std::vector<Value> &args) { // - with multiple args
    //TODO: To complete the substraction logic
    if (args.empty()) {
        throw(RuntimeError("Wrong typename"));
    }
    if (args.size() == 1) {
        return minu(IntegerV(0), args[0]);
    }

    Value result = args[0];
    for (int i = 1; i < args.size(); ++i) {
        result = minu(result, args[i]);
    }
    return result;
}

Value multiply(const Value &rand1, const Value &rand2) {
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 * n2;
        return IntegerV(result);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int num2 = r2->numerator;
        int den2 = r2->denominator;
        int new_num = num1 * num2;
        int new_den = den1 * den2;
        if (new_den < 0) {
            new_num = -new_num;
            new_den = -new_den;
        }
        return RationalV(new_num, new_den);
    }
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num = r2->numerator;
        int den = r2->denominator;
        int new_num = n * num;
        return RationalV(new_num, den);
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int num = r1->numerator;
        int den = r1->denominator;
        int n = dynamic_cast<Integer*>(rand2.get())->n;
        int new_num = n * num;
        return RationalV(new_num, den);
    }
    throw(RuntimeError("Wrong typename"));
}
Value MultVar::evalRator(const std::vector<Value> &args) { // * with multiple args
    //TODO: To complete the multiplication logic
    if (args.empty()) {
        return IntegerV(1);
    }
    Value result = args[0];
    for (int i = 1; i < args.size(); ++i) {
        result = multiply(result, args[i]);
    }
    return result;
}

Value divide(const Value &rand1, const Value &rand2) {
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        if (n2 != 0) {
            if (n2 < 0) {
                n1 = -n1;
                n2 = -n2;
            }
            return RationalV(n1, n2);
        }else {
            throw(RuntimeError("Wrong typename"));
        }
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num1 = r1->numerator;
        int den1 = r1->denominator;
        int num2 = r2->numerator;
        int den2 = r2->denominator;
        int new_num = num1 * den2;
        int new_den = den1 * num2;
        if (new_den != 0) {
            if (new_den < 0) {
                new_num = -new_num;
                new_den = -new_den;
            }
            return RationalV(new_num, new_den);
        }
        else {
            throw(RuntimeError("Wrong typename"));
        }

    }
    else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL) {
        int n = dynamic_cast<Integer*>(rand1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(rand2.get());
        int num = r2->numerator;
        int den = r2->denominator;
        int new_num = n * den;
        int new_den = num;
        if (new_den != 0) {
            if (new_den < 0) {
                new_num = -new_num;
                new_den = -new_den;
            }
            return RationalV(new_num, new_den);
        }else {
            throw(RuntimeError("Wrong typename"));
        }
    }
    else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(rand1.get());
        int num = r1->numerator;
        int den = r1->denominator;
        int n = dynamic_cast<Integer*>(rand2.get())->n;
        int new_den = den * n;
        if (new_den != 0) {
            if (new_den < 0) {
                num = -num;
                new_den = -new_den;
            }
            return RationalV(num, new_den);
        }
        else {
            throw(RuntimeError("Wrong typename"));
        }
    }
    throw(RuntimeError("Wrong typename"));
}
Value DivVar::evalRator(const std::vector<Value> &args) { // / with multiple args
    //TODO: To complete the divisor logic
    if (args.empty()) {
        throw(RuntimeError("Wrong typename"));
    }
    if (args.size() == 1) {
        return divide(IntegerV(1),args[0] );
    }
    Value result = args[0];
    for (int i = 1; i < args.size(); ++i) {
        result = divide(result, args[i]);
    }
    return result;
}


Value Expt::evalRator(const Value &rand1, const Value &rand2) { // expt
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int base = dynamic_cast<Integer*>(rand1.get())->n;
        int exponent = dynamic_cast<Integer*>(rand2.get())->n;

        if (exponent < 0) {
            throw(RuntimeError("Negative exponent not supported for integers"));
        }
        if (base == 0 && exponent == 0) {
            throw(RuntimeError("0^0 is undefined"));
        }

        long long result = 1;
        long long b = base;
        int exp = exponent;

        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= b;
                if (result > INT_MAX || result < INT_MIN) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            b *= b;
            if (b > INT_MAX || b < INT_MIN) {
                if (exp > 1) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            exp /= 2;
        }

        return IntegerV((int)result);
    }
    throw(RuntimeError("Wrong typename"));
}

//A FUNCTION TO SIMPLIFY THE COMPARISON WITH INTEGER AND RATIONAL NUMBER
int compareNumericValues(const Value &v1, const Value &v2) {
    if (v1->v_type == V_INT && v2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        return (n1 < n2) ? -1 : (n1 > n2) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        int left = r1->numerator;
        int right = n2 * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_INT && v2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = n1 * r2->denominator;
        int right = r2->numerator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = r1->numerator * r2->denominator;
        int right = r2->numerator * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    throw RuntimeError("Wrong typename in numeric comparison");
}

Value Less::evalRator(const Value &rand1, const Value &rand2) { // <
    //TODO: To complete the less logic
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp < 0);

}

Value LessEq::evalRator(const Value &rand1, const Value &rand2) { // <=
    //TODO: To complete the lesseq logic
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp <= 0);

}

Value Equal::evalRator(const Value &rand1, const Value &rand2) { // =
    //TODO: To complete the equal logic
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp == 0);

}

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) { // >=
    //TODO: To complete the greatereq logic
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp >= 0);

}

Value Greater::evalRator(const Value &rand1, const Value &rand2) { // >
    //TODO: To complete the greater logic
    int cmp = compareNumericValues(rand1, rand2);
    return BooleanV(cmp > 0);
}

Value LessVar::evalRator(const std::vector<Value> &args) { // < with multiple args
    //TODO: To complete the less logic
    if (args.size() < 2) {
        throw RuntimeError("Wrong number of arguments");
    }
    for (size_t i = 0; i < args.size()-1; i++) {
        if ((args[i]->v_type != V_INT && args[i]->v_type != V_RATIONAL) ||
           (args[i+1]->v_type != V_INT && args[i+1]->v_type != V_RATIONAL)) {
            throw(RuntimeError("Wrong typename"));
           }
        if (compareNumericValues(args[i], args[i+1]) >= 0) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value LessEqVar::evalRator(const std::vector<Value> &args) { // <= with multiple args
    //TODO: To complete the lesseq logic
    if (args.size() < 2) {
        throw RuntimeError("Wrong number of arguments");
    }
    for (int i = 0; i < args.size()-1; i++) {
        if (compareNumericValues(args[i], args[i+1]) > 0) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value EqualVar::evalRator(const std::vector<Value> &args) { // = with multiple args
    //TODO: To complete the equal logic
        if (args.size() < 2) {
            throw RuntimeError("Wrong number of arguments");
        }
        for (int i = 0; i < args.size()-1; i++) {
            if (compareNumericValues(args[i], args[i+1]) != 0) {
                return BooleanV(false);
            }
        }
        return BooleanV(true);
}


Value GreaterEqVar::evalRator(const std::vector<Value> &args) { // >= with multiple args
    //TODO: To complete the greatereq logic
    if (args.size() < 2) {
        throw RuntimeError("Wrong number of arguments");
    }
    for (int i = 0; i < args.size()-1; i++) {
        int cmp = compareNumericValues(args[i], args[i+1]);
        if (cmp < 0) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value GreaterVar::evalRator(const std::vector<Value> &args) { // > with multiple args
    //TODO: To complete the greater logic
    if (args.size() < 2) {
        throw RuntimeError("Wrong number of arguments");
    }
    for (int i = 0; i < args.size()-1; i++) {
        if (compareNumericValues(args[i], args[i+1]) <= 0) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}




Value Cons::evalRator(const Value &rand1, const Value &rand2) { // cons
    //TODO: To complete the cons logic
    return PairV(rand1, rand2);
}

Value ListFunc::evalRator(const std::vector<Value> &args) { // list function
    //TODO: To complete the list logic
    if (args.empty()) {
        return NullV();
    }
    Value result = NullV();

    for (int i = args.size()-1; i >= 0; i--) {
        result = PairV(args[i],result);
    }
    return result;
}

Value IsList::evalRator(const Value &rand) {
    // list?
    //TODO: To complete the list? logic

    if (rand->v_type == V_NULL) {
        return BooleanV(true);
    }

    if (rand->v_type != V_PAIR) {
        return BooleanV(false);
    }

    // 添加快慢指针检测环形列表
    Value slow = rand;
    Value fast = rand;

    while (fast->v_type == V_PAIR) {
        // 快指针前进两步
        fast = dynamic_cast<Pair*>(fast.get())->cdr;//如果fast不是pair，退出循环
        if (fast->v_type != V_PAIR) break;
        fast = dynamic_cast<Pair*>(fast.get())->cdr;

        // 慢指针前进一步
        slow = dynamic_cast<Pair*>(slow.get())->cdr;

        // 如果快慢指针指向同一个节点，说明有环
        if (slow.get() == fast.get()) {
            return BooleanV(false); // 有环的不是正常列表
        }
    }

    // 检查是否以 null 结尾
    return BooleanV(fast->v_type == V_NULL);
}
    //错误！！！如果是环形链表会无限循环
    // //无限循环，直到找到结果
    // Value cur = rand;
    // while (true) {
    //     if (cur->v_type == V_NULL) {
    //         return BooleanV(true);
    //     }
    //
    //     if (cur->v_type == V_PAIR) {
    //         auto pair = dynamic_cast<Pair*>(cur.get());
    //         cur = pair->cdr;
    //     }
    //
    //     else {
    //         return BooleanV(false);
    //     }
    // }


Value Car::evalRator(const Value &rand) { // car
    //TODO: To complete the car logic
    if (rand->v_type == V_PAIR) {
        Pair* pair = dynamic_cast<Pair*>(rand.get());
        return pair->car;
    }
    else {
        throw RuntimeError("Wrong typename");
    }

}

Value Cdr::evalRator(const Value &rand) { // cdr
    //TODO: To complete the cdr logic
    if (rand->v_type == V_PAIR) {
        Pair* pair = dynamic_cast<Pair*>(rand.get());
        return pair->cdr;
    }
    else {
        throw RuntimeError("Wrong typename");
    }
}

Value SetCar::evalRator(const Value &rand1, const Value &rand2) { // set-car!
    //TODO: To complete the set-car! logic
    // struct ValueBase {
    //     ValueType v_type; 只知道类型标签
    //     virtual void show(std::ostream &) = 0;
    //     没有 car, cdr 等具体字段
    // };
    // struct Pair : ValueBase {
    //     Value car;  // 包含具体字段
    //     Value cdr;
    //     .....
    // };
    if (rand1->v_type != V_PAIR) {
        throw RuntimeError("Wrong typename");
    }
    //将Value转换为Pair指针，指向实际的pair对象
    auto pair = dynamic_cast<Pair*>(rand1.get());
    pair->car = rand2;
    return VoidV();
}

Value SetCdr::evalRator(const Value &rand1, const Value &rand2) { // set-cdr!
   //TODO: To complete the set-cdr! logic
    if (rand1->v_type != V_PAIR) {
        throw RuntimeError("Wrong typename");
    }
    auto pair = dynamic_cast<Pair*>(rand1.get());
    pair->cdr = rand2;

    return VoidV();
}

Value IsEq::evalRator(const Value &rand1, const Value &rand2) { // eq?
    // 检查类型是否为 Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
    }
    // 检查类型是否为 Boolean
    else if (rand1->v_type == V_BOOL && rand2->v_type == V_BOOL) {
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    }
    // 检查类型是否为 Symbol
    else if (rand1->v_type == V_SYM && rand2->v_type == V_SYM) {
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    }
    // 检查类型是否为 Null 或 Void
    else if ((rand1->v_type == V_NULL && rand2->v_type == V_NULL) ||
             (rand1->v_type == V_VOID && rand2->v_type == V_VOID)) {
        return BooleanV(true);
    } else {
        return BooleanV(rand1.get() == rand2.get());
    }
}

Value IsBoolean::evalRator(const Value &rand) { // boolean?
    return BooleanV(rand->v_type == V_BOOL);
}

Value IsFixnum::evalRator(const Value &rand) { // number?
    return BooleanV(rand->v_type == V_INT);
}

Value IsNull::evalRator(const Value &rand) { // null?
    return BooleanV(rand->v_type == V_NULL);
}

Value IsPair::evalRator(const Value &rand) { // pair?
    return BooleanV(rand->v_type == V_PAIR);
}

Value IsProcedure::evalRator(const Value &rand) { // procedure?
    return BooleanV(rand->v_type == V_PROC);
}

Value IsSymbol::evalRator(const Value &rand) { // symbol?
    return BooleanV(rand->v_type == V_SYM);
}

Value IsString::evalRator(const Value &rand) { // string?
    return BooleanV(rand->v_type == V_STRING);
}

Value Begin::eval(Assoc &e) {
    //TODO: To complete the begin logic
    //begin全面修改，过于简化，只能处理普通表达式序列
    // Value result = VoidV();//创建一个表示空的值对象作为初始结果
    // for (auto &expr : es) {//遍历begin对象中的所有表达式，依次执行他们
    //     result = expr->eval(e);//->eval(e)：调用表达式的求值方法并储存到result中
    // }
    // return result;//返回最后一个表达式的结果
    //想要能处理包含内部定义的表达式序列的复杂版本，支持函数定义的相互递归调用

    if (es.empty()) {
        return VoidV(); //如果begin里没有表达式，返回空值
    }
    //第一步：识别连续的内部define
    std::vector<std::pair<std::string,Expr> > internal_defs;
    size_t first_non_define = 0;
    for (size_t i = 0; i < es.size(); i++) {
        if (es[i]->e_type == E_DEFINE) {
            Define* def = dynamic_cast<Define*>(es[i].get());
            if (def) {
                internal_defs.push_back({def->var,def->e});//保存变量名和表达式
                first_non_define = i+1;//记录第一个非define的位置
            }else {
                break;//转化失败，停止扫描
            }
        }else {
            break;//非define类型，停止
        }
    }

    if (internal_defs.empty()) {
        Value result = es[0]->eval(e);
        for (size_t i = 1; i < es.size(); i++) {
            Value temp = es[i]->eval(e);
            result = temp;
        }
        return result;
    }

    //有内部定义,类似letrec
        //创建新环境，为所有定义的变量创建占位符绑定
    Assoc new_env = e;
    for (const auto& def : internal_defs) {
        // 使用 VoidV() 作为占位符，表示"已定义但未初始化"
        new_env = extend(def.first, VoidV(), new_env);
    }
        //在新环境中求值所有定义的表达式，此时所有变量名都在环境中，支持相互引用
    for (const auto& def : internal_defs) {
        Value value = def.second->eval(new_env);//jisuan计算表达式的实际值
        //更新绑定
        modify(def.first,value,new_env);
    }

    //新环境中执行剩余表达式，如果没有就返回空
    if (first_non_define >= es.size()) {
        return VoidV();
    }

    Value result = es[first_non_define]->eval(new_env);//第一个
    //剩下的
    for (size_t i = first_non_define + 1; i < es.size(); i++) {
        Value temp = es[i]->eval(new_env);
        result = temp;
    }
    return result;



}
//
// Value syntaxToValue(const Syntax &syntax) {
//     if (auto num = dynamic_cast<Number*>(syntax.get())) {
//         return IntegerV(num->n);
//     }
//     if (auto true_syntax = dynamic_cast<TrueSyntax*>(syntax.get())) {
//         return BooleanV(true);
//     }
//     if (auto false_syntax = dynamic_cast<FalseSyntax*>(syntax.get())) {
//         return BooleanV(false);
//     }
//     if (auto sym = dynamic_cast<SymbolSyntax*>(syntax.get())) {
//         return SymbolV(sym->s);
//     }
//     if (auto str = dynamic_cast<StringSyntax*>(syntax.get())) {
//         return StringV(str->s);
//     }
//     if (auto lst = dynamic_cast<List*>(syntax.get())) {
//         //获取列表中所有元素
//         auto stxs_get = lst->stxs;
//         if (stxs_get.empty()) {
//             return NullV();
//         }
//         if (stxs_get.size() == 1) {
//             return PairV(syntaxToValue(stxs_get[0]),NullV());
//         }
//         int dot_pos = -1;
//         int dot_count = 0;
//         int len = stxs_get.size();
//         for (int i = 0;i < len;i++) {
//             if (auto sym = dynamic_cast<SymbolSyntax*>(stxs_get[i].get())) {
//                 if (sym->s == ".") {
//                     dot_pos = i;
//                     dot_count++;
//                 }
//             }
//         }
//         if (dot_count > 1 || (dot_count == 1 && dot_pos != len-2) ||(dot_count == 1 && len < 3)) {
//             throw RuntimeError("Syntax Error");
//         }
//         if (len == 3 && dot_pos == 1) {
//             if (auto dot_sym = dynamic_cast<SymbolSyntax*>(stxs_get[dot_pos].get())) {
//                 if (dot_sym->s == ".") {
//                     return PairV(syntaxToValue(stxs_get[0]),syntaxToValue(stxs_get[2]));
//                 }
//             }
//         }
//         List* right_list = new List();
//         right_list->stxs = std::vector<Syntax>(stxs_get.begin()+1, stxs_get.end());
//         //递归
//         return PairV(syntaxToValue(stxs_get[0]),syntaxToValue(Syntax(right_list)));
//     }
//
//     return NullV();
// }
// Value syntaxToValue(const Syntax &syntax) {
//     if (auto num = dynamic_cast<Number*>(syntax.get())) {
//         return IntegerV(num->n);
//     }
//     if (auto rational = dynamic_cast<RationalSyntax*>(syntax.get())) {
//         return RationalV(rational->numerator, rational->denominator);
//     }
//     if (auto true_syntax = dynamic_cast<TrueSyntax*>(syntax.get())) {
//         return BooleanV(true);
//     }
//     if (auto false_syntax = dynamic_cast<FalseSyntax*>(syntax.get())) {
//         return BooleanV(false);
//     }
//     if (auto sym = dynamic_cast<SymbolSyntax*>(syntax.get())) {
//         return SymbolV(sym->s);
//     }
//     if (auto str = dynamic_cast<StringSyntax*>(syntax.get())) {
//         return StringV(str->s);
//     }
//     if (auto lst = dynamic_cast<List*>(syntax.get())) {
//         // 处理点符号表示的不规范列表
//         auto &elements = lst->stxs;
//
//         // 检查点符号
//         int dotPos = -1;
//         for (size_t i = 0; i < elements.size(); i++) {
//             if (auto sym = dynamic_cast<SymbolSyntax*>(elements[i].get())) {
//                 if (sym->s == ".") {
//                     if (dotPos != -1) {
//                         throw RuntimeError("Multiple dots in list");
//                     }
//                     dotPos = i;
//                 }
//             }
//         }
//
//         if (dotPos != -1) {
//             // 处理带点符号的列表 (a b . c)
//             if (dotPos == 0 || dotPos == elements.size() - 1) {
//                 throw RuntimeError("Invalid dot position");
//             }
//             if (elements.size() != dotPos + 2) {
//                 throw RuntimeError("Dot must be followed by exactly one element");
//             }
//
//             Value carPart = syntaxToValue(elements[0]);
//             Value cdrPart = syntaxToValue(elements[elements.size() - 1]);
//
//             // 构建中间的 Pair 链
//             Value current = carPart;
//             for (size_t i = 1; i < dotPos; i++) {
//                 current = PairV(current, syntaxToValue(elements[i]));
//             }
//             // 最后一个元素的 cdr 指向点符号后的元素
//             if (auto pair = dynamic_cast<Pair*>(current.get())) {
//                 pair->cdr = cdrPart;
//             }
//             return current;
//         } else {
//             // 规范列表：递归构建 Pair 链，以 NullV() 结尾
//             if (elements.empty()) {
//                 return NullV();
//             }
//
//             Value result = NullV();
//             for (int i = elements.size() - 1; i >= 0; i--) {
//                 result = PairV(syntaxToValue(elements[i]), result);
//             }
//             return result;
//         }
//     }
//
//     throw RuntimeError("Unsupported syntax type in quote");
// }
Value syntaxToValue(const Syntax &syntax) {
    if (auto num = dynamic_cast<Number*>(syntax.get())) {
        return IntegerV(num->n);
    }
    if (auto rational = dynamic_cast<RationalSyntax*>(syntax.get())) {
        return RationalV(rational->numerator, rational->denominator);
    }
    if (auto true_syntax = dynamic_cast<TrueSyntax*>(syntax.get())) {
        return BooleanV(true);
    }
    if (auto false_syntax = dynamic_cast<FalseSyntax*>(syntax.get())) {
        return BooleanV(false);
    }
    if (auto sym = dynamic_cast<SymbolSyntax*>(syntax.get())) {
        return SymbolV(sym->s);
    }
    if (auto str = dynamic_cast<StringSyntax*>(syntax.get())) {
        return StringV(str->s);
    }
    if (auto lst = dynamic_cast<List*>(syntax.get())) {
        auto &elements = lst->stxs;

        // 处理空列表
        if (elements.empty()) {
            return NullV();
        }

        // 检查点符号
        for (size_t i = 0; i < elements.size(); i++) {
            if (auto sym = dynamic_cast<SymbolSyntax*>(elements[i].get())) {
                if (sym->s == ".") {
                    // 找到点符号，创建不规范的列表
                    if (i == 0 || i == elements.size() - 1) {
                        throw RuntimeError("Invalid dot position");
                    }
                    if (elements.size() != i + 2) {
                        throw RuntimeError("Dot must be followed by exactly one element");
                    }

                    // 构建 car 部分
                    Value carPart = NullV();
                    for (int j = i - 1; j >= 0; j--) {
                        if (carPart.get() == nullptr) {
                            carPart = syntaxToValue(elements[j]);
                        } else {
                            carPart = PairV(syntaxToValue(elements[j]), carPart);
                        }
                    }

                    // 构建最终的 pair
                    Value cdrPart = syntaxToValue(elements[i + 1]);
                    if (auto lastPair = dynamic_cast<Pair*>(carPart.get())) {
                        // 找到最后一个 pair，设置其 cdr
                        Value current = carPart;
                        while (auto pair = dynamic_cast<Pair*>(current.get())) {
                            if (pair->cdr->v_type == V_NULL) {
                                pair->cdr = cdrPart;
                                return carPart;
                            }
                            current = pair->cdr;
                        }
                    } else {
                        // 单个元素的点符号： (a . b)
                        return PairV(carPart, cdrPart);
                    }
                }
            }
        }

        // 没有点符号，构建规范列表
        Value result = NullV();
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = PairV(syntaxToValue(elements[i]), result);
        }
        return result;
    }

    throw RuntimeError("Unsupported syntax type in quote");
}
//#######
Value Quote::eval(Assoc& e) {
    //TODO: To complete the quote logic
    return syntaxToValue(s);
}


bool check_true(const Value &v) {
    if (v->v_type == V_BOOL) {
        Boolean* bool_val = dynamic_cast<Boolean*>(v.get());
        return bool_val->b;
    }
    return true;
}

Value AndVar::eval(Assoc &e) { // and with short-circuit evaluation
    //TODO: To complete the and logic
    if (rands.empty()) {
        return BooleanV(true);
    }
    Value result = BooleanV(true);
    for (auto &expr : rands) {
        result = expr->eval(e);
        if (!check_true(result)) {
            return BooleanV(false);
        }
    }
    //修改：如果都不为#f，返回最后一个参数的值，而不是#t
    return rands.back()->eval(e);
}

Value OrVar::eval(Assoc &e) { // or with short-circuit evaluation
    //TODO: To complete the or logic
    if (rands.empty()) {
        return BooleanV(false);
    }

    for (auto &expr : rands) {
        Value result = expr->eval(e);
        if (check_true(result)) {
            return result;//短路返回第一个真值
        }
    }
    return BooleanV(false);//所有表达式都假，返回最后一个值
}

Value Not::evalRator(const Value &rand) { // not
    //TODO: To complete the not logic
    return BooleanV(!check_true(rand));
}

Value If::eval(Assoc &e) {
    //TODO: To complete the if logic
    Value result = cond->eval(e);
    if (check_true(result)) {
        return conseq->eval(e);
    }else {
        return alter->eval(e);
    }
}

Value Cond::eval(Assoc &env) {
    //TODO: To complete the cond logic
    for (auto &clause : clauses) {
        if (clause.empty()) {
            continue;
        }

        if (clause[0]->e_type == E_VAR) {
            auto var_expr = dynamic_cast<Var*>(clause[0].get());
            if (var_expr!=nullptr && var_expr->x == "else") {
                if (clause.size() == 1) {
                    return VoidV();
                }
                else {
                    Value result = VoidV();
                    for (size_t i = 1; i < clause.size(); ++i) {
                        result = clause[i]->eval(env);
                    }
                    return result;
                }
            }
        }
        else {
            Value condition_value = clause[0]->eval(env);
            bool check;
            //只有#f是假
            if (condition_value->v_type == V_BOOL) {
                auto bool_val = dynamic_cast<Boolean*>(condition_value.get());
                check = bool_val->b;
            }
            //非布尔值都是真
            else {
                check = true;
            }

            if (check) {
                if (clause.size() == 1) {
                    return condition_value;
                }
                else {
                    Value result = VoidV();
                    for (size_t i = 1; i < clause.size(); ++i) {
                        result = clause[i]->eval(env);
                    }
                    return result;
                }
            }
        }

    }
    return VoidV();
}

Value Lambda::eval(Assoc &env) {
    //TODO: To complete the lambda logic
    Assoc new_env = env;
    return ProcedureV(x,e,new_env);//创建一个闭包，包括参数列表，函数体，定义时的环境
}

Value Apply::eval(Assoc &e) {

    Value proc_value = rator->eval(e);
     if (proc_value->v_type != V_PROC) {//不是函数类型
         throw RuntimeError("Attempt to apply a non-procedure");
     }

     //TODO: TO COMPLETE THE CLOSURE LOGIC
     Procedure* clos_ptr = dynamic_cast<Procedure*>(proc_value.get());//把基类指针 ValueBase* 转换为具体的 Procedure*

     //TODO: TO COMPLETE THE ARGUMENT PARSER LOGIC
     std::vector<Value> args;
     for (auto &arg_expr : rand) {
         args.push_back(arg_expr->eval(e));
     }
     // if (auto varNode = dynamic_cast<Variadic*>(clos_ptr->e.get())) {
     //     //TODO
     //
     // }
     if (args.size() != clos_ptr->parameters.size()) throw RuntimeError("Wrong number of arguments");

     //TODO: TO COMPLETE THE PARAMETERS' ENVIRONMENT LOGIC
     //创建临时环境（只在函数调用期间存在，返回时自动销毁）
     Assoc param_env = clos_ptr->env;
     for (size_t i = 0; i < clos_ptr->parameters.size(); i++) {
         param_env = extend(clos_ptr->parameters[i],args[i], param_env);
     }

     return clos_ptr->e->eval(param_env);

}


Value Define::eval(Assoc &env) {
    //TODO: To complete the define logic
    if (primitives.count(var) || reserved_words.count(var)) {
        throw RuntimeError("Undefined variable");
    }
    env = extend(var,Value(nullptr),env);
    Value value = e->eval(env);//e是Define结构体的表达式成员，->eval(env)是调用该表达式的求值方法
    modify(var,value,env);//在环境中创建或修改变量绑定
    return VoidV();
}


// Value Let::eval(Assoc &env) {
//     //TODO: To complete the let logic
//     std::vector<Value> values;
//     for (auto &b :bind) {
//         //对每个<string,Expr>的第二个元素（值表达式），在外部环境env中求值
//         Value value = b.second->eval(env);
//         values.push_back(value);
//     }
//     //创建新环境，添加局部变量绑定
//     //从当前环境开始，把所有的绑定都添加到新环境中
//     Assoc new_env = env;
//     for (size_t i = 0; i < values.size(); i++) {
//         std::string var_name = bind[i].first;
//         Value var_value = values[i];
//         new_env = extend(var_name,var_value,new_env);
//     }
//     return body->eval(new_env);
// }

Value Let::eval(Assoc &env) {


    // 求值所有绑定
    std::vector<Value> values;
    for (auto &b : bind) {

        Value value = b.second->eval(env);
        values.push_back(value);


    }

    // 创建新环境
    Assoc new_env = env;
    for (size_t i = 0; i < values.size(); i++) {

        new_env = extend(bind[i].first, values[i], new_env);

        // 验证扩展是否成功
        Value test = find(bind[i].first, new_env);
        if (test.get() != nullptr) {

        } else {

        }
    }

// Value Letrec::eval(Assoc &env) {//实现局部递归函数
//     //TODO: To complete the letrec logic
//     // struct Letrec : ExprBase {
//     //     std::vector<std::pair<std::string, Expr>> bind;
//     //     Expr body;
//     //     Letrec(const std::vector<std::pair<std::string, Expr>> &, const Expr &);
//     //     virtual Value eval(Assoc &) override;
//     // };
//     //构造函数
//     // Letrec::Letrec(const std::vector<std::pair<std::string, Expr>> &bind, const Expr &body)
//     // : ExprBase(E_LETREC), bind(bind), body(body) {} 这里初始化
//
//     Assoc env1 = env;//占位绑定
//     for (auto &binding : bind) {
//         env1 = extend(binding.first, Value(nullptr),env1);
//     }
//     //再env1中求值所有绑定表达式
//     //求值之前，所有的变量名（extend(binding.first）都已经定义了，虽然是空值，使得函数调用时不会因为递归引用而报错
//     std::vector<Value> values;
//     for (auto &binding : bind) {
//         Value value = binding.second->eval(env1);
//         values.push_back(value);
//     }
//     //binding.second = (lambda (n) ...
//     // 在 env1 中求值这个 lambda
//     // 在 env1 中查找 fact → 找到 nullptr
//     // 创建闭包：{参数:n, 函数体:(if ...), 环境:env1}
//     //虽然 fact 是 nullptr，但创建闭包时不执行函数体
//     //如果不是lambda，立即求值，不能递归引用
//     //创建新环境重新绑定
//     Assoc new_env = env;
//     for (size_t i = 0; i < values.size(); i++) {
//         new_env = extend(bind[i].first,values[i],new_env);
//     }
//     return body->eval(new_env);
// }
Value Letrec::eval(Assoc &env) {
    // 第一步：创建包含占位符的环境
    Assoc env1 = env;
    for (auto &binding : bind) {
        env1 = extend(binding.first, Value(nullptr), env1);
    }

    // 第二步：在包含占位符的环境中求值所有绑定
    std::vector<Value> values;
    for (auto &binding : bind) {
        Value value = binding.second->eval(env1);  // 关键：在 env1 中求值
        values.push_back(value);
    }

    // 第三步：更新环境中的占位符为实际值
    Assoc current_env = env1;
    for (size_t i = 0; i < values.size(); i++) {
        modify(bind[i].first, values[i], current_env);
    }

    // 第四步：在更新后的环境中执行 body
    return body->eval(env1);  // 使用 env1，不是 new_env
}

Value Set::eval(Assoc &env) {//修改当前环境中找到的第一个该变量
    //TODO: To complete the set logic
    // struct Set : ExprBase {
    //     std::string var;  // 要修改的变量名
    //     Expr e;           // 新值的表达式
    //     Set(const std::string &, const Expr &);
    //     virtual Value eval(Assoc &) override;
    // };
    Value new_value = e->eval(env);
    //find函数沿着环境链查找，返回找到的第一个
    Value old_value = find(var,env);
    if (old_value.get()==nullptr) {
        throw RuntimeError("No such variable");
    }
    modify(var,new_value,env);
    return VoidV();

}

Value Display::evalRator(const Value &rand) { // display function
    if (rand->v_type == V_STRING) {
        String* str_ptr = dynamic_cast<String*>(rand.get());
        std::cout << str_ptr->s;
    } else {
        rand->show(std::cout);
    }

    return VoidV();
}
