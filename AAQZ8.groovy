abstract class ExprC {}

class numC extends ExprC {
    int num;
    numC(int num) {
        this.num = num;
    }
    String toString() {
        return num.toString();
    }
}

class idC extends ExprC {
    String id;
    idC(String id) {
        this.id = id;
    }
    String toString() {
        return id;
    }
}

class strC extends ExprC {
    String str;
    strC(String str) {
        this.str = str;
    }
    String toString() {
        return str;
    }
}

class ifC extends ExprC {
    ExprC test;
    ExprC then;
    ExprC els;
    ifC(ExprC test, ExprC then, ExprC els) {
        this.test = test;
        this.then = then;
        this.els = els;
    }
    String toString() {
        return "ifC" + " (" + test.toString() + ")" + " (" + then.toString() + ")" + " (" + els.toString() + ")";
    }
}

class lamC extends ExprC {
    String[] args;
    ExprC body;
    lamC(String[] args, ExprC body) {
        this.args = args;
        this.body = body;
    }
    String toString() {
        return "lamC"  + args.toString() + " (" + body.toString() + ")";
    }
}

class appC extends ExprC {
    ExprC fun;
    ExprC[] args;
    appC(ExprC fun, ExprC[] args) {
        this.fun = fun;
        this.args = args;
    }
    String toString() {
        return "appC" + " (" + fun.toString() + ")" + " (" + args.toString() + ")";
    }
}

abstract class Value {}

class numV extends Value {
    int num;
    numV(int num) {
        this.num = num;
    }
    String toString() {
        return num.toString();
    }
}

class boolV extends Value {
    boolean bool;
    boolV(boolean bool) {
        this.bool = bool;
    }
    String toString() {
        return bool.toString();
    }
}

class strV extends Value {
    String str;
    strV(String str) {
        this.str = str;
    }
    String toString() {
        return str;
    }
}

class cloV extends Value {
    String[] args;
    ExprC body;
    cloV(String[] args, ExprC body) {
        this.args = args;
        this.body = body;
    }
    String toString() {
        return "cloV" + args.toString() + " (" + body.toString() + ")";
    }
}

class primV extends Value {
    String name;
    primV(String name) {
        this.name = name;
    }
    String toString() {
        return name;
    }
}

class bindC {
    String id;
    Value val;
    bindC(String id, Value val) {
        this.id = id;
        this.val = val;
    }
    String toString() {
        return id + " = " + val.toString();
    }
}

class envC {
    bindC[] binds;
    envC(bindC[] binds) {
        this.binds = binds;
    }
    String toString() {
        return binds.toString();
    }
}

bindC[] bindArray = new bindC[13]
bindArray[0] = new bindC("+", new primV("+"))
bindArray[1] = new bindC("-", new primV("-"))
bindArray[2] = new bindC("*", new primV("*"))
bindArray[3] = new bindC("/", new primV("/"))
bindArray[4] = new bindC("<=", new primV("<="))
bindArray[5] = new bindC("equal?", new primV("equal?"))
bindArray[6] = new bindC("true", new boolV(true))
bindArray[7] = new bindC("false", new boolV(false))
bindArray[8] = new bindC("println", new primV("println"))
bindArray[9] = new bindC("read-num", new primV("read-num"))
bindArray[10] = new bindC("read-str", new primV("read-str"))
bindArray[11] = new bindC("seq", new primV("seq"))
bindArray[12] = new bindC("++", new primV("++"))

def topEnv = new envC(bindArray)

def interp (ast, env) {
    if (ast instanceof numC) {
        return new numV(ast.num);
    } else if (ast instanceof idC) {
        for (bind in env.binds) {
            if (bind.id == ast.id) {
                return bind.val;
            }
        }
        throw new Exception("Unbound identifier: " + ast.id);
    } else if (ast instanceof strC) {
        return new strV(ast.str);
    } else if (ast instanceof ifC) {
        def test = interp(ast.test, env);
        if (test instanceof boolV) {
            if (test.bool) {
                return interp(ast.then, env);
            } else {
                return interp(ast.els, env);
            }
        } else {
            throw new Exception("Non-boolean test in if: " + test);
        }
    } else if (ast instanceof lamC) {
        return new cloV(ast.args, ast.body);
    } else if (ast instanceof appC) {
        def fun = interp(ast.fun, env);
        if (fun instanceof cloV) {
            def args = new Value[ast.args.length];
            for (int i = 0; i < ast.args.length; i++) {
                args[i] = interp(ast.args[i], env);
            }
            if (fun.args.length == args.length) {
                def newEnv = new bindC[fun.args.length + env.binds.length];
                for (int i = 0; i < fun.args.length; i++) {
                    newEnv[i] = new bindC(fun.args[i], args[i]);
                }
                for (int i = 0; i < env.binds.length; i++) {
                    newEnv[i + fun.args.length] = env.binds[i];
                }
                return interp(fun.body, new envC(newEnv));
            } else {
                throw new Exception("Wrong number of arguments: " + args.length + " instead of " + fun.args.length);
            }
        } else if (fun instanceof primV) {
            if (fun.name == "+") {
                if (ast.args.length == 2) {
                    def left = interp(ast.args[0], env);
                    def right = interp(ast.args[1], env);
                    if (left instanceof numV && right instanceof numV) {
                        return new numV(left.num + right.num);
                    } else {
                        throw new Exception("Non-numeric argument to +: " + left + " or " + right);
                    }
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 2");
                }
            } else if (fun.name == "-") {
                if (ast.args.length == 2) {
                    def left = interp(ast.args[0], env);
                    def right = interp(ast.args[1], env);
                    if (left instanceof numV && right instanceof numV) {
                        return new numV(left.num - right.num);
                    } else {
                        throw new Exception("Non-numeric argument to -: " + left + " or " + right);
                    }
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 2");
                }
            } else if (fun.name == "*") {
                if (ast.args.length == 2) {
                    def left = interp(ast.args[0], env);
                    def right = interp(ast.args[1], env);
                    if (left instanceof numV && right instanceof numV) {
                        return new numV(left.num * right.num);
                    } else {
                        throw new Exception("Non-numeric argument to *: " + left + " or " + right);
                    }
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 2");
                }
            } else if (fun.name == "/") {
                if (ast.args.length == 2) {
                    def left = interp(ast.args[0], env);
                    def right = interp(ast.args[1], env);
                    if (left instanceof numV && right instanceof numV) {
                        if (right.num == 0) {
                            throw new Exception("Division by zero");
                        }
                        int result = left.num / right.num;
                        return new numV(result);
                    } else {
                        throw new Exception("Non-numeric argument to /: " + left + " or " + right);
                    }
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 2");
                }
            } else if (fun.name == "<=") {
                if (ast.args.length == 2) {
                    def left = interp(ast.args[0], env);
                    def right = interp(ast.args[1], env);
                    if (left instanceof numV && right instanceof numV) {
                        return new boolV(left.num <= right.num);
                    } else {
                        throw new Exception("Non-numeric argument to <=: " + left + " or " + right);
                    }
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 2");
                }
            } else if (fun.name == "equal?") {
                if (ast.args.length == 2) {
                    def left = interp(ast.args[0], env);
                    def right = interp(ast.args[1], env);
                    return new boolV(left == right);
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 2");
                }
            } else if (fun.name == "println") {
                if (ast.args.length == 1) {
                    def arg = interp(ast.args[0], env);
                    println(arg);
                    return new boolV(true);
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 1");
                }
            } else if (fun.name == "read-num") {
                if (ast.args.length == 0) {
                    def input = System.console().readLine();
                    return new numV(Integer.parseInt(input));
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 0");
                }
            } else if (fun.name == "read-str") {
                if (ast.args.length == 0) {
                    def input = System.console().readLine();
                    return new strV(input);
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 0");
                }
            } else if (fun.name == "seq") {
                def result = new boolV(true);
                for (int i = 0; i < ast.args.length; i++) {
                    result = interp(ast.args[i], env);
                }
                return result;
            } else if (fun.name == "++") {
                if (ast.args.length == 2) {
                    def left = interp(ast.args[0], env);
                    def right = interp(ast.args[1], env);
                    if (left instanceof strV && right instanceof strV) {
                        return new strV(left.str + right.str);
                    } else {
                        throw new Exception("Non-string argument to ++: " + left + " or " + right);
                    }
                } else {
                    throw new Exception("Wrong number of arguments: " + ast.args.length + " instead of 2");
                }
            }
                else {
                throw new Exception("Unknown primitive: " + fun.name);
            }
        } else {
            throw new Exception("Non-function in application: " + fun);
        }
    } else {
        throw new Exception("Unknown AST node: " + ast);
    }
}

def parser(Sexp) {
    if (!(Sexp instanceof List)) {
        if (Sexp instanceof Integer) {
            return new numC(Sexp);
        } else if (Sexp instanceof String) {
            if (Sexp.startsWith("\"") && Sexp.endsWith("\"")) {
                return new strC(Sexp.substring(1, Sexp.length() - 1));
            } else {
                return new idC(Sexp);
            }
        } else {
            throw new Exception("Unknown element: " + Sexp);
        }
    }

    if (Sexp.size() == 0) {
        throw new Exception("Empty array");
    }

    if (Sexp.size() == 1) {
        def args = new ExprC[0];
        return new appC(parser(Sexp[0]), args);
    }

    if ((Sexp.size() == 4) && (Sexp[0] == "if")) {
        return new ifC(parser(Sexp[1]), parser(Sexp[2]), parser(Sexp[3]));
    }

    if ((Sexp.size() == 3) && (Sexp[1] == "=>")) {
        if (!(Sexp[0] instanceof List)) {
            throw new Exception("Lambda arguments should be a list: " + Sexp[0]);
        }
        return new lamC(Sexp[0], parser(Sexp[2]));
    }

    if ((Sexp.size() >= 3) && (Sexp[0] == "bind")){
        def body = parser(Sexp[-1])
        def args = new String[Sexp.size() - 2]
        def binds = new ExprC[Sexp.size() - 2]
        for (int i = 1; i < Sexp.size() - 1; i++){
            args[i - 1] = Sexp[i][0]
            binds[i - 1] = parser(Sexp[i][2])
        }

        return new appC(new lamC(args, body), binds)
    }

    if (Sexp.size() >= 2) {
        def args = new ExprC[Sexp.size() - 1];
        for (int i = 1; i < Sexp.size(); i++) {
            args[i - 1] = parser(Sexp[i]);
        }
        return new appC(parser(Sexp[0]), args);
    }

    throw new Exception("Invalid array format: " + arr);
}

def topInterp(Sexp, topEnv) {
    parse = parser(Sexp)
    return interp(parse, topEnv)
}
// Test Cases
def checkEqual(testno, act, exp){
    def passed = "Test "+ testno + " : Passed"
    def failed = "Test "+ testno + " : Failed, Expected: " + exp + ", Actual: " + act
    def actStr = act.toString();
    println((actStr == exp)? passed : failed)
}

def ast1 = new numC(5)
checkEqual(1, interp(ast1, topEnv),"5")

def ast2 = new strC("hello")
checkEqual(2, interp(ast2, topEnv),"hello")

def list1 = new ExprC[2]
list1[0] = new numC(3)
list1[1] = new numC(4)
def ast3 = new appC(new idC("+"), list1)
checkEqual(3, interp(ast3, topEnv),"7")

def ast4 = new appC(new idC("-"), list1)
checkEqual(4, interp(ast4, topEnv),"-1")

def ast5 = new appC(new idC("*"), list1)
checkEqual(5, interp(ast5, topEnv),"12")

def ast6 = new appC(new idC("/"), list1)
checkEqual(6, interp(ast6, topEnv),"0")

def ast7 = new appC(new idC("<="), list1)
checkEqual(7, interp(ast7, topEnv),"true")


def ast8 = new appC(new idC("equal?"), list1)
checkEqual(8, interp(ast8, topEnv),"false")

def list2 = new ExprC[1]
list2[0] = new strC("hi")
def ast9 = new appC(new idC("println"), list2)
checkEqual(9, interp(ast9, topEnv),"true")

def ast10 = new appC(new idC("read-num"), new ExprC[0])
println("Enter a number:")
checkEqual(10, (interp(ast10, topEnv) instanceof numV), "true") //Instanceof is converted to a string in the checker

def ast11 = new appC(new idC("read-str"), new ExprC[0])
println("Enter a string:")
checkEqual(11, (interp(ast11, topEnv) instanceof strV), "true")

def list3 = new ExprC[2]
def list4 = new ExprC[1]
list4[0] = new strC("hello ")
def list5 = new ExprC[1]
list5[0] = new strC("world")
list3[0] = (new appC(new idC("println"), list4))
list3[1] = (new appC(new idC("println"), list5))
def ast12 = new appC(new idC("seq"), list3)
println("--------------------")
checkEqual(12, interp(ast12, topEnv), "true")
println("--------------------")

def list6 = new ExprC[2]
list6[0] = new strC("hello ")
list6[1] = new strC("world")
def ast13 = new appC(new idC("++"), list6)
checkEqual(13, interp(ast13, topEnv), "hello world")

def ast14 = new idC("true")
checkEqual(14, interp(ast14, topEnv), "true")

def ast15 = new idC("false")
checkEqual(15, interp(ast15, topEnv), "false")

def ast16 = new ifC(new idC("true"), new numC(1), new numC(2))
checkEqual(16, interp(ast16, topEnv), "1")

def list7 = new String[1]
list7[0] = "x"
def list8 = new ExprC[2]
list8[0] = new idC("x")
list8[1] = new numC(2)
def list9 = new ExprC[1]
list9[0] = new numC(1)
def ast17 = new appC(new lamC(list7, new appC(new idC("+"), list8)), list9)
checkEqual(17, interp(ast17, topEnv), "3")



def prog1 = ["+", 1, 2]
def list10 = new ExprC[2]
list10[0] = new numC(1)
list10[1] = new numC(2)
(checkEqual(18, topInterp(prog1, topEnv), "3"))

def prog2 = "\"start\""

checkEqual(19, topInterp(prog2, topEnv), "start")

def prog3 = ["read-str"]
println("Enter a string:")
checkEqual(20, (topInterp(prog3, topEnv) instanceof strV), "true")

def prog4 = ["bind", ["x", "=", 1], ["y", "=", 2], ["+", "x", "y"]]
checkEqual(21, topInterp(prog4, topEnv), "3")

def prog5 = ["bind", ["x", "=", 1], ["y", "=", 2], ["bind", ["z", "=", 3], ["+", ["+", "x", "y"], "z"]]]
checkEqual(22, topInterp(prog5, topEnv), "6")

