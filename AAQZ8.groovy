abstract class ExprC {}

class numC extends ExprC {
    int num;
    numC(int num) {
        this.num = num;
    }
}

class idC extends ExprC {
    String id;
    idC(String id) {
        this.id = id;
    }
}

class strC extends ExprC {
    String str;
    strC(String str) {
        this.str = str;
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
}

class lamC extends ExprC {
    String[] args;
    ExprC body;
    lamC(String[] args, ExprC body) {
        this.args = args;
        this.body = body;
    }
}

class appC extends ExprC {
    ExprC fun;
    ExprC[] args;
    appC(ExprC fun, ExprC[] args) {
        this.fun = fun;
        this.args = args;
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
}

class primV extends Value {
    String name;
    primV(String name) {
        this.name = name;
    }
}

class bindC {
    String id;
    Value val;
    bindC(String id, Value val) {
        this.id = id;
        this.val = val;
    }
}

class envC {
    bindC[] binds;
    envC(bindC[] binds) {
        this.binds = binds;
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

// Test Cases
def ast1 = new numC(5)
if (interp(ast1, topEnv).toString() == "5") {
    println("Test 1 Passed")
} else {
    println("Test 1 Failed")
}

def ast2 = new strC("hello")
if (interp(ast2, topEnv).toString() == "hello") {
    println("Test 2 Passed")
} else {
    println("Test 2 Failed")
}

def list1 = new ExprC[2]
list1[0] = new numC(3)
list1[1] = new numC(4)

def ast3 = new appC(new idC("+"), list1)
if (interp(ast3, topEnv).toString() == "7") {
    println("Test 3 Passed")
} else {
    println("Test 3 Failed")
}

def ast4 = new appC(new idC("-"), list1)
if (interp(ast4, topEnv).toString() == "-1") {
    println("Test 4 Passed")
} else {
    println("Test 4 Failed")
}

def ast5 = new appC(new idC("*"), list1)
if (interp(ast5, topEnv).toString() == "12") {
    println("Test 5 Passed")
} else {
    println("Test 5 Failed")
}

def ast6 = new appC(new idC("/"), list1)
if (interp(ast6, topEnv).toString() == "0") {
    println("Test 6 Passed")
} else {
    println("Test 6 Failed")
}

def ast7 = new appC(new idC("<="), list1)
if (interp(ast7, topEnv).toString() == "true") {
    println("Test 7 Passed")
} else {
    println("Test 7 Failed")
}

def ast8 = new appC(new idC("equal?"), list1)
if (interp(ast8, topEnv).toString() == "false") {
    println("Test 8 Passed")
} else {
    println("Test 8 Failed")
}

def list2 = new ExprC[1]
list2[0] = new strC("hi")
def ast9 = new appC(new idC("println"), list2)
if (interp(ast9, topEnv).toString() == "true") {
    println("Test 9 Passed")
} else {
    println("Test 9 Failed")
}

def ast10 = new appC(new idC("read-num"), new ExprC[0])
println("Enter a number:")
if (interp(ast10, topEnv) instanceof numV) {
    println("Test 10 Passed")
} else {
    println("Test 10 Failed")
}

def ast11 = new appC(new idC("read-str"), new ExprC[0])
println("Enter a string:")
if (interp(ast11, topEnv) instanceof strV) {
    println("Test 11 Passed")
} else {
    println("Test 11 Failed")
}

def list3 = new ExprC[2]
def list4 = new ExprC[1]
list4[0] = new strC("hello ")
def list5 = new ExprC[1]
list5[0] = new strC("world")
list3[0] = (new appC(new idC("println"), list4))
list3[1] = (new appC(new idC("println"), list5))
def ast12 = new appC(new idC("seq"), list3)
if (interp(ast12, topEnv).toString() == "true") {
    println("Test 12 Passed")
} else {
    println("Test 12 Failed")
}

def list6 = new ExprC[2]
list6[0] = new strC("hello ")
list6[1] = new strC("world")
def ast13 = new appC(new idC("++"), list6)
if (interp(ast13, topEnv).toString() == "hello world") {
    println("Test 13 Passed")
} else {
    println("Test 13 Failed")
}

