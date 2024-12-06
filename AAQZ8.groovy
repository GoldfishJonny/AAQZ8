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

bindC[] bindArray = new bindC[4]

bindArray[0] = new bindC("+", new primV("+"))
bindArray[1] = new bindC("-", new primV("-"))
bindArray[2] = new bindC("*", new primV("*"))
bindArray[3] = new bindC("/", new primV("/"))

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
                        return new numV(left.num / right.num);
                    } else {
                        throw new Exception("Non-numeric argument to /: " + left + " or " + right);
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