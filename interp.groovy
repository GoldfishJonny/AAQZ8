// Interpreter Implementation

//TOO RUN: groovy interp.groovy


def interp(ast, env) {
    def type = ast[0]

    switch (type) {
        case "program":
            ast[1].each { stmt -> interp(stmt, env) }
            break

        case "var":
            def varName = ast[1]
            def value = interp(ast[2], env)
            env[varName] = value
            break

        case "+":
            def left = interp(ast[1], env)
            def right = interp(ast[2], env)
            return left + right

        case "-":
            def left = interp(ast[1], env)
            def right = interp(ast[2], env)
            return left - right

        case "number":
            return ast[1]

        case "identifier":
            def varName = ast[1]
            if (!env.containsKey(varName)) {
                throw new Exception("Undefined variable: $varName")
            }
            return env[varName]

        default:
            throw new Exception("Unknown AST node: $type")
    }
}

// Test Cases
println("Running Tests...")

// Test 1: Variable Declaration and Addition
def ast1 = [
    "program", [
        ["var", "x", ["number", 5]],
        ["+", ["identifier", "x"], ["identifier", "x"]]
    ]
]
def env1 = [:]
def result1 = interp(ast1, env1)
println("Test 1 Result: $result1") // Expected: 10

// Test 2: Simple Arithmetic
def ast2 = [
    "+", ["number", 10], ["number", 20]
]
def env2 = [:]
def result2 = interp(ast2, env2)
println("Test 2 Result: $result2") // Expected: 30