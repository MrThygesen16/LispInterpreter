# Simple Interpreter

This is an interpreter for a simple LISP-like language.


For example, a simple addition expression:

```scala
 "FUNCTIONS: add" should "add two numbers" in {
    assert(add(LNumber(4), LNumber(3)) == LNumber(7))
  }
```

The way this program would look when implemented in our language would be the folllowing:


```lisp
(+ 4 3)
```

Lisp uses what's known as Reverse Polish Notation.


Furthermore, the program can also handle much more complex instructions, including `if`, `eq`. Furthermore, this simple implementation follows correct precedence and association rules (i.e. order of operations).

```scala
it should "handle if with expressions: (if (eq 3 3) (+ 2 3) (* 4 5))" in {
    assert(eval(LList(LSymbol("if"),
                      LList(LList(LSymbol("eq"),
                                  LList(LNumber(3),
                                        LList(LNumber(3),LSymbol("nil")))),
                            LList(LList(LSymbol("+"),
                                        LList(LNumber(2),
                                              LList(LNumber(3),
                                                    LSymbol("nil")))),
                                  LList(LList(LSymbol("*"),
                                              LList(LNumber(4),
                                                    LList(LNumber(5),
                                                          LSymbol("nil")))),
                                        LSymbol("nil"))))))
           == LNumber(5))
  }
```
