(ns js-gen.test.core
  (:use js-gen.core :reload-all)
  (:use
     mini-kanren.core
     clojure.test))

; unit tests
;
(deftest test-js-gen
(is (= "a, b, c"
		   (infix-atom-list ", " '(a b c))))

(is (= "a, \"a string\", c"
		   (infix-atom-list ", " '(a "a string" c))))

(is (= '("foo()")
		   (run out-str
				(js-funcall '(foo) out-str))))

(is (= '("foo(a)")
		   (run out-str
				(js-funcall '(foo a) out-str))))

(is (= '("foo(a, b)")
		   (run out-str
				(js-funcall '(foo a b) out-str))))

(is (= "a"
		   (dot-list '(a))))

(is (= "a.b.c"
		   (dot-list '(a b c))))

(is (= "a.b(c, d)"
		   (dot-list '(a (b c d)))))

(is (= "a().b(c, d)"
		   (dot-list '((a) (b c d)))))

(is (= '("obj.prop")
		   (run out-str
				(js-dot-operator '(. obj prop) out-str))))

(is (= '("(1 + 2)")
		   (run out-str
				(js-operator '(+ 1 2) out-str))))

(is (= '("function(){\n}")
		   (run out-str
				(js-fn '(fn []) out-str))))

(is (= '("(1 + (a - b))")
		   (run out-str
				(js-expr '(+ 1 (- a b)) out-str))))

(is (= '("foo()")
		   (run out-str
				(js-expr '(foo) out-str))))

(is (= '("foo(a)")
		   (run out-str
				(js-expr '(foo a) out-str))))

(is (= '("foo(\"a string\")")
		   (run out-str
				(js-expr '(foo "a string") out-str))))

(is (= '("foo(a, b)")
		   (run out-str
				(js-expr '(foo a b) out-str))))

(is (= '("foo(bar(a, b))")
		   (run out-str
				(js-expr '(foo (bar a b)) out-str))))

(is (= '("obj.prop")
		   (run out-str
				(js-expr '(. obj prop) out-str))))

(is (= '("obj.meth(foo(a, b)).prop")
		   (run out-str
				(js-expr '(. obj (meth (foo a b)) prop) out-str))))

(is (= '("foo(c, d).meth(foo(a, b)).prop")
		   (run out-str
				(js-expr '(. (foo c d) (meth (foo a b)) prop) out-str))))

(is (= '("(1 + foo(a, x))")
		   (run out-str
				(js-expr '(+ 1 (foo a x)) out-str))))

(is (= '("(obj.prop mod foo(a, x))")
		   (run out-str
				(js-expr '(mod (. obj prop) (foo a x)) out-str))))

(is (= '("(obj.prop mod foo(a, x))")
		   (run out-str
				(js-expr '(mod obj.prop (foo a x)) out-str))))

(is (#{'("{prop2:\"str-val\",prop1:3}")
       '("{prop1:3,prop2:\"str-val\"}")}
		   (run out-str
				(js-expr '{prop1 3, prop2 "str-val"} out-str))))

(is (= '("[1,3,\"str-val\"]")
		   (run out-str
				(js-expr '[1 3 "str-val"] out-str))))

(is (#{'("foo({prop2:\"str-val\",prop1:3})")
       '("foo({prop1:3,prop2:\"str-val\"})")}
		   (run out-str
				(js-expr '(foo {prop1 3, prop2 "str-val"}) out-str))))

(is (= '("foo([1,3,\"str-val\"])")
		   (run out-str
				(js-expr '(foo [1 3 "str-val"]) out-str))))

(is (= '("z = 1")
		   (run out-str 
				(js-assignment '(= z 1) out-str))))

(is (= '("z = (1 + x)")
		   (run out-str 
				(js-assignment '(= z (+ 1 x)) out-str))))

(is (= '("z = foo((1 + x))")
		   (run out-str 
				(js-assignment '(= z (foo (+ 1 x))) out-str))))

(is (#{'("obj = {prop2:\"str-val\",prop1:3}")
       '("obj = {prop1:3,prop2:\"str-val\"}")}
		   (run out-str
				(js-assignment '(= obj {prop1 3, prop2 "str-val"}) out-str))))

(is (= '("arr = [1,3,\"str-val\"]")
		   (run out-str
				(js-assignment '(= arr [1 3 "str-val"]) out-str))))

(is (= '("var z = 1")
		   (run out-str 
				(js-var '(var z 1) out-str))))

(is (= '("var str = \"another string\"")
		   (run out-str
				(js-var '(var str "another string") out-str))))

(is (#{'("var obj = {prop2:\"str-val\",prop1:3}")
       '("var obj = {prop1:3,prop2:\"str-val\"}")}
		   (run out-str
				(js-var '(var obj {prop1 3, prop2 "str-val"}) out-str))))

(is (= '("var arr = [1,3,\"str-val\"]")
		   (run out-str
				(js-var '(var arr [1 3 "str-val"]) out-str))))

(is (= '("delete MyObj")
		   (run out-str
				(js-delete '(delete MyObj) out-str))))

(is (= '("new MyObj(x, 1, z)")
		   (run out-str
				(js-new '(new MyObj x 1 z) out-str))))

(is (= '("foo();")
		   (run out-str
				(js-statement '(foo) out-str))))

(is (= '("foo(a, b, c);")
		   (run out-str
				(js-statement '(foo a b c) out-str))))

(is (= '("var z = 1;")
		   (run out-str 
				(js-statement '(var z 1) out-str))))

(is (#{'("var obj = {prop2:\"str-val\",prop1:3};")
       '("var obj = {prop1:3,prop2:\"str-val\"};")}
		   (run out-str
				(js-statement '(var obj {prop1 3, prop2 "str-val"}) out-str))))

(is (= '("var arr = [1,3,\"str-val\"];")
		   (run out-str
				(js-statement '(var arr [1 3 "str-val"]) out-str))))

(is (= '("new MyObj(x, 1, z);")
		   (run out-str
				(js-statement '(new MyObj x 1 z) out-str))))

(is (= '("ab = 152;")
		   (run out-str
				(js-statement '(= ab 152) out-str))))

(is (= '("ab = foo();")
		   (run out-str
				(js-statement '(= ab (foo)) out-str))))

(is (= '("ab = foo(x, y);")
		   (run out-str
				(js-statement '(= ab (foo x y)) out-str))))

(is (= '("ab = obj.foo(x, y);")
		   (run out-str
				(js-statement '(= ab (. obj (foo x y))) out-str))))

(is (= '("obj.foo(x, y);")
		   (run out-str
				(js-statement '(. obj (foo x y)) out-str))))

(is (= '("ab = (x + y);")
		   (run out-str
				(js-statement '(= ab (+ x y)) out-str))))

(is (= '("if(a)\n{\nfoo();\n}\n")
		   (run out-str
				(js-statement '(if a (foo)) out-str))))

(is (= '("if(a || b)\n{\nfoo();\n}\n")
		   (run out-str
				(js-statement '(if (|| a b) (foo)) out-str))))

(is (= '("if(a && b)\n{\nfoo();\n}\nelse\n{\nbar(x, y);\n}\n")
		   (run out-str
				(js-statement '(if (&& a b)
								 (foo)
								 (bar x y)) out-str))))

(is (= '("if(a && b)\n{\nfoo();\n}\nelse if(true)\n{\nbar(x, y);\n}\n")
		   (run out-str
				(js-statement '(cond
								 (&& a b) (foo)
								 true (bar x y))
							  out-str))))

(is (= '("if(a && b)\n{\nfoo();\n}\n")
		   (run out-str
				(js-statement '(cond
								 (&& a b) (foo))
							  out-str))))

(is (= '("if(a && b)\n{\nfoo();\nbar(x, y, 1);\n}\n")
		   (run out-str
				(js-statement '(if (&& a b)
								 (do
								   (foo)
								   (bar x y 1)))
							  out-str))))

(is (= '("var newFn = function(a, b){\nstmt1(a);\nstmt2(b);\nreturn x.meth1().meth2().prop1;\n};")
		   (run out-str
				(js-statement '(var newFn (fn [a b]
											  (stmt1 a)
											  (stmt2 b)
											  (. x (meth1) (meth2) prop1))) out-str))))

(is (= '("var empty = function(){\n};")
		   (run out-str
				(js-defn '(defn empty [])
						 out-str))))

(is (= '("var new-fn = function(x, y){\nstmt(x);\nreturn y.meth1();\n};")
		   (run out-str
				(js-defn '(defn new-fn [x y]
								(stmt x)
								(. y (meth1)))
						 out-str))))

(is (= 'a (expr-item 'a)))

(is (= "foo();"
		   (js-gen
			 '(foo))))

(is (= "foo(a, b, c);"
		   (js-gen
			 '(foo a b c))))

(is (= "var foo = function(a, b, c){\nreturn stmt1(stmt2(c, b));\n};"
		   (js-gen
			 '(var foo (fn [a b c]
						   (stmt1
							 (stmt2 c b)))))))

(is (= "x.meth1().meth2().prop1;"
		   (js-gen '(. x (meth1) (meth2) prop1))))

(is (= "var a = (1 + (b * 2) + (c / 3));"
		   (js-gen '(var a (+ 1 (* b 2) (/ c 3))))))

(is (= "var a = obj1.prop1;"
		   (js-gen '(var a (. obj1 prop1)))))

(is (= "var a = 1;\nvar b = 2;"
		   (javascript (var a 1)
					   (var b 2))))

(is (= "var new-obj = object(old-obj);"
		   (javascript (var new-obj (object old-obj)))))

(is (= "if(a && b)\n{\nfoo();\n}\nelse if(true)\n{\nbar(x, y);\n}\n"
		   (javascript (cond
						 (&& a b) (foo)
						 true (bar x y)))))

(is (= "if(a && b)\n{\nfoo();\nbar(a, 1);\n}\nelse\n{\nbar(x, y);\n}\n"
		   (javascript (if (&& a b)
						 (do
						   (foo)
						   (bar a 1))
						 (bar x y)))))

; Douglas Crockford's 'object' function that abstracts away the need to use 'new' to create new objects
(is (= "var object = function(o){\nvar F = function(){\n};\nF.prototype = o;\nreturn new F();\n};"
		   (javascript (defn object [o]
							 (defn F [])
							 (= (. F prototype) o)
							 (new F)))))
)

(defmacro jquery [selector & body]
  (list* '. (list 'jQuery selector) body))


