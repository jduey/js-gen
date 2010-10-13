(ns js-gen.core
  (:use mini-kanren.core :reload-all)
  (:use
     clojure.test))

(defn infix-atom-list [sep-str lst]
	  (apply str (interpose sep-str
							(map pr-str lst))))

(def js-expr)

(defn expr-item [expr-element]
	  (cond
		(string? expr-element) (pr-str expr-element)
		(or (seq? expr-element)
			(map? expr-element)
			(vector? expr-element)) (first (run out-str
												(js-expr expr-element out-str)))
		true expr-element))

(defn infix-list [sep-str lst]
	  (apply str (interpose sep-str (map expr-item lst))))

(defn infix-expr [sep-str lst]
	  (str "(" (infix-list sep-str lst) ")"))

(defn js-funcall [js-ast out-str]
	  (exist (fname parms)
			 (cons-o fname parms js-ast)
			 (&-expr out-str (str (value-of fname)
								  (infix-expr ", " (value-of parms))))))

(defn dot-item [dot-element]
	  (if (seq? dot-element)
		(first
		  (run out-str
			   (js-funcall dot-element out-str)))
		dot-element))

(defn dot-list [[fst & rst]]
	  (loop [result (str (dot-item fst)), lst rst]
			(if (nil? lst)
			  result
			  (recur (str result '. (dot-item (first lst)))
					 (next lst)))))

(defn js-dot-operator [js-ast out-str]
	  (exist (expr-list dot-parts)
			 (& (lcons '. dot-parts) js-ast)
			 (&-expr out-str (str (dot-list (value-of dot-parts))))))

(def expr-ops '(+ - * / mod || && == != === !== > >= < <=))
(defn js-operator [js-ast out-str]
	  (exist (fst rst)
			 (cons-o fst rst js-ast)
			 (member-o fst expr-ops)
			 (&-expr out-str (infix-expr (str " " (value-of fst) " ")
										 (value-of rst)))))

(def js-statement)
(defn fn-body [lst out-str]
	  (exist (stmt stmt-str rst rst-str)
			 (cons-o stmt rst lst)
			 (js-statement stmt stmt-str)
			 (cond-u
			   ((null-o rst) (&-expr out-str (str "return " (value-of stmt-str) "\n")))
			   (else (fn-body rst rst-str)
					 (&-expr out-str (str (value-of stmt-str) "\n"
										  (value-of rst-str)))))))

(defn js-fn [js-ast out-str]
	  (exist (parms body body-str)
			 (cond-u
			   ((& (list 'fn parms) js-ast)
				(& body-str ""))
			   ((append-o (list 'fn parms) body js-ast)
				(fn-body body body-str)))
			 (&-expr out-str (str "function"
								  (infix-expr ", " (seq (value-of parms)))
								  "{\n"
								  (value-of body-str)
								  "}"))))

(defn js-array-lit [arr-vec]
	  (str \[ (infix-list "," arr-vec) \]))

(defn js-object-lit [obj-map]
	  (str \{
		   (apply str (interpose ","
								 (map #(str (pr-str (key %)) \:
											(pr-str (val %)))
									  obj-map)))
		   \}))

(defn js-expr [js-ast out-str]
	  (cond-u
		[(map-o js-ast) (&-expr out-str (js-object-lit (value-of js-ast)))]
		[(vector-o js-ast) (&-expr out-str (js-array-lit (value-of js-ast)))]
		[(js-dot-operator js-ast out-str)]
		[(js-operator js-ast out-str)]
		[(js-fn js-ast out-str)]
		[(js-funcall js-ast out-str)]
		[(&-expr out-str (pr-str (value-of js-ast)))]))

(defn js-assignment [js-ast out-str]
	  (exist (lvalue lval-str rvalue rval-str)
			 (& ['= lvalue rvalue] js-ast)
			 (cond-u
			   [(js-dot-operator lvalue lval-str)]
			   [(& lval-str lvalue)])
			 (js-expr rvalue rval-str)
			 (&-expr out-str (str (value-of lval-str) " = "
								  (value-of rval-str)))))

(defn js-var [js-ast out-str]
	  (exist [var-name var-value val-str]
			 (& ['var var-name var-value] js-ast)
			 (js-expr var-value val-str)
			 (&-expr out-str (str "var " (value-of var-name)
								  " = " (value-of val-str)))))

(defn js-delete [js-ast out-str]
	  (exist (old-obj)
			 (& (list 'delete old-obj) js-ast)
			 (&-expr out-str (str 'delete " " (value-of old-obj)))))

(defn js-new [js-ast out-str]
	  (exist (constructor parms)
			 (append-o (list 'new constructor) parms js-ast)
			 (&-expr out-str (str 'new " "
								  (value-of constructor)
								  (infix-expr ", " (value-of parms))))))

(def statement-block)
(defn build-if-str [test action out-str]
	  (exist (test-str body-str)
			 (js-expr test test-str)
			 (statement-block action body-str)
			 (cond-u
			   ((list-o test) (&-expr out-str (str "if" (value-of test-str) "\n"
												   (value-of body-str))))
			   (else (&-expr out-str (str "if(" (value-of test-str) ")\n"
										  (value-of body-str)))))))

(defn js-if [js-ast out-str]
	  (exist (test action)
			 (& ['if test action] js-ast)
			 (build-if-str test action out-str)))

(defn js-if-else [js-ast out-str]
	  (exist (test action if-str else-action else-str)
			 (& (list 'if test action else-action) js-ast)
			 (build-if-str test action if-str)
			 (statement-block else-action else-str)
			 (&-expr out-str (str (value-of if-str)
								  "else\n"
								  (value-of else-str)))))

(defn elseif-list [js-ast out-str]
	  (exist (test action if-str elseif-pairs elseif-str)
			 (append-o (list test action) elseif-pairs js-ast)
			 (build-if-str test action if-str)
			 (cond-u
			   ((null-o elseif-pairs) (& out-str if-str))
			   ((elseif-list elseif-pairs elseif-str)
				(&-expr out-str (str (value-of if-str)
									 "else "
									 (value-of elseif-str)))))))

(defn js-cond [js-ast out-str]
	  (exist (elseif-pairs)
			 (cons-o 'cond elseif-pairs js-ast)
			 (elseif-list elseif-pairs out-str)))

(defn js-conditional [js-ast out-str]
	  (cond-u
		((js-if js-ast out-str))
		((js-if-else js-ast out-str))
		((js-cond js-ast out-str))))

(defn js-defn [js-ast out-str]
	  (exist (fname parms body new-fn new-ast)
			 (append-o ['defn fname parms] body js-ast)
			 (append-o ['fn parms] body new-fn)
			 (& ['var fname new-fn] new-ast)
			 (js-statement new-ast out-str)))

(defn loop-var [js-ast out-str]
  (exist [v]
  (cond-u
    [(& ['var v] js-ast) (&-expr out-str (format "var %s" (value-of v)))]
    [(&-expr out-str (str (value-of js-ast)))])))

(defn loop-parms [js-ast out-str]
  (exist [v v-str c c-str]
         (cond-u
           [(& ['in v c] js-ast) (loop-var v v-str) (js-expr c c-str)
            (&-expr out-str (format "%s in %s"
                                    (value-of v-str)
                                    (value-of c-str)))])))

(defn js-loop [js-ast out-str]
  (exist [parms parm-str body body-str]
         (append-o ['for parms] body js-ast)
         (loop-parms parms parm-str)
         (statement-block body body-str)
         (&-expr out-str (str "for("
                              (value-of parm-str)
                              ")\n"
                              (value-of body-str)
                              ))))

(defn js-statement [stmt out-str]
	  (cond-u
		[(js-conditional stmt out-str)]
		[(js-defn stmt out-str)]
        [(js-loop stmt out-str)]
		[else (exist (stmt-str)
					 (cond-u
					   ((js-dot-operator stmt stmt-str))
					   ((js-var stmt stmt-str))
					   ((js-new stmt stmt-str))
					   ((js-delete stmt stmt-str))
					   ((js-assignment stmt stmt-str))
					   ((js-funcall stmt stmt-str)))
					 (&-expr out-str (str (value-of stmt-str) ";")))]))

(defn statement-list [lst out-str]
	  (exist [stmt stmt-str rst rst-str]
			 (cons-o stmt rst lst)
			 (js-statement stmt stmt-str)
			 (cond-u
			   [(null-o rst) (&-expr out-str (str (value-of stmt-str)))]
			   [else (statement-list rst rst-str)
					 (&-expr out-str (str (value-of stmt-str) "\n"
										  (value-of rst-str)))])))

(defn statement-block [js-ast out-str]
	  (exist (block-str do-list)
			 (cond-u
			   ((cons-o 'do do-list js-ast) (statement-list do-list block-str))
			   ((statement-list js-ast block-str))
			   ((js-statement js-ast block-str)))
			 (&-expr out-str (str "{\n" (value-of block-str) "\n}\n"))))

(defn js-gen [js-ast]
	  (first (run out-str
				  (js-statement js-ast out-str))))

(let [macro-excepts '(defn fn cond)]
  (defn js-macro [macro-sym]
		(drop-while #(not (= macro-sym %1)) macro-excepts)))

(defn- is-macro? [item]
	  (when (symbol? item)
		(:macro (meta (resolve item)))))

(defn js-macro-expand [expr]
	  (cond
		(not expr) nil
		(not (seq? expr)) expr
		(and (is-macro? (first expr))
			 (not (js-macro (first expr)))) (js-macro-expand (macroexpand expr))
		true (cons (js-macro-expand (first expr)) (js-macro-expand (next expr)))))

(defmacro javascript [& js-items]
  (apply str
		 (interpose "\n" (map (comp js-gen js-macro-expand)
							  js-items))))

