;;;; This file is one of components of CL-MIZAR system, licenced under GPLv3, see COPYING for details

(in-package #:cl-mizar)

(enable-read-macro-tokens)

;;; TODO: words, which we do not yet specify
;;; 1) File-Name
;;; 2) Identifier
;;; 3) Numeral
;;; 4) Symbol

(define-mizar-rule article ()
  `(mizar-article ,environment-declaration
		  ,@text-proper))

;;; TODO: smart way to incorporate whitespaces and line-breaks,
;;; which do not seem to matter here

(define-mizar-rule environment-declaration ()
  "environ"
  (times directive))

(define-mizar-rule directive ()
  (|| vocabulary-directive
      library-directive
      requirement-directive))

(defmacro! comma-semicolon-list (rule &key (semicolon 't))
  `(let ((,g!-first ,rule)
	 (,g!-rest (times (progn "," ,rule))))
     ,@(if semicolon '(";"))
     (cons ,g!-first ,g!-rest)))

(define-mizar-rule vocabulary-directive ()
  (cons (intern (string-upcase "vocabularies"))
	(comma-semicolon-list vocabulary-name)))

(define-mizar-rule vocabulary-name ()
  file-name)

(define-mizar-rule library-directive ()
  (cons (intern (string-upcase
		 (|| "notations"
		     "constructors"
		     "registrations"
		     "definitions"
		     "theorems"
		     "schemes")))
	(comma-semicolon-list article-name)))
    
(define-mizar-rule article-name ()
  file-name)

(define-mizar-rule requirement-directive ()
  (cons (intern (string-upcase "requirements"))
	(comma-semicolon-list requirement)))

(define-mizar-rule requirement ()
  file-name)

(define-mizar-rule text-proper ()
  (times section))

(define-mizar-rule section ()
  "begin"
  (cons 'section
	(times text-item)))

(define-mizar-rule text-item ()
  (|| reservation
      definitional-item
      registration-item
      notation-item
      theorem
      scheme-item
      auxillary-item
      cancelled-theorem))

(define-mizar-rule reservation ()
  (cons (intern (string-upcase "reserve"))
	(comma-semicolon-list reservation-segment)))

;; TODO: human-lisp syntax
(define-mizar-rule reservation-segment ()
  (list reserved-identifiers "for" type-expression))

(define-mizar-rule reserved-identifiers ()
  (comma-semicolon-list identifier :semicolon nil))

(define-mizar-rule definitional-item ()
  (prog1 definitional-block ";"))

(define-mizar-rule registration-item ()
  (prog1 registration-block ";"))

(define-mizar-rule notation-item ()
  (prog1 notation-block ";"))

(define-mizar-rule definitional-block ()
  (progm "definition"
	 (|| definition-item
	     definition
	     redefinition)
	 "end"))

(define-mizar-rule registration-block ()
  (progm "registration"
	 (|| loci-declaration
	     cluster-registration
	     identify-registration
	     properties-registration
	     reduction-registration
	     cancelled-registration)
	 "end"))

(define-mizar-rule notation-block ()
  (progm "notation"
	 (|| loci-declaration
	     notation-declaration)
	 "end"))

(define-mizar-rule definition-item ()
  (|| loci-declaration
      permissive-assumption
      auxiliary-item))

(define-mizar-rule notation-declaration ()
  (|| attribute-synonym
      attribute-antonym
      functor-synonym
      mode-synonym
      predicate-synonym
      predicate-antonym))

(define-mizar-rule loci-declaration ()
  (progm "let"
	 (list qualified-variables
	       (? (progn "such" conditions)))
	 ";"))

(define-mizar-rule permissive-assumption ()
  assumption)

(define-mizar-rule definition ()
  (|| structure-definition
      mode-definition
      functor-definition
      predicate-definition
      attribute-definition
      cancelled-definition))

(define-mizar-rule redefinition ()
  "redefine"
  (|| mode-definition
      functor-definition
      predicate-definition
      attribute-definition))

(define-mizar-rule struct ()
  "struct"
  (let ((ancestors (? (progm "(" ancestors ")")))
	(structure-symbol structure-symbol)
	(loci (? (progn "over" loci)))
	(fields (progm "(#" fields "#)")))
    ";"
    `(struct ,structure-symbol ,fields
	     ,@(if loci `(:loci ,loci))
	     ,@(if ancestors `(:ancestors ,ancestors)))))

(define-mizar-rule ancestors ()
  (comma-semicolon-list structure-type-expression
			:semicolon nil))

(define-mizar-rule structure-symbol ()
  symbol)

(define-mizar-rule loci ()
  (comma-semicolon-list locus :semicolon nil))

(define-mizar-rule fields ()
  (comma-semicolon-list field-segment :semicolon nil))

(define-mizar-rule locus ()
  variable-identifier)

(define-mizar-rule variable-identifier ()
  identifier)

(define-mizar-rule field-segment ()
  (let ((selectors (comma-semicolon-list selector-symbol
					 :semicolon nil))
	(spec specification))
    (list selectors spec)))

(define-mizar-rule selector-symbol ()
  symbol)

(define-mizar-rule specification ()
  "->"
  type-expression)

(define-mizar-rule mode-definition ()
  "mode"
  (let ((mode-pattern mode-pattern)
	(body (|| (list :means
			(? specification)
			(? (progn "means" definiens))
			(progn ";"
			       correctness-conditions))
		  (list :is
			(prog1 type-expression ";")))))
    ;; TODO: human lisp syntax
    `(mode ,mode-pattern ,body)))

(define-mizar-rule mode-pattern ()
  (cons mode-symbol
	(? (progn "of" loci))))

(define-mizar-rule mode-symbol ()
  (|| symbol
      (progn "set" :set)))


(define-mizar-rule mode-synonym ()
  "synonym"
  (let ((new mode-pattern)
	(old (progm "for" mode-pattern ";")))
    `(synonym ,new ,old)))

(define-mizar-rule definiens ()
  (|| simple-definiens
      conditional-definiens))

(define-mizar-rule simple-definiens ()
  (let ((label (? (progm ":" label-identifier ":")))
	(body (|| sentence
		  term-expression)))
    (list label body)))

(define-mizar-rule label-identifier ()
  identifier)

(define-mizar-rule conditional-definiens ()
  (let ((label (? (progm ":" label-identifier ":")))
	(partials partial-definiens-list)
	(body (? (progn "otherwise"
			(|| sentence
			    term-expression)))))
    (list label partials body)))

(define-mizar-rule partial-definiens-list ()
  (comma-semicolon-list partial-definiens
			:semicolon nil))

(define-mizar-rule partial-definiens ()
  (list (|| sentence term-expression)
	(progn "if" sentence)))

(define-mizar-rule functor-definition ()
  "func"
  (let ((functor-pattern functor-pattern)
	(spec (? specification))
	(defins (? (progn (|| "means" "equals")
			  definiens)))
	(conds correctness-conditions)
	(props (times functor-property)))
    `(func ,functor-pattern
	   ,spec
	   ,defins
	   ,conds
	   ,props)))

(define-mizar-rule left-functor-bracket ()
  (|| symbol "{" "["))

(define-mizar-rule right-functor-bracket ()
  (|| symbol "}" "]"))

(define-mizar-rule functor-pattern ()
  (|| (let ((pre-loci (? functor-loci))
	    (functor-symbol functor-symbol)
	    (post-loci (? functor-loci)))
	(list functor-symbol pre-loci post-loci))
      (list left-functor-bracket
	    loci
	    right-functor-bracket)))

(define-mizar-rule functor-property ()
  (list (intern (string-upcase (|| "commutativity"
				   "idempotence"
				   "involutiveness"
				   "projectivity"))
		(find-package "KEYWORD"))
	(prog1 justification ";")))

(define-mizar-rule functor-synonym ()
  "synonym"
  (let ((new functor-pattern)
	(old (progm "for" functor-pattern ";")))
    `(synonym ,new ,old)))

(define-mizar-rule functor-loci ()
  (|| locus
      (progm "(" loci ")")))

(define-mizar-rule functor-symbol ()
  symbol)

(define-mizar-rule predicate-definition ()
  "pred"
  (let ((pattern predicate-pattern)
	(defins (? (progn "means" definiens)))
	(conds (progn ";" correctness-conditions))
	(props (times predicate-property)))
    `(pred ,pattern ,defins ,conds ,props)))

(define-mizar-rule predicate-pattern ()
  (let ((pre-loci (? loci))
	(predicate-symbol predicate-symbol)
	(post-loci (? loci)))
    `(,predicate-symbol ,pre-loci ,post-loci)))

(define-mizar-rule predicate-property ()
  (list (intern (string-upcase (|| "symmetry"
				   "asymmetry"
				   "connectedness"
				   "reflexivity"
				   "irreflexivity"))
		(find-package "KEYWORD"))
	(prog1 justification ";")))

(define-mizar-rule predicate-synonym ()
  "synonym"
  (let ((new predicate-pattern)
	(old (progm "for" predicate-pattern ";")))
    `(synonym ,new ,old)))

(define-mizar-rule predicate-antonym ()
  "antonym"
  (let ((new predicate-pattern)
	(old (progm "for" predicate-pattern ";")))
    `(antonym ,new ,old)))

(define-mizar-rule predicate-symbol ()
  (|| symbol "="))

(define-mizar-rule attribute-definition ()
  "attr"
  (let ((pattern attribute-pattern)
	(defins (progm "means" definiens ";"))
	(conds correctness-conditions))
    `(attr ,pattern ,defins ,conds)))

(define-mizar-rule attribute-pattern ()
  (let ((locus (prog1 locus "is"))
	(loci (? attribute-loci))
	(sym attribute-symbol))
    `(,sym ,locus ,loci)))

(define-mizar-rule attribute-synonym ()
  "synonym"
  (let ((new attribute-pattern)
	(old (progm "for" attribute-pattern ";")))
    `(synonym ,new ,old)))
  
(define-mizar-rule attribute-antonym ()
  "antonym"
  (let ((new attribute-pattern)
	(old (progm "for" attribute-pattern ";")))
    `(antonym ,new ,old)))

(define-mizar-rule attribute-symbol ()
  symbol)

(define-mizar-rule attribute-loci ()
  (|| loci ; TODO: should there be a 'locus' here?
      (progm "(" loci ")")))

(define-mizar-rule cancelled-definition ()
  `(,(progn "cancelled" 'cancelled)
     ,(progn numeral ";")))

(define-mizar-rule cancelled-registration ()
  `(,(progn "cancelled" 'cancelled)
     ,(progn numeral ";")))

(define-mizar-rule cluster-registration ()
  (|| existential-registration
      conditional-registration
      functorial-registration))

(define-mizar-rule existential-registration ()
  `(,(progn "cluster" 'cluster)
     ,adjective-cluster
     ,(progm "for" type-expression ";")
     ,correctness-conditions))

(define-mizar-rule adjective-cluster ()
  (times adjective))

(define-mizar-rule adjective ()
  (list (? (progn "non" :non))
	(? adjective-arguments)
	adjective-symbol))

(define-mizar-rule conditional-registration ()
  `(,(progn "cluster" 'cluster)
     ,adjective-cluster
     ,adjective-cluster
     ,(progm "for" type-expression ";")
     ,correctness-conditions))

(define-mizar-rule functorial-registration ()
  `(,(progn "cluster" 'cluster)
     ,term-expression
     ,adjective-cluster
     ,(? (progn "for" type-expression))
     ,(progn ";" correctness-conditions)))

(define-mizar-rule identify-registration ()
  `(,(progn "identify" 'identify)
     ,functor-pattern
     ,(progn "with" functor-pattern)
     ,(? (cons "when"
	       (comma-semicolon-list (list locus
					   "=" locus)
				     :semicolon nil)))
     ,(progn ";" correctness-conditions)))

(define-mizar-rule properties-registration ()
  `(,(progn "sethood" "of" 'sethood-of)
     ,type-expression
     ,(prog1 justification ";")))

(define-mizar-rule reduction-registration ()
  `(,(progn "reduce" 'reduce)
     ,term-expression
     ,(progm "to" term-expression ";")
     ,correctness-conditions))

(define-mizar-rule correctness-conditions ()
  (append (times correctness-condition)
	  (? (list (intern (string-upcase "correctness")
			   (find-package "KEYWORD"))
		   (prog1 justification ";")))))

(define-mizar-rule correctness-condition ()
  (list (intern (string-upcase (|| "existence"
				   "uniqness"
				   "coherence"
				   "compatibility"
				   "consistency"
				   "reducibility"))
		(find-package "KEYWORD"))
	(prog1 justification ";")))
			     
(define-mizar-rule theorem ()
  `(,(progn "theorem" 'theorem)
     ,compact-statement))

(define-mizar-rule scheme-item ()
  (prog1 scheme-block ";"))

(define-mizar-rule scheme-block ()
  `(,(progn "scheme" 'scheme)
     ,scheme-identifier
     ,(progm "{" scheme-parameters "}")
     ,(progn ":" scheme-conclusion)
     ,(? (progn "provided"
		(let ((first scheme-premise)
		      (rest (times (progn "and"
					  scheme-premise))))
		  (cons first rest))))
     ,(progm ";" reasoning "end")))

(define-mizar-rule scheme-identifier ()
  identifier)

(define-mizar-rule scheme-parameters ()
  (comma-semicolon-list scheme-segment :semicolon nil))

(define-mizar-rule scheme-conclusion ()
  sentence)

(define-mizar-rule scheme-premise ()
  proposition)

(define-mizar-rule scheme-segment ()
  (|| predicate-segment
      functor-segment))

(define-mizar-rule predicate-segment ()
  `(predicates
    ,(comma-semicolon-list predicate-identifier)
    ,(progm "[" type-expression-list "]")))

(define-mizar-rule predicate-identifier ()
  identifier)

(define-mizar-rule functor-segment ()
  `(functors
    ,(comma-semicolon-list functor-identifier)
    ,(progm "(" type-expression-list ")")
    ,specification))

(define-mizar-rule functor-identifier ()
  identifier)

(define-mizar-rule auxiliary-item ()
  (|| statement
      private-definition))
    
(define-mizar-rule cancelled-theorem ()
  (prog1 `(,(progn "cancelled" 'cancelled)
	    ,(? numeral))
    ";"))

(define-mizar-rule private-definition ()
  (|| constant-definition
      private-functor-definition
      private-predicate-definition))

(define-mizar-rule constant-definition ()
  `(,(progn "set" 'set)
     ,@(prog1 equating-list ";")))

(define-mizar-rule equating-list ()
  (comma-semicolon-list equating :semicolon nil))

(define-mizar-rule equating ()
  `(,variable-identifier
    ,(progn "=" term-expression)))

(define-mizar-rule private-functor-definition ()
  `(,(progn "deffunc" 'deffunc)
     ,@private-functor-pattern
     ,(progn "=" term-expression)))

(define-mizar-rule private-predicate-definition ()
  `(,(progn "defpred" 'defpred)
     ,@private-predicate-pattern
     ,(progn "means" sentence)))

(define-mizar-rule private-functor-pattern ()
  (list functor-identifier
	(progm "(" (? type-expression-list) ")")))

(define-mizar-rule private-predicate-pattern ()
  (list predicate-identifier
	(progm "[" (? type-expression-list) "]")))

(define-mizar-rule reasoning ()
  (let ((reasonings (times reasoning-item))
	(addendum
	 (? (progn (? "then") "per" "cases"
		   (list simple-justification
			 (progn ";"
				(|| case-list
				    suppose-list)))))))
    ;; TODO: human lispy syntax
    (list reasonings addendum)))

(define-mizar-rule case-list ()
  (postimes case))

(define-mizar-rule case ()
  (list (progn "case" 'case)
	(|| proposition
	    conditions)
	(progn ";"
	       (prog1 reasoning
		 "end" ";"))))

(define-mizar-rule suppose-list ()
  (postimes suppose))

(define-mizar-rule suppose ()
  (list (progn "suppose" 'suppose)
	(|| proposition
	    conditions)
	(progn ";"
	       (prog1 reasoning
		 "end" ";"))))
  
(define-mizar-rule reasoning-item ()
  (|| auxiliary-item
      skeleton-item))

(define-mizar-rule skeleton-item ()
  (|| generalization
      assumption
      conclusion
      exemplification))

(define-mizar-rule generalization ()
  (prog1 `(,(progn "let" 'let)
	    ,qualified-variables
	    ,(? (progn "such" conditions)))
    ";"))

(define-mizar-rule assumption ()
  (|| single-assumption
      collective-assumption
      existential-assumption))

(define-mizar-rule single-assumption ()
  (prog1 `(,(progn "assume" 'assume)
	    ,proposition)
    ";"))

(define-mizar-rule collective-assumption ()
  (prog1 `(,(progn "assume" 'assume)
	    ,@conditions)
    ";"))

(define-mizar-rule existential-assumption ()
  (prog1 `(,(progn "given" 'given)
	    ,qualified-variables
	    ,(? (progn "such" conditions)))
    ";"))

(define-mizar-rule conclusion ()
  (|| (progn (|| "thus" "hence") compact-statement)
      diffuse-conclusion))

(define-mizar-rule diffuse-conclusion ()
  (|| (progn "thus" c!-1-diffuse-statement)
      (progn "hereby" c!-1-reasoning "end" ";"))
  c!-1)


(define-mizar-rule exemplification ()
  `(,(progn "take" 'take)
     ,@(comma-semicolon-list example)))

(define-mizar-rule example ()
  (|| term-expression
      (progn c!-1-variable-identifier
	     "="
	     c!-2-term-expression
	     (list c!-1 c!-2))))

(define-mizar-rule statement ()
  (? "then")
  (|| linkable-statement
      diffuse-statement))

(define-mizar-rule linkable-statement ()
  (|| compact-statement
      choice-statement
      type-changing-statement
      iterative-equality))

(define-mizar-rule compact-statement ()
  (prog1 (list proposition justification)
    ";"))

(define-mizar-rule choice-statement ()
  "consider"
  c!-1-qualified-variables
  "such"
  c!-2-conditions
  c!-3-simple-justification
  ";"
  `(consider ,c!-1 ,c!-2 c!-3))

(define-mizar-rule type-changing-statement ()
  "reconsider"
  c!-1-type-change-list
  "as"
  c!-2-type-expression
  c!-3-simple-justification
  ";"
  `(reconsider ,c!-1 ,c!-2 ,c!-3))

(define-mizar-rule type-change-list ()
  
