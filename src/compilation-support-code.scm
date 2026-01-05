(define *class-global-info*
  '((<api-entry> 1 :doc :owner :file-name)
    (<api-named-entry>
      2
      :doc
      :owner
      :file-name
      :name)
    (<api-group>
      3
      :doc
      :owner
      :file-name
      :name
      :entries)
    (<api-entry-with-header>
      4
      :doc
      :owner
      :file-name
      :name
      :header
      :examples)
    (<api-function-entry>
      5
      :doc
      :owner
      :file-name
      :name
      :header
      :examples)
    (<api-generic-entry>
      6
      :doc
      :owner
      :file-name
      :name
      :header
      :examples
      :methods)
    (<api-class-entry>
      7
      :doc
      :owner
      :file-name
      :name
      :header
      :examples)
    (<sal-cmd-line-option>
      8
      :category
      :short-opt-name
      :long-opt-name
      :description
      :proc)
    (<sal-ast> 9 :place :context :hash :internal-idx)
    (<sal-ast-leaf>
      10
      :place
      :context
      :hash
      :internal-idx)
    (<sal-identifier>
      11
      :place
      :context
      :hash
      :internal-idx
      :name)
    (<sal-new-binds-ast>
      12
      :place
      :context
      :hash
      :internal-idx)
    (<sal-local-binds-ast>
      13
      :place
      :context
      :hash
      :internal-idx)
    (<sal-decl>
      14
      :place
      :context
      :hash
      :internal-idx
      :id)
    (<sal-typed-decl>
      15
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sal-const-decl>
      16
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value)
    (<sal-auxiliary-decl>
      17
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value)
    (<sal-context>
      18
      :place
      :context
      :hash
      :internal-idx
      :id
      :params
      :declarations
      :constant-declarations
      :type-declarations
      :module-declarations
      :assertion-declarations
      :context-name-declarations
      :sal-env
      :importers
      :internal-actuals
      :file-name)
    (<sal-type-param-decl>
      19
      :place
      :context
      :hash
      :internal-idx
      :id)
    (<sal-var-decl>
      20
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sal-var-param-decl>
      21
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sal-idx-var-decl>
      22
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sal-state-var-decl>
      23
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :curr-node
      :next-node)
    (<sal-input-state-var-decl>
      24
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :curr-node
      :next-node)
    (<sal-choice-input-state-var-decl>
      25
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :curr-node
      :next-node)
    (<sal-output-state-var-decl>
      26
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :curr-node
      :next-node)
    (<sal-global-state-var-decl>
      27
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :curr-node
      :next-node)
    (<sal-local-state-var-decl>
      28
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :curr-node
      :next-node)
    (<sal-let-decl>
      29
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value)
    (<sal-top-decl>
      30
      :place
      :context
      :hash
      :internal-idx
      :id)
    (<sal-recursive-decl>
      31
      :place
      :context
      :hash
      :internal-idx
      :id)
    (<sal-type-decl>
      32
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sal-module-decl>
      33
      :place
      :context
      :hash
      :internal-idx
      :id
      :parametric-module)
    (<sal-parametric-module>
      34
      :place
      :context
      :hash
      :internal-idx
      :local-decls
      :module)
    (<sal-constant-decl>
      35
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value
      :memoize?)
    (<sal-implicit-decl>
      36
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value
      :memoize?)
    (<sal-constructor-decl>
      37
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value
      :memoize?
      :accessors
      :recognizer-decl
      :data-type-decl)
    (<sal-recognizer-decl>
      38
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value
      :memoize?
      :constructor-decl)
    (<sal-accessor-decl>
      39
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value
      :memoize?
      :constructor-decl)
    (<sal-scalar-element-decl>
      40
      :place
      :context
      :hash
      :internal-idx
      :id
      :type
      :value
      :memoize?
      :scalar-type-decl)
    (<sal-context-name-decl>
      41
      :place
      :context
      :hash
      :internal-idx
      :id
      :context-ref
      :actuals)
    (<sal-assertion-decl>
      42
      :place
      :context
      :hash
      :internal-idx
      :id
      :kind
      :assertion-expr)
    (<sal-name-ref>
      43
      :place
      :context
      :hash
      :internal-idx)
    (<sal-qualified-name-ref>
      44
      :place
      :context
      :hash
      :internal-idx)
    (<sal-expr>
      45
      :place
      :context
      :hash
      :internal-idx
      :type)
    (<sal-simple-expr>
      46
      :place
      :context
      :hash
      :internal-idx
      :type)
    (<sal-application>
      47
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-infix-application>
      48
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-in>
      49
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-binary-application>
      50
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-binary-infix-application>
      51
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-unary-application>
      52
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-builtin-application>
      53
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-constructor-application>
      54
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-recognizer-application>
      55
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-accessor-application>
      56
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-eq>
      57
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-assignment>
      58
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-diseq>
      59
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-propositional-application>
      60
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-binary-propositional-application>
      61
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-unary-propositional-application>
      62
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-iff>
      63
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-xor>
      64
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-and>
      65
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-or>
      66
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-choice>
      67
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-not>
      68
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-implies>
      69
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-temporal-application>
      70
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ltl-application>
      71
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-unary-ltl-application>
      72
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-binary-ltl-application>
      73
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ltl-x>
      74
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ltl-g>
      75
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ltl-f>
      76
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ltl-u>
      77
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ltl-r>
      78
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ltl-w>
      79
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ltl-m>
      80
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-application>
      81
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-unary-ctl-application>
      82
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-binary-ctl-application>
      83
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-ax>
      84
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-ex>
      85
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-ag>
      86
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-eg>
      87
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-af>
      88
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-ef>
      89
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-au>
      90
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-eu>
      91
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-ar>
      92
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ctl-er>
      93
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-accepting>
      94
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-weak-accepting>
      95
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-arith-application>
      96
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-binary-arith-application>
      97
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-arith-op>
      98
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-infix-arith-op>
      99
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-binary-arith-op>
      100
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-binary-infix-arith-op>
      101
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-add>
      102
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-sub>
      103
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-mul>
      104
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-div>
      105
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-idiv>
      106
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-mod>
      107
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-max>
      108
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-min>
      109
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-arith-relation>
      110
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-inequality>
      111
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-lt>
      112
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-gt>
      113
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ge>
      114
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-le>
      115
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-real-pred>
      116
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-int-pred>
      117
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-definition-expression>
      118
      :place
      :context
      :hash
      :internal-idx
      :type
      :lhs-list
      :expr)
    (<sal-numeral>
      119
      :place
      :context
      :hash
      :internal-idx
      :type
      :num)
    (<sal-name-expr>
      120
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl)
    (<sal-var-param-name-expr>
      121
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl)
    (<sal-qualified-name-expr>
      122
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sal-scalar>
      123
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sal-true>
      124
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sal-false>
      125
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sal-constructor>
      126
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sal-accessor>
      127
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sal-recognizer>
      128
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sal-local-binds-expr>
      129
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-lambda>
      130
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-set-pred-expr>
      131
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-set-list-expr>
      132
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-quantified-expr>
      133
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-for-all-expr>
      134
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-exists-expr>
      135
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-multi-choice-expr>
      136
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-let-expr>
      137
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-collection-literal>
      138
      :place
      :context
      :hash
      :internal-idx
      :type)
    (<sal-array-literal>
      139
      :place
      :context
      :hash
      :internal-idx
      :type
      :local-decls
      :expr)
    (<sal-tuple-literal>
      140
      :place
      :context
      :hash
      :internal-idx
      :type
      :exprs)
    (<sal-arg-tuple-literal>
      141
      :place
      :context
      :hash
      :internal-idx
      :type
      :exprs)
    (<sal-record-literal>
      142
      :place
      :context
      :hash
      :internal-idx
      :type
      :entries)
    (<sal-state-record-literal>
      143
      :place
      :context
      :hash
      :internal-idx
      :type
      :entries)
    (<sal-record-entry>
      144
      :place
      :context
      :hash
      :internal-idx
      :id
      :expr)
    (<sal-selection>
      145
      :place
      :context
      :hash
      :internal-idx
      :type)
    (<sal-array-selection>
      146
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-simple-selection>
      147
      :place
      :context
      :hash
      :internal-idx
      :type
      :target
      :idx)
    (<sal-tuple-selection>
      148
      :place
      :context
      :hash
      :internal-idx
      :type
      :target
      :idx)
    (<sal-record-selection>
      149
      :place
      :context
      :hash
      :internal-idx
      :type
      :target
      :idx)
    (<sal-update-expr>
      150
      :place
      :context
      :hash
      :internal-idx
      :type
      :target
      :idx
      :new-value)
    (<sal-function-update>
      151
      :place
      :context
      :hash
      :internal-idx
      :type
      :target
      :idx
      :new-value)
    (<sal-array-update>
      152
      :place
      :context
      :hash
      :internal-idx
      :type
      :target
      :idx
      :new-value)
    (<sal-record-update>
      153
      :place
      :context
      :hash
      :internal-idx
      :type
      :target
      :idx
      :new-value)
    (<sal-tuple-update>
      154
      :place
      :context
      :hash
      :internal-idx
      :type
      :target
      :idx
      :new-value)
    (<sal-conditional>
      155
      :place
      :context
      :hash
      :internal-idx
      :type
      :cond-expr
      :then-expr
      :else-expr)
    (<sal-next-operator>
      156
      :place
      :context
      :hash
      :internal-idx
      :type
      :name-expr)
    (<sal-string-expr>
      157
      :place
      :context
      :hash
      :internal-idx
      :type
      :string)
    (<sal-mod-init>
      158
      :place
      :context
      :hash
      :internal-idx
      :type
      :module)
    (<sal-mod-trans>
      159
      :place
      :context
      :hash
      :internal-idx
      :type
      :module)
    (<sal-ring-application>
      160
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ring-pre>
      161
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-ring-succ>
      162
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-debug-application>
      163
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-debug-print>
      164
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-debug-expr>
      165
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-pre-operator>
      166
      :place
      :context
      :hash
      :internal-idx
      :type
      :expr)
    (<sal-type>
      167
      :place
      :context
      :hash
      :internal-idx)
    (<sal-type-name>
      168
      :place
      :context
      :hash
      :internal-idx
      :decl)
    (<sal-type-param-name>
      169
      :place
      :context
      :hash
      :internal-idx
      :decl)
    (<sal-qualified-type-name>
      170
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-any-type>
      171
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-bool-type>
      172
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-number-type>
      173
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-real-type>
      174
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-int-type>
      175
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-nat-type>
      176
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-string-type>
      177
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-function-type>
      178
      :place
      :context
      :hash
      :internal-idx
      :domain
      :range)
    (<sal-array-type>
      179
      :place
      :context
      :hash
      :internal-idx
      :domain
      :range)
    (<sal-tuple-type>
      180
      :place
      :context
      :hash
      :internal-idx
      :types)
    (<sal-domain-tuple-type>
      181
      :place
      :context
      :hash
      :internal-idx
      :types)
    (<sal-record-type>
      182
      :place
      :context
      :hash
      :internal-idx
      :fields)
    (<sal-field>
      183
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sal-state-type>
      184
      :place
      :context
      :hash
      :internal-idx
      :module)
    (<sal-subtype>
      185
      :place
      :context
      :hash
      :internal-idx
      :expr)
    (<sal-bounded-subtype>
      186
      :place
      :context
      :hash
      :internal-idx
      :expr
      :lower
      :upper)
    (<sal-subrange>
      187
      :place
      :context
      :hash
      :internal-idx
      :expr
      :lower
      :upper)
    (<sal-symmetric-type>
      188
      :place
      :context
      :hash
      :internal-idx
      :expr
      :lower
      :upper)
    (<sal-scalar-set-type>
      189
      :place
      :context
      :hash
      :internal-idx
      :expr
      :lower
      :upper)
    (<sal-ring-set-type>
      190
      :place
      :context
      :hash
      :internal-idx
      :expr
      :lower
      :upper)
    (<sal-type-def>
      191
      :place
      :context
      :hash
      :internal-idx)
    (<sal-scalar-type>
      192
      :place
      :context
      :hash
      :internal-idx
      :scalar-elements)
    (<sal-data-type>
      193
      :place
      :context
      :hash
      :internal-idx
      :constructors)
    (<sal-definition>
      194
      :place
      :context
      :hash
      :internal-idx)
    (<sal-simple-definition>
      195
      :place
      :context
      :hash
      :internal-idx
      :lhs
      :rhs)
    (<sal-simple-selection-definition>
      196
      :place
      :context
      :hash
      :internal-idx
      :lhs
      :rhs)
    (<sal-for-all-definition>
      197
      :place
      :context
      :hash
      :internal-idx
      :local-decls
      :definitions)
    (<sal-command-section>
      198
      :place
      :context
      :hash
      :internal-idx
      :commands
      :else-command)
    (<sal-command>
      199
      :place
      :context
      :hash
      :internal-idx)
    (<sal-guarded-command>
      200
      :place
      :context
      :hash
      :internal-idx
      :guard
      :assignments)
    (<sal-labeled-command>
      201
      :place
      :context
      :hash
      :internal-idx
      :label
      :command)
    (<sal-multi-command>
      202
      :place
      :context
      :hash
      :internal-idx
      :local-decls
      :command)
    (<sal-else-command>
      203
      :place
      :context
      :hash
      :internal-idx
      :assignments)
    (<sal-module>
      204
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table)
    (<sal-non-base-module>
      205
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table)
    (<sal-module-composition>
      206
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :module1
      :module2)
    (<sal-asynch-composition>
      207
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :module1
      :module2)
    (<sal-synch-composition>
      208
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :module1
      :module2)
    (<sal-observer>
      209
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :module1
      :module2)
    (<sal-multi-composition>
      210
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :local-decls
      :module)
    (<sal-multi-asynch-composition>
      211
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :local-decls
      :module)
    (<sal-multi-synch-composition>
      212
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :local-decls
      :module)
    (<sal-renaming>
      213
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :renames
      :module)
    (<sal-rename>
      214
      :place
      :context
      :hash
      :internal-idx
      :from-name
      :to-expr)
    (<sal-org-module>
      215
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :identifiers
      :module)
    (<sal-hiding>
      216
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :identifiers
      :module)
    (<sal-new-output>
      217
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :identifiers
      :module)
    (<sal-with-module>
      218
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :new-state-vars
      :module)
    (<sal-module-name>
      219
      :place
      :context
      :hash
      :internal-idx
      :decl)
    (<sal-qualified-module-name>
      220
      :place
      :context
      :hash
      :internal-idx
      :decl
      :context-ref
      :actuals)
    (<sal-module-instance>
      221
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :module-name
      :actuals)
    (<sal-base-module>
      222
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :definitions
      :initialization-definitions
      :initialization-command-section
      :transition-definitions
      :transition-command-section)
    (<sal-flat-module>
      223
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :definition
      :initialization
      :transition
      :skip
      :transition-trace-info
      :choice-vars
      :component-info
      :valid-input-expr
      :valid-state-expr
      :valid-constant-expr)
    (<sal-component-info>
      224
      :place
      :context
      :hash
      :internal-idx
      :data)
    (<sal-base-component-info>
      225
      :place
      :context
      :hash
      :internal-idx
      :data
      :input-data
      :output-data
      :owned-data)
    (<sal-multi-component-info>
      226
      :place
      :context
      :hash
      :internal-idx
      :data
      :local-decls
      :component)
    (<sal-composite-component-info>
      227
      :place
      :context
      :hash
      :internal-idx
      :data
      :components)
    (<sal-derived-flat-module>
      228
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :definition
      :initialization
      :transition
      :skip
      :transition-trace-info
      :choice-vars
      :component-info
      :valid-input-expr
      :valid-state-expr
      :valid-constant-expr
      :original-module
      :var-trace-info
      :const-trace-info)
    (<sal-sliced-flat-module>
      229
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :definition
      :initialization
      :transition
      :skip
      :transition-trace-info
      :choice-vars
      :component-info
      :valid-input-expr
      :valid-state-expr
      :valid-constant-expr
      :original-module
      :var-trace-info
      :const-trace-info)
    (<sal-simple-data-flat-module>
      230
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :definition
      :initialization
      :transition
      :skip
      :transition-trace-info
      :choice-vars
      :component-info
      :valid-input-expr
      :valid-state-expr
      :valid-constant-expr
      :original-module
      :var-trace-info
      :const-trace-info)
    (<sal-sliced-simple-data-flat-module>
      231
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :definition
      :initialization
      :transition
      :skip
      :transition-trace-info
      :choice-vars
      :component-info
      :valid-input-expr
      :valid-state-expr
      :valid-constant-expr
      :original-module
      :var-trace-info
      :const-trace-info)
    (<sal-boolean-flat-module>
      232
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :definition
      :initialization
      :transition
      :skip
      :transition-trace-info
      :choice-vars
      :component-info
      :valid-input-expr
      :valid-state-expr
      :valid-constant-expr
      :original-module
      :var-trace-info
      :const-trace-info)
    (<sal-sliced-boolean-flat-module>
      233
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :definition
      :initialization
      :transition
      :skip
      :transition-trace-info
      :choice-vars
      :component-info
      :valid-input-expr
      :valid-state-expr
      :valid-constant-expr
      :original-module
      :var-trace-info
      :const-trace-info)
    (<sal-assertion-expr>
      234
      :place
      :context
      :hash
      :internal-idx
      :type)
    (<sal-module-models>
      235
      :place
      :context
      :hash
      :internal-idx
      :type
      :module
      :expr)
    (<sal-module-implements>
      236
      :place
      :context
      :hash
      :internal-idx
      :type
      :module1
      :module2)
    (<sal-assertion-proposition>
      237
      :place
      :context
      :hash
      :internal-idx
      :type
      :op
      :assertion-exprs)
    (<sal-qualified-assertion-name>
      238
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sal-trace-info>
      239
      :place
      :context
      :hash
      :internal-idx)
    (<sal-transition-trace-info>
      240
      :place
      :context
      :hash
      :internal-idx)
    (<sal-else-trace-info>
      241
      :place
      :context
      :hash
      :internal-idx)
    (<sal-nested-trace-info>
      242
      :place
      :context
      :hash
      :internal-idx
      :info)
    (<sal-module-instance-trace-info>
      243
      :place
      :context
      :hash
      :internal-idx
      :info)
    (<sal-labeled-trace-info>
      244
      :place
      :context
      :hash
      :internal-idx
      :info
      :label)
    (<sal-multi-trace-info>
      245
      :place
      :context
      :hash
      :internal-idx
      :info)
    (<sal-multi-choice-trace-info>
      246
      :place
      :context
      :hash
      :internal-idx
      :info
      :choice-var-names
      :original-var-names)
    (<sal-multi-command-choice-trace-info>
      247
      :place
      :context
      :hash
      :internal-idx
      :info
      :choice-var-names
      :original-var-names)
    (<sal-multi-sequence-trace-info>
      248
      :place
      :context
      :hash
      :internal-idx
      :info
      :idx-var-name)
    (<sal-nested-list-trace-info>
      249
      :place
      :context
      :hash
      :internal-idx
      :info-list)
    (<sal-choice-trace-info>
      250
      :place
      :context
      :hash
      :internal-idx
      :info-list
      :choice-var-name)
    (<sal-sequence-trace-info>
      251
      :place
      :context
      :hash
      :internal-idx
      :info-list)
    (<sal-esm-component>
      252
      :place
      :context
      :hash
      :internal-idx)
    (<sal-esm-statement>
      253
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives)
    (<sal-esm-composition-statement>
      254
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :statements)
    (<sal-esm-choice>
      255
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :statements)
    (<sal-esm-seq>
      256
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :statements)
    (<sal-esm-monitor-seq>
      257
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :statements)
    (<sal-esm-case>
      258
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :expr
      :case-entries)
    (<sal-esm-case-entry>
      259
      :place
      :context
      :hash
      :internal-idx
      :value
      :statement)
    (<sal-esm-when-undefined>
      260
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :lhs
      :statement)
    (<sal-esm-new-binds-statement>
      261
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :local-decls
      :statement)
    (<sal-esm-multi-seq>
      262
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :local-decls
      :statement)
    (<sal-esm-multi-choice>
      263
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :local-decls
      :statement)
    (<sal-esm-leaf>
      264
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :dependencies
      :no-delay?)
    (<sal-esm-guard>
      265
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :dependencies
      :no-delay?
      :expr)
    (<sal-esm-assignment>
      266
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :dependencies
      :no-delay?
      :lhs
      :rhs)
    (<sal-esm-choice-assignment>
      267
      :place
      :context
      :hash
      :internal-idx
      :num-alternatives
      :dependencies
      :no-delay?
      :lhs
      :rhs)
    (<sal-esm-module>
      268
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :initialization
      :transition
      :definition
      :transition-trace-info
      :choice-vars)
    (<sal-data-flat-esm-module>
      269
      :place
      :context
      :hash
      :internal-idx
      :state-vars
      :state-vars-table
      :initialization
      :transition
      :definition
      :transition-trace-info
      :choice-vars
      :original-module
      :var-trace-info)
    (<sal-parser-manager>
      270
      :curr-line
      :curr-column)
    (<sal-instantiation-info>
      271
      :context
      :actuals
      :proc)
    (<pp-info>
      272
      :max-width
      :max-ribbon
      :max-num-lines
      :max-indent
      :single-line?
      :bounded?)
    (<doc> 273)
    (<doc-nil> 274)
    (<doc-concat> 275 :docs)
    (<doc-nest> 276 :i :doc)
    (<doc-text> 277 :str)
    (<doc-line> 278)
    (<doc-line++> 279 :str)
    (<doc-union> 280 :docs)
    (<idoc> 281)
    (<idoc-nil> 282)
    (<idoc-text> 283 :str :idoc)
    (<idoc-line> 284 :i :idoc)
    (<pp-info++>
      285
      :max-width
      :max-ribbon
      :max-num-lines
      :max-indent
      :single-line?
      :bounded?
      :max-depth
      :default-indent
      :simplify-qualified-names?)
    (<pp-value-info++>
      286
      :max-width
      :max-ribbon
      :max-num-lines
      :max-indent
      :single-line?
      :bounded?
      :max-depth
      :default-indent
      :simplify-qualified-names?)
    (<sal-var-rename-info> 287 :defined-vars)
    (<sal-var-rename-info++>
      288
      :defined-vars
      :preserve?)
    (<sal-type-checker-info>
      289
      :already-defined-positions
      :base-module-region
      :inside-property?
      :logic)
    (<sal-string-reader>
      290
      :scratch-context
      :sal-env)
    (<lsal-string-reader>
      291
      :scratch-context
      :sal-env)
    (<sal-env>
      292
      :contexts
      :sal-context-path
      :extension-parser-table
      :type-check?
      :string-reader
      :prelude
      :ast-attributes
      :class-to-decl
      :auxiliary-context)
    (<bound> 293 :value)
    (<upper-bound> 294 :value)
    (<lower-bound> 295 :value)
    (<le-bound> 296 :value)
    (<lt-bound> 297 :value)
    (<ge-bound> 298 :value)
    (<gt-bound> 299 :value)
    (<sal-assignment-info> 300)
    (<sal-undef-assignment-info> 301)
    (<sal-partially-def-assignment-info> 302)
    (<sal-def-assignment-info> 303)
    (<sal-def-assignment-info-with-rhs> 304 :rhs)
    (<sal-collection-assignment-info> 305 :children)
    (<sal-tuple-assignment-info> 306 :children)
    (<sal-record-assignment-info> 307 :children)
    (<sal-array-assignment-info> 308 :children)
    (<sal-entry-info> 309 :idx :info)
    (<polarity> 310)
    (<pos> 311)
    (<neg> 312)
    (<pos-neg> 313)
    (<sal-to-esm-context>
      314
      :synch-decl-list
      :choice-var-decl
      :choice-idx
      :choice-vars-queue
      :transition?)
    (<pre-vwaa-transition> 315 :label :target-list)
    (<vwaa-transition> 316 :label :target-set)
    (<ltl-context>
      317
      :manager
      :atom-table
      :bdd-var-idx->atom
      :pre-vwaa-transition-list-true
      :place-provider)
    (<vwaa-node>
      318
      :source
      :transition-list
      :final?)
    (<vwaa-graph>
      319
      :initial-nodes
      :final-nodes
      :node-table
      :formula->id
      :context)
    (<pre-gba-transition>
      320
      :label
      :target-set
      :mark-set)
    (<pre-gba-node>
      321
      :source-set-list
      :transition-list)
    (<gba-graph>
      322
      :initial-nodes
      :node-vector
      :num-marks
      :vwaa-node-mapping
      :context)
    (<gba-transition> 323 :label :target :mark-list)
    (<gba-node> 324 :source :transition-list)
    (<ba-transition> 325 :label :target)
    (<pre-ba-node> 326 :source-list :transition-list)
    (<ba-graph>
      327
      :initial-nodes
      :final-nodes
      :node-vector
      :gba-node-mapping
      :context)
    (<ba-node> 328 :source :transition-list)
    (<sal-command-expansion-info>
      329
      :choice-var-decl
      :choice-idx
      :choice-vars-queue
      :transition?)
    (<module-flattening-info>
      330
      :synch-decl-list
      :aux-env)
    (<dependency-info>
      331
      :graph
      :edge-table
      :context
      :next-vars
      :region
      :collect-implicit?
      :ignore-next-dependencies?)
    (<sal-bdd-context>
      332
      :bdd-manager
      :curr-vars
      :next-vars
      :tmp-bdd-var-list)
    (<sal-bdd-fsm>
      333
      :bdd-manager
      :curr-vars
      :next-vars
      :tmp-bdd-var-list
      :initial-states
      :flat-module
      :transition-relation-cluster
      :next-cube
      :curr-cube
      :curr-indices
      :next-indices
      :input-cube
      :choice-cube
      :next-var-array
      :curr-var-array
      :no-choice-var-array
      :num-vars
      :num-choice-vars
      :valid-input
      :valid-latch
      :reachable-states
      :layered-reachable-states
      :restriction
      :new-let-var-proc)
    (<sal-bdd-cluster> 334 :fsm)
    (<sal-bdd-monolithic-cluster> 335 :fsm :bdd)
    (<sal-bdd-disj-cluster> 336 :fsm :cluster-list)
    (<sal-bdd-conj-cluster>
      337
      :fsm
      :forward-cluster-element-list
      :backward-cluster-element-list)
    (<sal-bdd-cluster-element>
      338
      :cluster
      :e-vars-cube
      :info)
    (<sal-bdd-let-cluster>
      339
      :fsm
      :nested-cluster
      :let-var-cube
      :num-let-variables)
    (<iwls95-cluster-info>
      340
      :supp-Ti
      :supp-Q-Ci
      :PSPI-Ti
      :NS-Ti
      :v_c
      :w_c
      :x_c
      :y_c
      :z_c
      :m_c
      :M_c)
    (<sal-path>
      341
      :flat-module
      :step-info-list
      :auxiliary-decls
      :global-constraint-list)
    (<sal-step>
      342
      :assignment-table
      :constraint-list
      :transition-step)
    (<sal-concrete-path>
      343
      :flat-module
      :step-info-list
      :auxiliary-decls
      :global-constraint-list)
    (<sal-cyclic-path>
      344
      :flat-module
      :step-info-list
      :auxiliary-decls
      :global-constraint-list
      :cycle-step-info-list)
    (<sal-pseudo-cyclic-path>
      345
      :flat-module
      :step-info-list
      :auxiliary-decls
      :global-constraint-list)
    (<sal-df-info>
      346
      :new-decl-table
      :support-uninterpreted-types?)
    (<sat-context> 347)
    (<sat-generic-context>
      348
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider)
    (<sat-decl>
      349
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-global-decl>
      350
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-generic-context-model>
      351
      :sat-generic-context
      :atomic-constraint-queue)
    (<sal-var-eq-ground>
      352
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sal-var-eq-term>
      353
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sat-aux-decl>
      354
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-let-aux-decl>
      355
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sal-cyclic-eq>
      356
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<sat-lambda-aux-decl>
      357
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-skolem-aux-decl>
      358
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-alias-aux-decl>
      359
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-ite-aux-decl>
      360
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-ho-eq-aux-decl>
      361
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-type-constraint-aux-decl>
      362
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-scalar-bit-decl>
      363
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-scalar-bit-aux-decl>
      364
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<sat-generic-context-solver-interface> 365)
    (<sat-bmc-context> 366)
    (<sat-boolean-context> 367 :next-idx)
    (<sat-boolean-bmc-context> 368)
    (<sat-boolean-ics-context>
      369
      :next-idx
      :file-name
      :top-level-args)
    (<sat-boolean-ics-bmc-context>
      370
      :next-idx
      :file-name
      :top-level-args
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-boolean-cnf-context>
      371
      :next-idx
      :file-name
      :num-clauses
      :solver-proc)
    (<sat-boolean-cnf-bmc-context>
      372
      :next-idx
      :file-name
      :num-clauses
      :solver-proc
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-boolean-smt-context>
      373
      :next-idx
      :file-name
      :num-parenthesis
      :num-asserts
      :vars)
    (<sat-boolean-smt-bmc-context>
      374
      :next-idx
      :file-name
      :num-parenthesis
      :num-asserts
      :vars
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-generic-context-result>
      375
      :sat-context
      :result-constraint-list)
    (<sat-generic-bmc-context>
      376
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-step-decl>
      377
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<dp-translation-info>
      378
      :curr-idx
      :alias-idx
      :alias-queue
      :decl->id-mapping
      :id->decl-mapping
      :already-visited)
    (<sat-ics-context>
      379
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider)
    (<sat-ics-bool-aux-decl>
      380
      :place
      :context
      :hash
      :internal-idx
      :id
      :type)
    (<ics-term-true>
      381
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<ics-term-false>
      382
      :place
      :context
      :hash
      :internal-idx
      :type
      :decl
      :context-ref
      :actuals)
    (<sat-ics-bmc-context>
      383
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-svc-context>
      384
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider)
    (<sat-svc-bmc-context>
      385
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-cvcl-context>
      386
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider)
    (<sat-cvc-context>
      387
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider)
    (<sat-cvcl-bmc-context>
      388
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-cvc-bmc-context>
      389
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-uclid-context>
      390
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider)
    (<sat-uclid-bmc-context>
      391
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-yices-context>
      392
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider)
    (<sat-yices-bmc-context>
      393
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sat-yices2-context>
      394
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider)
    (<sat-yices2-bmc-context>
      395
      :declaration-queue
      :constraint-queue
      :scalar->bool-trace-info
      :scalar->int-trace-info
      :eliminated-declaration-queue
      :eliminated-constraint-queue
      :already-processed
      :place-provider
      :step-decl-tables
      :inv-step-decls
      :global-decls
      :inv-global-decls
      :max-step
      :flat-module)
    (<sal-transition-step>
      396
      :place
      :context
      :hash
      :internal-idx)
    (<sal-simple-transition-step>
      397
      :place
      :context
      :hash
      :internal-idx)
    (<sal-else-transition-step>
      398
      :place
      :context
      :hash
      :internal-idx)
    (<sal-nested-transition-step>
      399
      :place
      :context
      :hash
      :internal-idx
      :nested-transition)
    (<sal-labeled-transition-step>
      400
      :place
      :context
      :hash
      :internal-idx
      :nested-transition
      :label)
    (<sal-module-transition-step>
      401
      :place
      :context
      :hash
      :internal-idx
      :nested-transition)
    (<sal-module-instance-transition-step>
      402
      :place
      :context
      :hash
      :internal-idx
      :nested-transition)
    (<sal-nested-list-transition-step>
      403
      :place
      :context
      :hash
      :internal-idx
      :nested-transition-list)
    (<sal-let-transition-step>
      404
      :place
      :context
      :hash
      :internal-idx
      :nested-transition
      :with-var-value-list)
    (<ebv-info>
      405
      :step-vector
      :new-aux-var-decl-queue
      :transition-trace-info
      :curr-step)
    (<tvv-application>
      406
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<tvv2nat>
      407
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<nat2tvv>
      408
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<tvv-concat>
      409
      :place
      :context
      :hash
      :internal-idx
      :type
      :fun
      :arg)
    (<wmc-evidence>
      410
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<ag-wmc-evidence>
      411
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<ax-wmc-evidence>
      412
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<af-wmc-evidence>
      413
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<au-wmc-evidence>
      414
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<ar-wmc-evidence>
      415
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<eg-wmc-evidence>
      416
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<ex-wmc-evidence>
      417
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<ef-wmc-evidence>
      418
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<eu-wmc-evidence>
      419
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<er-wmc-evidence>
      420
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<atom-wmc-evidence>
      421
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<and-wmc-evidence>
      422
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<or-wmc-evidence>
      423
      :fp
      :expr
      :in
      :bad
      :sub1
      :sub2)
    (<wmc-witness-or-counterexample>
      424
      :bdd-list
      :fsm
      :expr)
    (<wmc-witness> 425 :bdd-list :fsm :expr)
    (<unary-wmc-witness>
      426
      :bdd-list
      :fsm
      :expr
      :sub)
    (<binary-wmc-witness>
      427
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<ag-wmc-witness> 428 :bdd-list :fsm :expr :sub)
    (<af-wmc-witness> 429 :bdd-list :fsm :expr :sub)
    (<ax-wmc-witness> 430 :bdd-list :fsm :expr :sub)
    (<au-wmc-witness>
      431
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<ar-wmc-witness>
      432
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<eg-wmc-witness> 433 :bdd-list :fsm :expr :sub)
    (<ef-wmc-witness> 434 :bdd-list :fsm :expr :sub)
    (<ex-wmc-witness> 435 :bdd-list :fsm :expr :sub)
    (<eu-wmc-witness>
      436
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<er-wmc-witness>
      437
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<atom-wmc-witness> 438 :bdd-list :fsm :expr)
    (<and-wmc-witness>
      439
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<or-wmc-witness>
      440
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<wmc-counterexample> 441 :bdd-list :fsm :expr)
    (<unary-wmc-counterexample>
      442
      :bdd-list
      :fsm
      :expr
      :sub)
    (<binary-wmc-counterexample>
      443
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<ag-wmc-counterexample>
      444
      :bdd-list
      :fsm
      :expr
      :sub)
    (<af-wmc-counterexample>
      445
      :bdd-list
      :fsm
      :expr
      :sub)
    (<ax-wmc-counterexample>
      446
      :bdd-list
      :fsm
      :expr
      :sub)
    (<au-wmc-counterexample>
      447
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<ar-wmc-counterexample>
      448
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<eg-wmc-counterexample>
      449
      :bdd-list
      :fsm
      :expr
      :sub)
    (<ef-wmc-counterexample>
      450
      :bdd-list
      :fsm
      :expr
      :sub)
    (<ex-wmc-counterexample>
      451
      :bdd-list
      :fsm
      :expr
      :sub)
    (<eu-wmc-counterexample>
      452
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<er-wmc-counterexample>
      453
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<atom-wmc-counterexample>
      454
      :bdd-list
      :fsm
      :expr)
    (<and-wmc-counterexample>
      455
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<or-wmc-counterexample>
      456
      :bdd-list
      :fsm
      :expr
      :valid1
      :valid2
      :sub1
      :sub2)
    (<sal-global-context>
      457
      :const-table
      :type-table
      :module-table
      :assertion-table
      :declarations
      :name-table
      :sal-env
      :curr-type-decl)
    (<sal-scm-context>
      458
      :global-function-decl-table
      :global-constant-decl-table
      :idx-val-table
      :val-idx-table
      :scm-decl-queue
      :gmp?
      :runtime-type-check?
      :debug?
      :loop-detection?
      :max-vector-size
      :compile?
      :type-check-table
      :pick-random-table)
    (<sal-esm-expansion-context>
      459
      :expand-everything?)
    (<sal-esm-dependencies-ctx>
      460
      :graph
      :lhs-to-node)
    (<sal-esm-used-provided-lhs-ctx>
      461
      :cached-used-and-provided-lhs
      :defined-variables
      :section-id
      :conservative?)
    (<sal-esm-engine-scm-context>
      462
      :global-function-decl-table
      :global-constant-decl-table
      :idx-val-table
      :val-idx-table
      :scm-decl-queue
      :gmp?
      :runtime-type-check?
      :debug?
      :loop-detection?
      :max-vector-size
      :compile?
      :type-check-table
      :pick-random-table
      :curr-var-idx-table
      :next-var-idx-table
      :section-id
      :cached-code
      :action-scm-decl-queue
      :randomize?
      :restricted-randomization?
      :symmetry?
      :curr-var-name-table
      :next-var-name-table
      :to-bitstream-proc-table
      :from-bitstream-proc-table
      :normalized-proc-table
      :cmp-proc-table
      :symmetry-constraint-table)))

(define (opt-info/class-info name)
  (cond ((assq name *class-global-info*) => cdr)
        (else #f)))

(define (opt-class-info/class-idx info)
  (car info))

(define (opt-class-info/slot->idx info slot-name)
  (let loop
    ((pos 2) (slots (cdr info)))
    (cond ((null? slots) #f)
          ((eq? slot-name (car slots)) pos)
          (else (loop (+ pos 1) (cdr slots))))))

(define (opt-class-info/slots info) (cdr info))
(define *generic-global-info*
  '((api-entry/print 0 0)
    (api-entry/print-header 1 0)
    (api-entry/match 2 0)
    (sal-ast/for-each-children 3 1)
    (sal-new-binds-ast/for-each-new-bind 4 1)
    (sal-ast/map 5 0)
    (sal-ast/deep-copy-core 6 0)
    (sal-ast/open-references-core 7 0)
    (sal-ast/unique-decls-core 8 0)
    (sal-ast->list 9 0)
    (sal-ast/instantiate-with 10 0)
    (pp/flatten 11 0)
    (pp/layout 12 0)
    (fits 13 0)
    (be-next 14 0)
    (object->doc 15 0)
    (sal-ast->lsal-doc 16 0 1)
    (sal-ast->sal-doc 17 0 1)
    (sal-application/infix? 18 0)
    (sal-application/precedence 19 0)
    (sal-application/associativity 20 0)
    (sal-var-rename-info/new-decl 21 0)
    (sal-ast/rename-variables-core 22 0)
    (sal-trace-info/rename 23 0)
    (sal-ast/local-simplify-core 24 0)
    (sal-not-arg/local-simplify 25 0)
    (sal-quantified-ast/local-simplify-core 26 0 1)
    (sal-ast/eager-local-simplify 27 0)
    (sal-ast/simplify-without-converting-inequalities-core
      28
      0)
    (sal-expr/evaluate-core 29 0)
    (sal-application/logic-id 30 0)
    (sal-name-expr/type 31 0)
    (sal-var-decl/definition 32 0)
    (sal-name-expr/definition 33 0)
    (sal-name-expr/constructor-accessors 34 0)
    (sal-name-expr/accessor-type 35 0)
    (sal-name-expr/accessor-other-accessors 36 0)
    (sal-argument->argument-list 37 0)
    (sal-expr/apply 38 0 1)
    (sal-expr/apply-conservative 39 0)
    (sal-expr/value-core? 40 0)
    (sal-lhs/ground? 41 0)
    (sal-expr/update-type! 42 0)
    (sign-invalid-app-error 43 0)
    (sal-expr->next-expr 44 0)
    (sal-type-check 45 0)
    (check-observer 46 0)
    (sal-state-var-decl/kind 47 0)
    (sal-command/else? 48 0)
    (sal-string-reader/scratch-context 49)
    (sal-string-reader/read-expr 50 0)
    (sal-string-reader/read-expr-using 51 0)
    (sal-string-reader/read-type 52 0)
    (sal-string-reader/read-module 53 0)
    (sal-string-reader/read-import 54 0)
    (sal-string-reader/read-assertion-name 55 0)
    (sal-dnf/and 56 0 1)
    (sal-dnf/or 57 0 1)
    (sal-dnf/not 58 0)
    (sal-dnf/for-each-cube 59 0)
    (sal-cube/for-each-literal 60 0)
    (sal-expr->dnf-core 61 0)
    (sal-dnf/cube-list 62 0)
    (sal-cube/literal-list 63 0)
    (bound/invert 64 0)
    (bound/strict->non-strict 65 0)
    (sal-expr/bound 66 0)
    (sal-type-decl/definition 67 0)
    (sal-type-name/definition 68 0)
    (sal-type/expand-if-type-name 69 0)
    (finite-type? 70 0)
    (sal-type/kind? 71 0)
    (sal-type/cast 72 0)
    (sal-type/boolean? 73 0)
    (sal-type/for-each-used-type 74 1)
    (sal-type/subtype-of? 75 0)
    (sal-type/number? 76 0)
    (sal-type->bounded-subtype 77 0)
    (sal-type/real? 78 0)
    (sal-type/integer? 79 0)
    (sal-type/natural? 80 0)
    (sal-type->type-list 81 0)
    (sal-type/make-iterator-core 82 0)
    (sal-type/value-idx 83 0)
    (sal-type/number-of-elements-core 84 0)
    (sal-type/super-type 85 0)
    (sal-type/equivalent? 86)
    (sal-type/equivalent-core? 87 0 1)
    (sal-type/union-core 88 0 1)
    (sal-ast/compare-children 89 0 1)
    (sal-ast/equivalent-core? 90 0 1)
    (sal-name-ref/equivalent? 91 0 1)
    (sal-ast/hash 92 0)
    (sal-module/update-state-variable-table! 93 0)
    (sal-module/update-interface! 94 0)
    (sal-module-name/definition 95 0)
    (sal-module-instance/expand 96)
    (sal-module/defined-variables 97 0)
    (sal-derived-flat-module/original-flat-module
      98
      0)
    (sal-derived-flat-module/original-boolean-flat-module
      99
      0)
    (sal-derived-flat-module/original-simple-data-flat-module
      100
      0)
    (sal-flat-module/slice-of? 101 0)
    (sal-decl/attribute 102 0)
    (sal-decl/name-class 103 0)
    (sal-decl/app-class 104 0)
    (sal-expr/application-class 105 0)
    (sal-ast/reset-cache! 106 0)
    (sal-name-ref/decl 107 0)
    (sal-qualified-name-ref/context-ref 108 0)
    (sal-qualified-name-ref/actuals 109 0)
    (sal-local-binds-ast/local-decls 110 0)
    (sal-selection/target 111 0)
    (sal-selection/idx 112 0)
    (sal-selection/update-target 113 0)
    (sal-expr/lhs? 114 0)
    (sal-lhs/name-expr 115 0)
    (sal-lhs/trivial? 116 0)
    (sal-lhs/next-operator 117 0)
    (sal-lhs/remove-next-operator 118 0)
    (sal-lhs->next-lhs 119 0)
    (sal-next-lhs->lhs 120 0)
    (sal-decl/recursive? 121 0)
    (sal-ast/check-number-of-actuals 122 0)
    (sal-command/guard 123 0)
    (sal-else-command/assignments 124 0)
    (sal-else-command/trace-info 125 0)
    (sal-trace-info/find-choice-var 126 0)
    (sal-ast->sxml 127 0)
    (sal-lhs/supported? 128 0)
    (sal-simple-definition/supported? 129 0)
    (sal-simple-definition/convert-to-supported
      130
      0)
    (get-new-lhs 131 0)
    (gen-new-rhs 132 0)
    (sal-assignment-info/union 133 0 1)
    (sal-lhs->assignment-info 134 0)
    (sal-assignment-info->implicit-value 135 0)
    (process-definition 136 0)
    (process-command 137 0)
    (polarity/invert 138 0)
    (sal-ast/map-using-polarity 139 0)
    (sal-expr->nnf-core 140 0 1)
    (sal-ast/expand-core 141 0)
    (sal-expr/lift 142 0)
    (sal-expr/auxiliary-decl-for 143 0)
    (make-function-base-on 144 0)
    (sal-quantified-expr/expand-core 145 0)
    (sal-ast/expand-quantifiers-core 146 0)
    (skolem-expand? 147 0 1)
    (skolem-expand-finite? 148 0 1)
    (sal-ast/expand-names 149 0)
    (sal-ast/expand-applications 150 0)
    (sal-cse/filter-occurrences-core 151 0)
    (sal-cse/compute-occurrences-core 152 0)
    (places-to-insert-new-decls 153 0)
    (cse-core 154 0)
    (sal-ast/cse 155 0)
    (collect-let-decls 156 0)
    (remove-pulled-let-decls 157 0)
    (sal-ast/pull-let-decls 158 0)
    (sal-type/finite-rep-num-bits 159 0)
    (sal-type/union-finite-rep 160 0 1)
    (sal-component-info/empty? 161 0)
    (sal-component-info/simplify 162 0)
    (sal-component-info/convert-data 163 0)
    (sal-type/convert-to 164 0 1)
    (sal-type/finite-rep-membership-expr 165 0)
    (sal-expr->boolean-expr-core 166 0)
    (bit-list->sal-value 167 0)
    (sal-ast->boolean-ast 168 0)
    (sal-assertion->boolean-assertion-core 169 0)
    (sal-type/nontrivial-membership-expr? 170 0)
    (sal-type/membership-expr 171 0)
    (sal->esm 172 0)
    (sal-esm/add-choice-to-queue 173 0)
    (sal-esm/add-seq-to-queue 174 0)
    (sal->esm-core 175 0)
    (sal-cmd->esm 176 0)
    (sal-guard->esm 177 0 3)
    (sal-definition->esm 178 0)
    (sal-else-cmd->esm 179)
    (combine-multi-synch-core 180 0)
    (sal-ltl-expr/collect-atoms-core 181 0)
    (ltl/le? 182 0 1)
    (ltl/simplify-core 183 0)
    (ltl->pre-vwaa-transition-list 184 0)
    (ltl->ctl-core 185 0)
    (ctl->ltl-core 186 0)
    (sal-assertion-name/definition 187)
    (sal-assertion/transformation-core 188 0)
    (make-module-models-with-monitor 189 0)
    (sal-definition/expand 190 0)
    (sal-ast/expand-for-all-definitions 191 0)
    (sal-ast/flat-modules-core 192 0)
    (sal-ast/flat-modules 193 0)
    (sal-command/flat-modules-core 194 0)
    (sal-definition/defined-variables 195 0)
    (sal-module/flat-modules-core 196 0)
    (set-of-support-core 197 0)
    (implicit-dependencies-core 198 0)
    (collect-dependencies 199 0)
    (collect-lhs-targets-core! 200 0)
    (sal-ast/slice 201 0)
    (remove-non-used-var-from-valid-expr-core 202 0)
    (sal-sliced-flat-module->flat-module 203 0)
    (sal-expr->bdd-core 204 0 2)
    (collect-implicit-lets 205 0)
    (sal-bdd-cluster/image 206 0)
    (sal-bdd-cluster/pre-image 207 0)
    (sal-bdd-cluster/disj-image 208 0)
    (sal-bdd-cluster/size 209 0)
    (sal-bdd-cluster/num-clusters 210 0)
    (sal-bdd-cluster->bdd 211 0)
    (sal-bdd-cluster/display-info 212 0)
    (sal-bdd-cluster/compress 213 0)
    (sal-bdd-cluster/push-tiny-bdds-core 214 0)
    (sal-bdd-cluster->cluster-list 215 0)
    (sal-bdd-cluster/support 216 0)
    (make-bdd-let-cluster 217 0)
    (sal-expr->cluster-core 218 0)
    (sal-bdd-fsm/reorder! 219)
    (sal-path/length 220 0)
    (sal-path/step-data 221 0)
    (sal-path/state-expr-core 222 0)
    (sal-path/cut 223)
    (sal-path/cut-after 224)
    (sal-path/evaluate-expr-at 225 1)
    (sal-path->cyclic-path 226 0)
    (sal-type/flattenable? 227 0)
    (sal-ast/display-df-warnings-core 228 0)
    (sal-type->flattened-var-decls 229 0)
    (sal-type/num-atom-types 230 0)
    (sal-expr/flat-data-core 231 0)
    (sal-esm/flat-data-core 232 0)
    (sal-ast/flat-data 233 0)
    (sal-assertion/flat-data 234 0)
    (populate-inv-flat-data-map-core! 235 0)
    (data-list->sal-value 236 0)
    (flat-data-ast->ast 237 0)
    (sat-context/make-or* 238 0)
    (sat-context/make-false 239 0)
    (sat-context/make-and* 240 0)
    (sat-context/make-not 241 0 1)
    (sat-context/make-true 242 0)
    (sat-context/make-ite 243 0 1 2 3)
    (sat-context/make-eq 244 0 1 2)
    (sat-context/make-iff 245 0 1 2)
    (sat-context/assert 246 0 1)
    (sat-context/add-auxiliary-decl 247 0)
    (sal-ast/remove-lets 248 0)
    (sal-ast/eliminate-array-theory 249 0)
    (sal-ast/pull-lambdas 250 0)
    (sal-ast/ho-eq-expansion 251 0 2)
    (sal-ast/expand-lambdas 252 0)
    (sal-ast/eliminate-quantifiers 253 0 2)
    (sal-ast/ite->bool-ite 254 0)
    (sal-ast/eliminate-div-mod 255 0)
    (sal-ast/eliminate-int-diseq 256 0 2)
    (sal-ast/polarity-breaker-for-ho-eq 257 0)
    (sal-type/eliminate-function-subtypes 258 0)
    (sal-type/no-constraints? 259 0)
    (sal-ast/add-type-constraints 260 0)
    (sal-ast/scalar->int 261 0)
    (sal-ast/scalar->bool 262 0)
    (sal-ast/eliminate-int-real-pred 263 0 2)
    (sal-ast/ite->decl 264 0)
    (sat-bmc-context/id-at 265 0)
    (sal-ast->sat 266 0 1 4)
    (make-path-extension-sat-context 267 0)
    (append-to-path 268 0)
    (remove-and-collect-pseudo-lets 269 0)
    (sal-ast/remove-pseudo-lets 270 0)
    (sal-ast/simple-abstraction 271 0)
    (sal-expr/simple-abstraction 272 0)
    (sal-ast/substitute-state-var-definitions 273 0)
    (sal-ast/def-var-substitution 274 0)
    (obj->flat-assertion 275 0)
    (obj->flat-module 276 0)
    (sat-boolean-context/make-aux-var 277 0)
    (sat-bmc-context/decl-at 278 0)
    (sat-context/solve 279 0)
    (sat-bmc-context/make-path 280 0)
    (sal-ast/sat-ground? 281 0)
    (sal-ast/apply-assignment 282 0)
    (expected-type 283 0)
    (sal-ast/recover-scalars-from-integers 284 0 1)
    (sat-generic-context-result/cleanup! 285)
    (sat-generic-context-result/make-path 286 0)
    (build-state-ast 287 0)
    (sal-constraint/normalize 288 0)
    (sal-constraint/invert 289 0)
    (sal-ast/side-insert! 290 0)
    (sal-inequality/swap-class 291 0)
    (sal-ast/create-bool-bridge 292 0)
    (sal-ast/display-ics 293 0)
    (sal-constraint/display-ics 294 0)
    (sal-ast/display-svc 295 0)
    (sal-constraint/display-svc 296 0)
    (sal-type/display-cvcl 297 0)
    (sal-ast/display-cvcl 298 0)
    (sal-constraint/display-cvcl 299 0)
    (sal-type/display-uclid 300 0)
    (sal-ast/display-uclid 301 0)
    (sal-term/collect-data 302 0)
    (sal-constraint/display-uclid 303 0)
    (sal-type/display-yices 304 0)
    (sal-ast/display-yices 305 0)
    (sal-constraint/display-yices 306 0)
    (sal-type/display-yices2 307 0)
    (sal-ast/display-yices2 308 0)
    (sal-constraint/display-yices2 309 0)
    (make-sal-transition-step-core 310 0)
    (sal-transition-step->doc 311 0)
    (sal-derived-path->original-path-core 312 0 1)
    (sal-derived-input-seq->original-input-seq-core
      313
      0
      1)
    (find-original-choice-var-name 314 0)
    (sal-ast/eliminate-bad-vars! 315 0)
    (sal-module/var-bits 316 0)
    (var-decls->var-values 317 0)
    (derived-value->value 318 0)
    (sal-path/recover-executed-transition-info!
      319
      0)
    (sal-value->assignments 320)
    (sal-value->assignments-core 321 0 1)
    (sal-var-decl/display-value 322)
    (sal-var-decl/display-value-differences 323)
    (sal-path/display 324 0)
    (sal-smc/find-path-from-initial-state 325 0 1)
    (sal-smc/find-path-from-initial-state-with-at-most
      326
      0
      1)
    (make-state-expression-bdd 327 0)
    (temporal? 328 0)
    (wmc-for-each 329 0)
    (sal-wmc 330 1)
    (sal-wmc/symbolic-witness 331 0)
    (sal-wmc/symbolic-counterexample 332 0)
    (sal-wmc/get-initial-bdd 333 0)
    (sal-wmc/display-trace 334)
    (sal-wmc/display-trace-starting-at 335 0)
    (sal-ast/flat-globals 336 0)
    (x-g-property? 337 0)
    (x-g-property-body 338 0)
    (sal-scm-context/add-decl! 339)
    (scm-value->sal 340 1)
    (scm-value->sal-array 341 1 2)
    (sal-type/simple-idx-val? 342 0)
    (sal-type/idx->val 343 0)
    (sal-type/idx->val-proc 344 0)
    (sal-type/val->idx 345 0)
    (sal-type/val->idx-proc 346 0)
    (sal-type/setup-code 347 0)
    (sal-scm/encoded-as-lambda? 348 0)
    (sal->scm 349 0)
    (sal-scm/create-lambda 350 0)
    (sal-type->scm-iterator 351 0)
    (subrange-iterator 352)
    (sal-type/gen-eq-pred 353 0)
    (sal-type/gen-check-membership-pred-core 354 0)
    (esm->dnf-esm 355 0)
    (esm->dnf-esm-core 356 0)
    (sal-esm-expansion-context/expand 357)
    (sal-esm/expand-core 358 0)
    (sal-ast/collect-state-lhs! 359 0)
    (sal-esm/collect-dependencies! 360 0)
    (sal-lhs/subsumes? 361 1)
    (merge-common-lhs-core 362 1)
    (sal-lhs-target-type/num-components 363 0)
    (sal-lhs-type/components 364 0)
    (sal-esm/used-and-provided-lhs 365 0)
    (sal-esm/rearrange 366 0)
    (sal-esm-engine-scm-context/compile-code! 367)
    (sal-module/transient-vars 368 0)
    (sal-ast/collect-non-transient! 369 0)
    (sal-esm/collect-lhs-name-expr 370 0)
    (sal-esm/uses-next-operator? 371 0)
    (collect-defined-vars-input-dependent! 372 0)
    (break-definitions-in-two-lists 373 0)
    (decide-definition-kind 374 0)
    (sal-esm/remove-non-det-parts 375 0)
    (sal-esm/num-alternatives 376 0)
    (sal-esm/has-guards? 377 0)
    (sal-esm->next-alt-proc 378 0)
    (sal-esm->exec-alt-proc 379 0)
    (collect-case-target-core! 380 0)
    (erase-guard-exprs-subsumed-by-case-stmt 381 0)
    (sal-esm/promote-choice-to-case 382 0)
    (sal-esm/mark-may-delay! 383 0)
    (sal-esm/collect-access-levels! 384 0)
    (sal-lhs/access-level 385 0)
    (sal-esm-module/initialization-access-levels 386)
    (sal-esm-module/transition-access-levels 387)
    (sal-esm/collect-must-initialization 388 0)
    (sal-esm/collect-may-initialization 389 0)
    (lhs->ground-lhs 390 0)
    (add-implicit-initialization! 391 0)
    (sal-type/val->bitstream 392 0)
    (sal-type/val->bitstream-proc 393 0)
    (sal-function-type/val->bitstream-proc 394 0)
    (sal-type/simple? 395 0)
    (sal-type/bitstream->val 396 0)
    (sal-type/bitstream->vector-proc 397 0)
    (sal-type/gen-normalize 398 0)
    (sal-type/gen-normalize-proc 399 0)
    (sal-function-type/gen-normalize-proc 400 0)
    (sal-type/gen-cmp 401 0)
    (sal-type/gen-cmp-proc 402 0)
    (sal-function-type/gen-cmp-proc 403 0)
    (sal-ast/used-contexts-core 404 0)))

(define (opt-info/generic-info name)
  (cond ((assq name *generic-global-info*) => cdr)
        (else #f)))

(define (opt-generic-info/idx info) (car info))
(define (opt-generic-info/discriminator-pos-list info)
  (cdr info))

(define (opt-info/num-classes) 462)
(define (opt-info/num-generics) 405)
