globals [expression postfix_expression stack evaluation-stack yincr currx curry expression-stack
  hide-operators operand-list turtle-tracker turtle-stack curr-step circle-count
circle-operator-stack circle-stack current-rule circle-turtle-stack
first-operation more-digits]
breed [circle-models circle-model]
breed [circles circle]
breed [boperators boperator]
breed [breed-results breed-result]
breed [banners banner]


;;set up variables for parsing expression to post-fix form
;;call make-postfix expression to do actual conversion to postfix form
;; call final-pass to empty the stack of lesser priority operators
to parseEntry
  set expression Input_Expression
  set postfix_expression []
  set stack []
  make-postfix-expression
  final-pass
  set curr-step 0
end


to-report operator? [sign]
  report (sign = "+" or sign = "-" or sign = "/" or sign = "*")
end

;; converts the infix expression to a a post fix expression
to make-postfix-expression
  if empty? expression [stop]
  let expression_item first expression
  carefully
  [let operand read-from-string expression_item
    set postfix_expression sentence postfix_expression operand]
  [if operator? expression_item
    [if-else empty? stack
      [set stack sentence stack expression_item]
      [loop-pop-stack expression_item
      push-operator-stack expression_item]]]

    make-postfix-expression butfirst expression
end

to multiple-digit-read
  let number first expression
  if (operator? number)[stop]
  set more-digits (word more-digits number)
  set expression butfirst expression
  multiple-digit-read
end

;;takes list of operators that have not been parsed by make-postfix expression loop and empties it
;;by adding the operators on to the postfix stack
to final-pass
  if empty? stack [stop]
  set postfix_expression sentence postfix_expression first stack
  set stack butfirst stack
  final-pass
end


;; get the currently set operator precedence
to-report get-precedence [operator]
  if operator = "+" [report Addition]
  if operator = "-" [report Subtraction]
  if operator = "*" [report Multiplication]
  if operator = "/" [report Division]
end

;;get the second set of operator precedence
to-report get-second-precedence [operator]
  if operator = "+" [report Addition1]
  if operator = "-" [report Subtraction1]
  if operator = "*" [report Multiplication1]
  if operator = "/" [report Division1]
end

;; test precedence of two operators: the operator that is currently being processed and
;; the operator that is at the top of the stack
to-report test-precedence [curr_operator stack_operator]
  if-else current-rule = 1
  [report get-precedence stack_operator >= get-precedence curr_operator]
  [report get-second-precedence stack_operator >= get-second-precedence curr_operator]

end

;; loop through the stack to pop the stack if condition to pop the stack is satisfied
to loop-pop-stack [curr_operator]
  if empty? stack [stop]
  if not condition-to-pop-stack curr_operator (first stack) [stop]
  pop-operator-stack
  loop-pop-stack curr_operator
end


;; conditions to pop the stack
;; we can pop the stack if the stack is not empty and the priority of the current operator
;; is less than the priority of the operator in the stack
to-report condition-to-pop-stack [curr_operator stack_operator]
 report test-precedence curr_operator stack_operator
end

;;now evaluate the post fix expression
to evaluate-expression
  if empty? postfix_expression [set curr-step 0 stop]
  if curr-step = 0 [parseEntry reset-variables ]
  step-through
  evaluate-expression
end

to set-rule-and-parse [rule]
  set current-rule rule
  parseEntry
end

to parse-and-evaluate
  ct
  set-rule-and-parse 1
  reset-variables
  evaluate-expression

set-rule-and-parse 2
  reset-variables
  evaluate-expression
end

;; evaluate the expression one operation at a time
to step-through
  if empty? postfix_expression [set curr-step 0 stop]
  if curr-step = 0 [parseEntry reset-variables ]
  let expression_item first postfix_expression
  if-else not operator? expression_item
  [ push-stack expression_item "evaluation-stack"
    choose-draw-operand-model expression_item]
  [ let operandA pop-stack "evaluation-stack"
    let operandB pop-stack "evaluation-stack"
    let evaluation arithmetic-evaluate expression_item operandA operandB
    choose-draw-operator-model expression_item evaluation operandA operandB
    push-stack evaluation "evaluation-stack"]
  set postfix_expression butfirst postfix_expression
end


to choose-draw-operator-model [expression_item evaluation operandA operandB]
  if Model_chooser = "Tree" [draw-operator-and-result expression_item evaluation stop]
  if Model_Chooser = "Concentric Circles"
  [ draw-circle-or-square evaluation expression_item "circle"]
  if Model_Chooser = "Concentric Squares"
  [ draw-circle-or-square evaluation expression_item "square"]
end

to choose-draw-operand-model [expression_item]
  if Model_Chooser = "Tree"
  [make-turtle 3 expression_item]
  if (Model_Chooser = "Concentric Circles" or Model_Chooser = "Concentric Squares")
  [push-stack (list expression_item expression_item) "circle-operator-stack"]

end

;; this draws the operator and the result of executing the operator
to draw-operator-and-result [operator result]
    set curry curry - yincr
    make-operator-turtle -1.5 operator
    ;set curry curry - yincr
    make-result-breed-turtle 0 result

end

to push-stack [operand stack-string]
  if stack-string = "evaluation-stack"
  [set evaluation-stack sentence operand evaluation-stack]
  if stack-string = "turtle-stack"
  [set turtle-stack sentence operand turtle-stack]
  if stack-string = "circle-operator-stack"
  [set circle-operator-stack lput operand circle-operator-stack]
  if stack-string = "circle-stack"
  [set circle-stack sentence operand circle-stack]
  if stack-string = "circle-turtle-stack"
  [set circle-turtle-stack sentence operand circle-turtle-stack]
end

to-report pop-stack [stack-string]
  if stack-string = "evaluation-stack"[
    let popped-value first evaluation-stack
    set evaluation-stack butfirst evaluation-stack
    report popped-value]
if stack-string = "turtle-stack"[
    let popped-value first turtle-stack
    set turtle-stack butfirst turtle-stack
    report popped-value]
if stack-string = "circle-stack"[
    let popped-value first circle-stack
    set circle-stack butfirst circle-stack
    report popped-value]
if stack-string = "circle-operator-stack"[
    let popped-value last circle-operator-stack
    set circle-operator-stack butlast circle-operator-stack
    report popped-value]
if stack-string = "circle-turtle-stack"[
    let popped-value last circle-turtle-stack
    set circle-turtle-stack butlast circle-turtle-stack
    report popped-value]
end


;; get the first value of the operator stack
to pop-operator-stack
  let stack_item first stack
  set postfix_expression sentence postfix_expression stack_item
  set stack butfirst stack
end

to push-operator-stack [new-stack-item]
  set stack sentence new-stack-item stack
end


;;This function sets up the evaluation-stack and then evaluates its content
to setup-evaluate-expression
  reset-variables
  evaluate-expression
  reset-ticks
end


to reset-variables
  set evaluation-stack []
  set circle-stack []
  set circle-operator-stack []
  set circle-turtle-stack []
  set hide-operators true
  set operand-list [0 0]
  set turtle-tracker 0
  set curr-step 1
  set circle-count 1
  set first-operation true
  setup-make-turtle
end

;; This function takes an operand and 2 operators and evaluates them
to-report arithmetic-evaluate [operator operandA operandB]
  if operator = "+" [report operandB + operandA]
  if operator = "-" [report operandB - operandA]
  if operator = "*" [report operandB * operandA]
  if operator = "/" [report operandB / operandA]

end

to setup-make-turtle
  ;;ct
  set-default-shape turtles specify-model
  position-turtle-based-on-precedence
  set yincr 4

end

to position-turtle-based-on-precedence
  if-else current-rule = 1
  [set currx max-pxcor
    set curry max-pycor - 1]
  [set currx 0
    set curry max-pycor - 1]
end

to-report specify-model
  if Node_chooser = "Circle" [report "circle"]
  if Node_chooser = "Square" [report  "square 2"]
  if Node_chooser = "Tree" [report "circle"]
  if-else Node_chooser = "leaf" [report "leaf"]
  [report "square 2"]
end

to hide-if-concentric-model
  if not (Model_Chooser = "Tree")
  [ask link-neighbors [if breed = banners [hide-turtle]]
    hide-turtle]
end

;;makes the turtles for operands breed
;;pushes to turtle stack the turtle number that was just created
to make-turtle [xincr turtle-label]
  set currx currx + xincr
  create-circles 1 [
    track-operand-list who
    push-stack who "turtle-stack"
    make-all-turtles turtle-label
    hide-if-concentric-model]
end

;; sets the position and the size of the turtles created
;; it also creates a label-turtle for each turtle with attach-banner
;; several breeds use this function
to make-all-turtles [turtle-label]
    setxy currx curry
    set size 3
    attach-banner turtle-label
end

;;operator turtles are as the breed boperators
;;they have links to two operand turtles which could be boperator or circle breed
to make-operator-turtle [xincr turtle-label]
  set currx currx + xincr
  create-boperators 1
  [make-all-turtles turtle-label
    operand-to-operator-links who
  ]
end

;; breed-results turtles are created every time an operation is executed
to make-result-breed-turtle [xincr turtle-label]
  set currx currx + xincr
  create-breed-results 1
  [ ;track-operand-list who
    push-stack who "turtle-stack"
    hide-turtle
    make-all-turtles turtle-label]
end

;; the banner breed is setup
to attach-banner [x]  ;; circle procedure
  hatch-banners 1 [
    set size 0
    set label x
    create-link-from myself [
      tie
      hide-link
    ]
  ]
end

;; control the position of the label on the turtle
to reposition  [banner-heading banner-distance]
  move-to one-of in-link-neighbors
  set heading banner-heading
  fd banner-distance
end

;;switch between operator and evaluated value
to hide-operator-turtles
  if-else hide-operators
  [ask boperators [hide-turtle
    ask link-neighbors [if breed = banners [hide-turtle]]]
    ask breed-results [show-turtle
    ask link-neighbors [if breed = banners [show-turtle]]]
  set hide-operators false]

  [ask boperators [show-turtle
    ask link-neighbors [if breed = banners [show-turtle]]]
  ask breed-results [hide-turtle
    ask link-neighbors [if breed = banners [hide-turtle]]]
  set hide-operators true]
end



;; keeps track of turtles that are currently being operated on
to track-operand-list [curr-turtle]
  if-else remainder turtle-tracker 2 = 0
  [
    set operand-list but-first operand-list
    set operand-list sentence curr-turtle operand-list
    ;show operand-list

    set turtle-tracker turtle-tracker + 1
  ]
  [
    set operand-list but-last operand-list
    set operand-list sentence operand-list curr-turtle
    ;show operand-list
    set turtle-tracker turtle-tracker + 1
  ]
end



to operand-to-operator-links [operator]
  let turtleA pop-stack "turtle-stack"
  let turtleB pop-stack "turtle-stack"
  ask boperator operator [carefully [create-link-from circle turtleA]
    [create-link-from breed-result turtleA] ]
  ask boperator operator [carefully [create-link-from circle turtleB]
    [create-link-from breed-result turtleB] ]
end


;; concentric circles or squares
to draw-circle-or-square [evaluation expression_item  model-shape]
  let operatorA pop-stack "circle-operator-stack"
  let operatorB pop-stack "circle-operator-stack"

  if-else first-operation = true
    [ kill-and-loop (word (last operatorB) " " expression_item " " (last operatorA)) model-shape
      set first-operation false
      push-stack (list expression_item "trail" evaluation) "circle-operator-stack" ]
    [ if-else empty? circle-operator-stack
      [ kill-and-loop (word expression_item " " (ifelse-value (operator? (first operatorA) and (not (operator? (first operatorB))))
        [last operatorB]
        [last operatorA])) model-shape
    push-stack (list expression_item
      (ifelse-value ((item 1 operatorA) = "trail" or (item 1 operatorB) = "trail")["trail"]["trail"])
      evaluation)

    "circle-operator-stack" ]
      [if-else ((item 1 operatorA) = "trail" or (item 1 operatorB) = "trail")
        [ kill-and-loop (word expression_item " "
          (ifelse-value ((item 1 operatorA) = "trail")
            [last operatorB][last operatorA])
          ;;(ifelse-value (operator? (first operatorA) and (not (operator? (first operatorB)))) [last operatorB][last operatorA])
    ) model-shape
    push-stack (list expression_item "trail" (word (last operatorB) expression_item (last operatorA)))  "circle-operator-stack"]
        [push-stack (list expression_item " " (word "(" (last operatorB) expression_item (last operatorA) ")" ))  "circle-operator-stack"]]]



  ;;if-else operator? (first operatorA) or operator? (first operatorB)
  ;;[push-stack (list expression_item (word (last operatorB) expression_item (last operatorA))) "circle-operator-stack" ]
  ;;[]

end


;; I kill the turtles because turtles are redrawn from scratch every loop
;; If not larger circles and squares will be on smaller ones
to kill-turtles-in-stack
  loop [
    if empty? circle-turtle-stack [stop]
    ask circle-model (first circle-turtle-stack) [
      ask link-neighbors [if breed = banners [die]]
      die]
    set circle-turtle-stack butfirst circle-turtle-stack]
  set circle-turtle-stack []
end

to draw [label-model model-shape stack-length]

    create-circle-models 1 [
    set shape model-shape
    set size stack-length * 3
    position-circle-turtle-based-on-precedence stack-length
    attach-banner label-model
    let circle-size (size / 2)
    ask link-neighbors [if breed = banners [reposition 45 circle-size - 1 ;show circle-size
    ]]
    push-stack who "circle-turtle-stack"
    ]
end


;; draws turtles for content of the cicle stack as it loops through the stack
to loop-circle-stack [local-circle-stack model-shape]
    if empty? local-circle-stack [stop]
    let stack-length length local-circle-stack
    draw (first local-circle-stack) model-shape stack-length
    set local-circle-stack butfirst local-circle-stack
    loop-circle-stack local-circle-stack model-shape
end


to kill-and-loop [label-model model-shape]
  push-stack label-model "circle-stack"
  kill-turtles-in-stack
  loop-circle-stack circle-stack model-shape

end

;;start at different positions on the screen note min-pxcor and max-pxcor
to position-circle-turtle-based-on-precedence [stack-length]
  let global-stack-length length circle-stack
  let extra (global-stack-length - stack-length)
  if-else current-rule = 1
  [setxy (max-pxcor * 3 / 2) - extra (max-pycor / 2) - extra]
  [setxy (min-pxcor * 3 / 2) - extra (max-pycor / 2) - extra]
end
@#$#@#$#@
GRAPHICS-WINDOW
161
97
682
619
-1
-1
15.55
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

INPUTBOX
158
10
362
70
Input_Expression
5+2*3+1-5
1
0
String

BUTTON
687
416
855
449
Parse Expression
\nset-rule-and-parse Step_through_model
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
50
96
159
141
Addition
Addition
1 2 3 4
1

CHOOSER
50
140
159
185
Subtraction
Subtraction
1 2 3 4
1

CHOOSER
51
229
159
274
Multiplication
Multiplication
1 2 3 4
3

CHOOSER
51
185
159
230
Division
Division
1 2 3 4
0

BUTTON
687
554
917
587
Evaluation Expression
parse-and-evaluate
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
54
74
204
92
Specify Precedence Rules 
11
0.0
1

CHOOSER
529
25
683
70
Node_Chooser
Node_Chooser
"Circle" "Square" "leaf"
1

BUTTON
686
589
917
622
Operator/Sub-evaluation Switch
hide-operator-turtles
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
688
100
796
145
Addition1
Addition1
1 2 3 4
0

CHOOSER
688
145
796
190
Subtraction1
Subtraction1
1 2 3 4
0

CHOOSER
688
191
797
236
Division1
Division1
1 2 3 4
1

CHOOSER
687
237
798
282
Multiplication1
Multiplication1
1 2 3 4
1

TEXTBOX
649
77
833
105
Specify Second Precedence Rules
11
0.0
1

BUTTON
689
513
856
546
Step Through
step-through
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
687
449
855
482
Animated Step Through
step-through\ntick
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
360
25
530
70
Model_Chooser
Model_Chooser
"Tree" "Concentric Circles" "Concentric Squares"
1

CHOOSER
688
338
853
383
Step_through_model
Step_through_model
1 2
0

BUTTON
686
383
853
416
clear
ct\nreset-ticks
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

The model tries to show how arithmetic expressions are evaluated in order.

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

The model accepts infix arithmetic expressions and converts them to postfix expressions. 
Then evaluates the expression according to the set rules. The model then shows how the expression is being evaluated - step by step. For the tree model, it represents operands and operators with nodes (turtles) and creates links between operand and operator nodes as the operands are being operated on.

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

Enter an arithmetic expression into the Input_Expression input box. The model expects binary operations and does not recognise negative numbers though expressions may result to negative numbers. The model also expects no spaces between operands and operators.

Specify 2 sets of rules. One rule for the model that is drawn on the left side of the screen and the other set of rules for the model that is drawn on the right side of the screen.

Select your preferred visualization. The model provides a tree, circle and square visualization. 

If you choose a tree visualization, then you have the option to specify the shape of the nodes on the tree. They can be squares, circles or leaves.

To see the visualization press the "evaluate expression" button.

To see an animated view of the visualization select which of the rules you want to display first (left or right side rules) clear the sreen with the "clear" button, press the "parse" button. The parse button converts the expression to a form that can be evaluated by the program. Then press the animated step through button.

To step through manually, clear the screen, parse the expression and then press the step-through button until the visualization gets to where you desire or until the computation is complete.


## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

In the tree model, you can see a pattern of lesser priority operators appearing towards the left of the tree and higher priority operators appear towards the right side of the tree. They also seem to branch out of the main stream branch.

Also, in the concentric square and circle models, note that higher priority operators do not seem to appear alone in a square or circle.

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

The model does not recognise parenthesis and negative numbers. One extension would be to make the model so that it recognises parenthesis and negative numbers. The branches of the tree may also be animated and their shapes changed. Trees may also be built manually with the software interpreting the rules from the tree.

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

I could not get to directly specify the order in which turtles are drawn on the same patch or around the same patch. I had to redraw smaller squares on the bigger squares after each computation.

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
