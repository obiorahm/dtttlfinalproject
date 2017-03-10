globals [expressions postfix_expression stack evaluation-stack yincr currx curry expression-stack
  hide-operators operand-list turtle-tracker turtle-stack curr-step circle-count
circle-stack]
breed [circle-models circle-model]
breed [circles circle]
breed [boperators boperator]
breed [breed-results breed-result]
breed [banners banner]


to Evaluate
  print Input_Expression
  show user-input Input_Expression
end

;;set up variables for parsing expression to post-fix form
to parseEntry
  ct
  let expression Input_Expression
  set postfix_expression []
  set stack []
  make-postfix-expression expression
  print stack
  final-pass
  ;;set curr-step 0
  print sentence "another stack" postfix_expression
end




to-report operator? [sign]
  report (sign = "+" or sign = "-" or sign = "/" or sign = "*")
end

;; converts the infix expression to a a post fix expression
to make-postfix-expression [expression]
  if empty? expression [stop]
  let expression_item first expression
  carefully
  [let operand read-from-string expression_item
    set postfix_expression sentence postfix_expression operand
    print postfix_expression]
  [if operator? expression_item
    [ print "its an operator"
      if-else empty? stack
      [set stack sentence stack expression_item]
      [loop-pop-stack expression_item
      push-operator-stack expression_item]
      print stack]]
    make-postfix-expression butfirst expression
end

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

;; test precedence of two operators
to-report test-precedence [curr_operator stack_operator]
  report get-precedence stack_operator >= get-precedence curr_operator
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

;; go through each step slowly
to step-through
  if empty? postfix_expression [set curr-step 0 stop]
  if curr-step = 0 [parseEntry reset-variables ]
  let expression_item first postfix_expression
  if-else not operator? expression_item
  [ push-stack expression_item "evaluation-stack"
    make-turtle 3 expression_item]
  [ let operandA pop-stack "evaluation-stack"
    let operandB pop-stack "evaluation-stack"
    let evaluation arithmetic-evaluate expression_item operandA operandB
    draw-operator-and-result expression_item evaluation
    push-stack evaluation "circle-stack"
    draw-circle circle-stack
    push-stack evaluation "evaluation-stack"]
  print evaluation-stack
  set postfix_expression butfirst postfix_expression
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
  if stack-string = "circle-stack"
  [set circle-stack sentence operand circle-stack]
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

end


;; get the first value of the operator stack
to pop-operator-stack
  let stack_item first stack
  set postfix_expression sentence postfix_expression stack_item
  print sentence "this is" postfix_expression
  set stack butfirst stack
end

to push-operator-stack [new-stack-item]
  set stack sentence new-stack-item stack
end


;;This function sets up the evaluation-stack and then evaluates its content
to setup-evaluate-expression
  reset-variables
  evaluate-expression
  print last evaluation-stack
  reset-ticks
end


to reset-variables
  set evaluation-stack []
  set circle-stack []
  set hide-operators true
  set operand-list [0 0]
  set turtle-tracker 0
  set curr-step 1
  set circle-count 1
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
  ct
  set-default-shape turtles specify-model
  set currx max-pxcor
  set curry max-pycor - 1
  set yincr 4

end

to-report specify-model
  if Node_chooser = "Circle" [report "circle"]
  if Node_chooser = "Square" [report  "square 2"]
  if Node_chooser = "Tree" [report "circle"]
  if-else Node_chooser = "leaf" [report "leaf"]
  [report "square 2"]
end


;; makes a turtle attaches a another turtle to it that controls
;; the values displayed.
to make-turtle [xincr turtle-label]
  set currx currx + xincr
  create-circles 1 [
    track-operand-list who
    push-stack who "turtle-stack"
    make-all-turtles turtle-label
  ]
    ;ask banners [ reposition]

end

to make-all-turtles [turtle-label]
    setxy currx curry
    set size 3
    attach-banner turtle-label
end


to make-operator-turtle [xincr turtle-label]
  set currx currx + xincr
  create-boperators 1
  [make-all-turtles turtle-label
    operand-to-operator-links who
  ]
end


to make-result-breed-turtle [xincr turtle-label]
  set currx currx + xincr
  create-breed-results 1
  [ ;track-operand-list who
    push-stack who "turtle-stack"
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



to circle-name [x]
  ask circle x [hide-turtle]
end

;; keeps track of turtles that are currently being operated on
to track-operand-list [curr-turtle]
  if-else remainder turtle-tracker 2 = 0
  [
    set operand-list but-first operand-list
    set operand-list sentence curr-turtle operand-list
    show operand-list

    set turtle-tracker turtle-tracker + 1
  ]
  [
    set operand-list but-last operand-list
    set operand-list sentence operand-list curr-turtle
    show operand-list
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


;; concentric circles
to draw-circle [circle-label]
  make-circle-model circle-label
end


to make-circle-model [local-circle-stack]
  ask circle-models [
    ask link-neighbors [if breed = banners [die]]
    die]
  let stack-length length local-circle-stack
  loop [
    if empty? local-circle-stack [stop]

    create-circle-models 1 [
    set shape "square"
    set size stack-length * 3
    setxy (max-pxcor / 2) (max-pycor / 2)
    attach-banner first local-circle-stack
    let circle-size (size / 2)
    ask link-neighbors [if breed = banners [reposition 45 circle-size - 1 show circle-size]]

    set stack-length stack-length - 1]
    set local-circle-stack butfirst local-circle-stack
  ]



end


@#$#@#$#@
GRAPHICS-WINDOW
452
104
977
630
-1
-1
15.67
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
449
17
653
77
Input_Expression
2+1-4*5+2
1
0
String

BUTTON
449
641
619
674
Evaluate
evaluate
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
712
642
880
675
Parse Expression
ParseEntry
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
183
100
275
145
Addition
Addition
1 2 3 4
0

CHOOSER
183
144
275
189
Subtraction
Subtraction
1 2 3 4
0

CHOOSER
183
233
275
278
Multiplication
Multiplication
1 2 3 4
2

CHOOSER
183
189
275
234
Division
Division
1 2 3 4
1

BUTTON
450
688
617
721
Evaluation Expression
setup-evaluate-expression
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
168
78
318
96
Specify Precedence Rules 
11
0.0
1

CHOOSER
821
18
975
63
Node_Chooser
Node_Chooser
"Circle" "Square" "Tree" "leaf"
3

BUTTON
713
687
944
720
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
1154
101
1262
146
Addition1
Addition1
1 2 3 4
0

CHOOSER
1154
146
1262
191
Subtraction1
Subtraction1
1 2 3 4
2

CHOOSER
1154
192
1263
237
Division1
Division1
1 2 3 4
0

CHOOSER
1153
238
1264
283
Multiplication1
Multiplication1
1 2 3 4
0

TEXTBOX
1115
78
1299
106
Specify Second Precedence Rules
11
0.0
1

BUTTON
169
485
285
518
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
168
536
346
569
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
652
18
822
63
Model_Chooser
Model_Chooser
"Tree" "Concentric Circles" "Concentric Squares"
0

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

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
