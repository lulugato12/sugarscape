globals [d u y x z w per Wt SSt pi_tM estados siniestralidad as1 as0 das as1t as0t dast trim died ciclo ecm ticks1 aseg_ed]
breed [agentes agente]
breed [asegurados asegurado]
asegurados-own [uti total seguro exper riqueza pi_t perdida seg_edad asedad asedadlim obligado] 
agentes-own [comisiones capacidad cartera aedad aedadlim ligas]
patches-own [ocupado dist]

;Best found so far: intercept=-5.59000 capac=-0.220000 social=0.235300 expe=0.143000 riq=1.63600 real=On  Fitness= 2.98467
;Best found so far: intercept=-4.16700 capac=-0.452000 social=0.337300 expe=0.216000 riq=1.63400 real=On  Fitness= 3.73438
;intercept=-7.44900 capac=1.39100 social=0.0950000 expe=0.467000 riq=0.169000 real=On  Fitness= 0.0151818
;intercept=-4.44800 capac=-1.83200 social=1.93700 expe=1.64200 riq=0.462000 real=On
;intercept=-0.326000 capac=0.0990000 social=-0.00200000 expe=0.00700000 riq=-0.635000 real=On
;intercept=-4.44800 capac=0.0430000 social=1.93700 expe=1.64200 riq=0.462000 real=On  Fitness= 0.556191
;Best found so far: intercept=-4.44800 capac=0.0430000 social=1.93700 expe=1.64200 riq=0.462000 real=On  Fitness= 0.556191
;Best found so far: intercept=-9.44800 capac=0.0430000 social=1.93700 expe=1.64200 riq=0.462000 real=On  Fitness= 0.556191
; intercept=-1.69200 capac=1.40900 social=0.746000 expe=-1.92700 riq=0.0430000 real=On  Fitness= 0.556191
;Best found so far: intercept=-1.33900 capac=1.35000 social=1.70800 expe=1.40000 riq=-1.07500 real=On
;Best found so far: intercept=-10.3320 capac=1.26800 social=1.28100 expe=0.370000 riq=0.571000  Fitness= 0.660204
;intercept=-0.0990000 capac=1.35000 social=1.70800 expe=1.40000 riq=-1.07500 real=On
;Best found so far: intercept=-1.57500 capac=0.781000 social=0.448000 expe=1.71100 riq=-1.32700  Fitness= 0.460897
;Best found so far: intercept=-0.0150000 capac=1.15700 social=1.68900 expe=0.379000 riq=-1.68700 real=On
;From all searches:  Best found so far: intercept=-0.179000 capac=1.76200 social=0.0200000 expe=1.15200 riq=-1.10600  Fitness= 0.00000

;INTRODUCIR VIDA Y MUERTE DE ASEGURADOS Y AGENTES
;CALCULAR EL CAMBIO PORCENTUAL TRIMESTRAL
;INCLUIR VALORES DE LA GRçFICA DE EXCEL LOS CUALES SE COMPARAN CON LOS DATOS GENERADOS EN EL MODELO DE NETLOGO Y SE CALCULA UN 
;ERROR CUADRADO MEDIO A FIN DE ENCONTRAR EL ESPACIO DE PARçMETROS QUE GENERAN EL MEJOR AJUSTE. PARA ESTE ESPACIO, DEJAMOS QUE 
;TAMBIƒN ESCOJAN VALORES EN LOS OTROS COMPONENTES DE LA PROBABILIDAD
to setup 
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
ca
set ciclo (list 0.960101521  0.533082252  0.366929218  -0.726751026  1.052890226  0.516676442  0.415284733  -0.773337144  0.949347812  
0.527303059  0.369593516  -0.712358832  0.981536892  0.46032144  0.343784331  -0.748676027  0.953463221  0.483065652  0.365884486  
-0.747152483  1.301725301  0.478916506  0.369961355  -0.716359969  0.94713148  0.47077051  0.362207227  -0.714674017  0.904789086  
0.440056896  0.323189204  -0.728513789  0.929900035  0.463127027  0.358352728  -0.738912471  0.955480423  0.4615441  0.362399212  
-0.71999063  0.928186516  0.485412488  0.357912706  -0.721878368  0.948814974)
set ecm 0
  district

ask n-of (n_agentes * (count patches with [ocupado = 1])) patches with [ocupado = 1] 
                                                          [sprout-agentes 1 
                                                           [
                                                           set shape "person" set color red 
                                                           set cartera count links set capacidad random-float 0.1
                                                           ]
                                                          ]


ask agentes [set shape "person" set color red]               
set siniestralidad 0.3
set as0 count asegurados with [seguro = 1]
do-plots
end

to recolor-turtle
  if seguro = 1
  [set color green]
set trim 0
set as0t count asegurados with [seguro = 1]
set ticks1 0
end

to go
ifelse Real = "On" and ticks = periods [stop]
[do-plots
update-plots
tick-advance 1
ask agentes [ifelse cartera >= 1 [move-to one-of neighbors in-radius 1]
                                 [move-to one-of patches]
             ]
ask asegurados [
                set exper 0
                if obligado = 1 [set seguro 1 set color green]
                if seguro = 1 [set seg_edad seg_edad + 1]
                set total (sum [seguro] of asegurados-on neighbors)
                set perdida 0
                if seguro = 0 
                [
                let aseg one-of agentes in-radius 1
                ifelse aseg != nobody [ask aseg [
                                                 set capacidad capacidad + (cartera / (ticks))] 
             
                                       set pi_t (1 / (1 + (exp (-1 * (intercept + (social * (total / 8)) + (capac * [(capacidad)] of aseg) + (expe * exper) + (riq * ln(riqueza))))))) ]
                                      [set pi_t (1 / ((1 + exp (-1 * (intercept + (social * (total / 8)) ) + (expe * exper) + (riq * ln(riqueza)))))) ]
                ifelse pi_t >= .50 [set seguro 1 set color green if aseg != nobody [ask aseg [set cartera cartera + 1]]] 
                                   [set seguro 0 set color blue]
                ]
                
                
                let sini random-float 1  
                let sini1 random-float 1
                ifelse sini <= sini1 [set exper 1
                                               set perdida random-float 1.1 
                                               set riqueza riqueza - (perdida * riqueza) + (seguro * perdida * riqueza) - (seguro * siniestralidad * riqueza) + ([dist] of patch-here * 10)
                                     ]
                                     [
                                      set exper 0
                                      set riqueza riqueza -  (seguro * siniestralidad * riqueza) + ([dist] of patch-here * 10)
                                     ]
                                      if riqueza <= 0 [set died died + 1
                                                       hatch 1 
                                                        [
                                                          set shape "square" 
                                                          set color blue 
                                                          set perdida 0 
                                                          set exper 0
                                                          set seguro 0
                                                          set riqueza random-float 10
                                                        ] 
                                                       die
                                                      ]
                                                                                                    
               
               if seguro = 1 and seg_edad = 12 [set seguro 0 set seg_edad 0 ask my-links [die] set color blue]
               
;; all the blue links will die
               ]               
   
 ask agentes [
              ifelse cartera >= 1 [set aedad aedad + 1 if aedad = aedadlim [hatch 1 [set cartera 0 set capacidad random-float 0.1 set cartera 0 set aedad 0] die] ]
                             
                             [hatch 1 [set cartera 0 set capacidad random-float 0.1 set cartera 0 set aedad 0] die]
             ]
show  asegurados with [color = green ] 
set siniestralidad (sum [exper] of asegurados) / (count asegurados)
set as1 count asegurados with [seguro = 1]
set das (as1 - as0) / as0
set as0 as1
set trim trim + 1 
if trim = 3 [set ticks1 ticks1 + 1             
             set as1t count asegurados with [seguro = 1]
             set dast (as1t - as0t) / as0t
             set as0t as1t
             set-current-plot "Cambio porcentual trimestral" 
             plot dast
             if Real = "On" [set ecm ((item ( ticks1 - 1) ciclo - dast) ^ 2) + ecm] 
             set trim 0
            ]
]



end
   
to district     
set d 1
set u max-pycor 
repeat 8 
[
 set y u
 set x min-pxcor
 set z x
 set w y
 repeat 4
 [ 
  set x z
  repeat 10 
   [
     if x <= z + 10
     [
      set y w
      repeat 10 
        [
          if y >= w - 10
             [ask patch x y [set dist d]]
           set y y - 1
        ]
      set x x + 1
     ]
    ]
  set z z + 10
  set d d + 1
 ] 
 set u u - 10 
]
let ad min [dist] of patches 
let ae max [dist] of patches  
ask patches [set pcolor scale-color (orange) (dist) ae ad]
;Porcentajes de poblaci?n por estado de acuerdo a conteo de 2005
set estados (list 7.384037418 19.66011272 3.969484977 5.124857061 17.12598353 4.053787914 29.88881498 21.22662441 55.15352449
                  10.17526283 34.18709948 21.11634952 16.60646334 45.80413009 94.56498829 27.11251347 11.07439238 6.760803862
                  28.99698226 23.69107547 36.01571111 11.39038039 8.260042694 16.11107688 17.24668335 16.59064836 13.94935368
                  20.36726288 7.290194398 47.62685316 12.18574049 9.288764088)
; Porcentajes de asegurados por estado
set aseg_ed (list 0.045642348 0.046422742 0.052938185 0.054589934 0.047416834 0.037986796 0.022713892 0.042563185 0.199513732 
                  0.039050262 0.031690614 0.025949254 0.028733577 0.041890399 0.033906707 0.028038373 0.035059956 0.042574557
                  0.059946173 0.024052713 0.026493085 0.037456707 0.050064576 0.037219814 0.046694603 0.052157763 0.039833771
                  0.053890956 0.025672772 0.048048761 0.032230385 0.030349816)

set d 1
repeat 32 [ 
  let yy item (d - 1) estados 
ask n-of yy patches with [dist = d] [
  
                                  
                               sprout-asegurados 1 
                               [set shape "square" 
                                set color blue 
                                set perdida 0 
                                set exper 0
                                set seguro 0
                                set riqueza random-float 10
                                set asedad 0
                                set asedadlim random 70
                               ] 
                               set ocupado 1
                                                                
                                ]  
let xx item (d - 1) aseg_ed 
ask n-of (xx * count asegurados-on patches with [dist = d]) asegurados-on patches with [dist = d] [ set seguro 1         
  recolor-turtle]
set d d + 1
]
ask n-of (0.043 * count asegurados) asegurados [set obligado 1]
end





to do-plots

 set-current-plot "asegurados" 
 set-current-plot-pen "asegurados" 
 plot (count asegurados with [seguro = 1 and obligado = 0]) / (count asegurados)
 set-current-plot-pen "obligado" 
 plot (count asegurados with [obligado = 1]) / (count asegurados)
 
 set-current-plot "Cambio porcentual" 
 plot das
 
 end

to update-plots
 set-current-plot "Probabilidad"
 set-current-plot-pen "percibida-seguro"
 if count asegurados with [seguro = 1] > 0 [plot mean [pi_t] of asegurados with [seguro = 1]]
 set-current-plot-pen "percibida-noseguro"
 plot mean [pi_t] of asegurados with [seguro = 0]
 set-current-plot-pen "percibida"
 plot mean [pi_t] of asegurados 
 set-current-plot-pen "real" 
 plot siniestralidad
 
end 

 
 

@#$#@#$#@
GRAPHICS-WINDOW
226
10
451
471
-1
-1
5.38
1
10
1
1
1
0
0
0
1
-20
19
-39
40
0
0
1
ticks

BUTTON
90
18
156
51
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
27
18
90
51
go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL

PLOT
456
10
977
243
asegurados
time
asegurados
0.0
100.0
0.0
1.0
true
false
PENS
"asegurados" 1.0 0 -10899396 true
"obligado" 1.0 0 -16777216 true

PLOT
457
246
978
448
Probabilidad
time
probabilidad
0.0
10.0
0.0
1.0
true
true
PENS
"percibida" 1.0 0 -16777216 true
"real" 1.0 0 -13791810 true
"percibida-seguro" 1.0 0 -10899396 true
"percibida-noseguro" 1.0 0 -2674135 true

INPUTBOX
25
142
104
202
intercept
-4.16
1
0
Number

INPUTBOX
25
206
102
266
capac
-0.45
1
0
Number

PLOT
458
448
979
694
Cambio porcentual
NIL
NIL
0.0
10.0
0.0
1.0
true
false
PENS
"default" 1.0 0 -16777216 true

PLOT
459
696
978
943
Cambio porcentual trimestral
NIL
NIL
0.0
10.0
0.0
1.0
true
false
PENS
"default" 1.0 0 -16777216 true

INPUTBOX
106
81
183
141
periods
135
1
0
Number

MONITOR
30
390
101
435
ECM
ecm
2
1
11

INPUTBOX
105
142
181
202
social
0.33
1
0
Number

INPUTBOX
103
207
181
267
expe
0.21
1
0
Number

INPUTBOX
60
267
135
327
riq
1.63
1
0
Number

INPUTBOX
26
81
105
141
n_agentes
0.02
1
0
Number

TEXTBOX
29
66
179
84
Ref=0.02 
11
0.0
1

CHOOSER
47
332
139
377
Real
Real
"On" "Off"
0

@#$#@#$#@
## WHAT IS IT?

Este modelo intenta simular la dinï¿½mica de un  mercado de seguros. Los posibles compradores deciden la compra o no de un seguro con base en la probabilidad de ocurrencia de un siniestro (evento sï¿½bito, fortuito y violento que afecta el bienestar de los individuos) , ademï¿½s se aï¿½ade la influencia, tanto de los agentes de seguros, como de los vecinos del posible comprador. 

## HOW IT WORKS

Los consumidores son representados por patches y los agentes de seguros por tortugas. Cada consumidor se ubica en distritos que representan las condiciones socioeconï¿½micas en las que se desenvuelve. Periï¿½dicamente se encuentran sujetos a la ocurrencia de un siniestro; a su vez, son visitados por agentes de seguros que ofrecen alguno de los productos con los que las aseguradoras cuentan. Este trabajo se ubica en el contexto de una teorï¿½a alternativa de demanda de seguros, la cual, entre otras cosas, propone que la gente no conoce con certeza la probabilidad de ocurrencia de un siniestro y subraya la influencia e interacciï¿½n de los componentes del mercado. 

## HOW TO USE IT

El usuario puede modificar el parï¿½metro de siniestralidad y  el del nï¿½mero de asegurados iniciales. 

## THINGS TO NOTICE

El modelo logra simular adecuadamente el mercado de seguros, ello se debe a que la teorï¿½a bajo la que se sustenta es muy compatible con lo que se observa en los mercados de seguros en la realidad; en donde las variables tï¿½picas que afectan la demanda de seguros, como lo es el ingreso no explican del todo la compra de seguros. Esto se puede apreciar al observar el nï¿½mero de paï¿½ses en donde la compra de seguros se impone de manera obligatoria y en donde se asume que sin este tipo de medidas la gente no compra seguros de manera masiva, tal como lo asumirï¿½a el modelo neoclï¿½sico de seguros. 

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
