(*^

::[paletteColors = 128; automaticGrouping; currentKernel; 
	fontset = title, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeTitle, center, M7, bold, L1, e8,  24, "Times"; ;
	fontset = subtitle, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeTitle, center, M7, bold, L1, e6,  18, "Times"; ;
	fontset = subsubtitle, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeTitle, center, M7, italic, L1, e6,  14, "Times"; ;
	fontset = section, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeSection, grayBox, M22, bold, L1, a20,  18, "Times"; ;
	fontset = subsection, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeSection, blackBox, M19, bold, L1, a15,  14, "Times"; ;
	fontset = subsubsection, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeSection, whiteBox, M18, bold, L1, a12,  12, "Times"; ;
	fontset = text, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12;
	fontset = smalltext, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  10, "Times"; ;
	fontset = input, noPageBreakBelow, nowordwrap, preserveAspect, groupLikeInput, M42, N23, bold, L1,  12, "Courier"; ;
	fontset = output, output, inactive, noPageBreakBelow, nowordwrap, preserveAspect, groupLikeOutput, M42, N23, L-5,  12, "Courier"; ;
	fontset = message, inactive, noPageBreakBelow, nowordwrap, preserveAspect, groupLikeOutput, M42, N23, L1,  12, "Courier"; ;
	fontset = print, inactive, noPageBreakBelow, nowordwrap, preserveAspect, groupLikeOutput, M42, N23, L1,  12, "Courier"; ;
	fontset = info, inactive, noPageBreakBelow, nowordwrap, preserveAspect, groupLikeOutput, M42, N23, L1,  12, "Courier"; ;
	fontset = postscript, PostScript, formatAsPostScript, output, inactive, noPageBreakBelow, nowordwrap, preserveAspect, groupLikeGraphics, M7, l34, w282, h287, L1,  12, "Courier"; ;
	fontset = name, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, italic, L1,  10, "Times"; ;
	fontset = header, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12;
	fontset = Left Header, nohscroll, cellOutline,  12;
	fontset = footer, inactive, nohscroll, noKeepOnOnePage, preserveAspect, center, M7, L1,  12;
	fontset = Left Footer, cellOutline, blackBox,  12;
	fontset = help, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  10, "Times"; ;
	fontset = clipboard, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12;
	fontset = completions, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12, "Courier"; ;
	fontset = special1, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12;
	fontset = special2, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12;
	fontset = special3, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12;
	fontset = special4, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12;
	fontset = special5, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, L1,  12;]
:[font = title; inactive; preserveAspect; startGroup; ]
Converting TI Notations
:[font = input; preserveAspect; ]
<<Thomsen.m
:[font = section; inactive; preserveAspect; startGroup; ]
Cij to Thomsen Examples
:[font = text; inactive; preserveAspect; ]
We use the ``cracks'' expression below as an example to illustrate use of the functions  in the Thomsen.m package.
:[font = input; preserveAspect; ]
cracks = C11 C33 - C13^2 - 2 C66 (C13 + C33);
:[font = text; inactive; preserveAspect; ]
Note from Ilya Tsvankin, CWP:
This equation represents the relation between Cij's for TI media formed by a system of thin parallel horizontal cracks in a purely isotropic matrix. It comes from the fact that in this case there are only four independent parameters instead of five for general TI media. You can find a more detailed discussion in the paper by Schoenberg & Sayers in Geophysics (the only difference is that their cracks are vertical).
:[font = subsection; inactive; preserveAspect; startGroup; ]
General TI
:[font = text; inactive; preserveAspect; ]
Convert the expression to a modified set of Thomsen parameters. and store the result in the variable ``generalCracks.''  The parameters chosen for this canonical representation are cp (aka VP0), Tsvankin's quantity f, Thomsen's gamma, and epsilon, a nd Cohen's deltaTilde.  The package contains functions to eliminate the non-standard quantities f and deltaTilde (see below).
:[font = input; preserveAspect; startGroup; ]
generalCracks =
ConvertCij[cracks]
:[font = output; output; inactive; preserveAspect; endGroup; ]
cp^4*(-deltaTilde^2 + 2*epsilon - 2*deltaTilde*f - 
    4*deltaTilde*gamma - 8*f*gamma + 
    4*deltaTilde*f*gamma + 8*f^2*gamma)*rho^2
;[o]
  4             2
cp  (-deltaTilde  + 2 epsilon - 2 deltaTilde f - 
 
    4 deltaTilde gamma - 8 f gamma + 
 
                              2           2
    4 deltaTilde f gamma + 8 f  gamma) rho
:[font = text; inactive; preserveAspect; ]
Eliminate Tsvankin's f in favor of the speed cs (aka VS0)
:[font = input; preserveAspect; startGroup; ]
EliminateF[%]
:[font = output; output; inactive; preserveAspect; endGroup; ]
cp^4*(-2*deltaTilde + (2*cs^2*deltaTilde)/cp^2 - 
    deltaTilde^2 + 2*epsilon - (8*cs^2*gamma)/cp^2 + 
    (8*cs^4*gamma)/cp^4 - 
    (4*cs^2*deltaTilde*gamma)/cp^2)*rho^2
;[o]
                         2
  4                  2 cs  deltaTilde             2
cp  (-2 deltaTilde + ---------------- - deltaTilde  + 
                             2
                           cp
 
                    2             4
                8 cs  gamma   8 cs  gamma
    2 epsilon - ----------- + ----------- - 
                      2             4
                    cp            cp
 
        2
    4 cs  deltaTilde gamma     2
    ----------------------) rho
               2
             cp
:[font = text; inactive; preserveAspect; ]
Change back to the canonical representation by eliminating cs in favor of Tsvankin's f
:[font = input; preserveAspect; startGroup; ]
EliminateCs[%]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; ]
cp^4*(-deltaTilde^2 + 2*epsilon - 2*deltaTilde*f - 
    4*deltaTilde*gamma - 8*f*gamma + 
    4*deltaTilde*f*gamma + 8*f^2*gamma)*rho^2
;[o]
  4             2
cp  (-deltaTilde  + 2 epsilon - 2 deltaTilde f - 
 
    4 deltaTilde gamma - 8 f gamma + 
 
                              2           2
    4 deltaTilde f gamma + 8 f  gamma) rho
:[font = subsection; inactive; preserveAspect; startGroup; ]
Weak TI
:[font = text; inactive; preserveAspect; ]
Do the canonical conversion in the limit of weak TI.  In this limit, delta and deltaTilde are equal, so that the representation is presented in the variables cp, f, gamma, delta, and epsilon.
:[font = input; preserveAspect; startGroup; ]
weakCracks =
ConvertWeak[cracks]
:[font = output; output; inactive; preserveAspect; endGroup; ]
2*cp^4*(epsilon - delta*f - 4*f*gamma + 4*f^2*gamma)*
  rho^2
;[o]
    4                                     2
2 cp  (epsilon - delta f - 4 f gamma + 4 f  gamma) 
 
     2
  rho
:[font = text; inactive; preserveAspect; ]
Use Mathematica  to solve for delta
;[s]
3:0,0;4,1;15,2;35,-1;
3:1,11,8,Times,0,12,0,0,0;1,10,8,Times,2,12,0,0,0;1,11,8,Times,0,12,0,0,0;
:[font = input; preserveAspect; startGroup; ]
delta /. Flatten@Solve[weakCracks == 0, delta]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; ]
epsilon/f - 4*gamma + 4*f*gamma
;[o]
epsilon
------- - 4 gamma + 4 f gamma
   f
:[font = text; inactive; preserveAspect; ]
Using a typical value cp/cs = 2 or f  = 3/4 to get a feel for the result
:[font = input; preserveAspect; startGroup; ]
% /. f -> 3/4
:[font = output; output; inactive; preserveAspect; endGroup; ]
(4*epsilon)/3 - gamma
;[o]
4 epsilon
--------- - gamma
    3
:[font = text; inactive; preserveAspect; ]
Introduce cs in favor of f ...
:[font = input; preserveAspect; startGroup; ]
EliminateF[weakCracks]
:[font = output; output; inactive; preserveAspect; endGroup; ]
2*(-(cp^4*delta) + cp^2*cs^2*delta + cp^4*epsilon - 
    4*cp^2*cs^2*gamma + 4*cs^4*gamma)*rho^2
;[o]
       4            2   2           4
2 (-(cp  delta) + cp  cs  delta + cp  epsilon - 
 
        2   2             4           2
    4 cp  cs  gamma + 4 cs  gamma) rho
:[font = text; inactive; preserveAspect; ]
... and solve for delta in the new variables, and ...
:[font = input; preserveAspect; startGroup; ]
delta /. Flatten@Solve[% == 0, delta] //Simplify
:[font = output; output; inactive; preserveAspect; endGroup; ]
-((cp^4*epsilon - 4*cp^2*cs^2*gamma + 4*cs^4*gamma)/
    (-cp^4 + cp^2*cs^2))
;[o]
    4               2   2             4
  cp  epsilon - 4 cp  cs  gamma + 4 cs  gamma
-(-------------------------------------------)
                   4     2   2
                -cp  + cp  cs
:[font = text; inactive; preserveAspect; ]
... use a standard Mathematica function to get the limit in our typical case
;[s]
3:0,0;19,1;30,2;77,-1;
3:1,11,8,Times,0,12,0,0,0;1,10,8,Times,2,12,0,0,0;1,11,8,Times,0,12,0,0,0;
:[font = input; preserveAspect; startGroup; ]
Limit[%, cp -> 2 cs]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; ]
(4*epsilon)/3 - gamma
;[o]
4 epsilon
--------- - gamma
    3
:[font = input; preserveAspect; startGroup; ]
ConvertWeak[1 - 3 epsilon + deltaTilde + 2 delta]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; ]
1 + 3*delta - 3*epsilon
;[o]
1 + 3 delta - 3 epsilon
:[font = subsection; inactive; preserveAspect; startGroup; ]
Expressions without deltaTilde and/or f
:[font = text; inactive; preserveAspect; ]
And here is the promised function to eliminate Cohen's deltaTilde.  Note the square root which is the price to be paid (unless we make the weak TI assumption).
:[font = input; preserveAspect; startGroup; ]
EliminateDeltaTilde[generalCracks]
:[font = output; output; inactive; preserveAspect; endGroup; ]
2*cp^4*(epsilon - delta*f - 2*f*gamma + 
    2*f^2*gamma - 2*f*((2*delta + f)/f)^(1/2)*
     gamma + 2*f^2*((2*delta + f)/f)^(1/2)*gamma)*
  rho^2
;[o]
    4                                     2
2 cp  (epsilon - delta f - 2 f gamma + 2 f  gamma - 
 
             2 delta + f
    2 f Sqrt[-----------] gamma + 
                  f
 
       2      2 delta + f            2
    2 f  Sqrt[-----------] gamma) rho
                   f
:[font = input; preserveAspect; startGroup; ]
EliminateF[%]
:[font = output; output; inactive; preserveAspect; endGroup; ]
2*(-(cp^4*delta) + cp^2*cs^2*delta + cp^4*epsilon - 
    2*cp^2*cs^2*gamma + 2*cs^4*gamma - 
    2*cp^2*cs^2*((cp^2 - cs^2 + 2*cp^2*delta)/
        (cp^2 - cs^2))^(1/2)*gamma + 
    2*cs^4*((cp^2 - cs^2 + 2*cp^2*delta)/
        (cp^2 - cs^2))^(1/2)*gamma)*rho^2
;[o]
       4            2   2           4
2 (-(cp  delta) + cp  cs  delta + cp  epsilon - 
 
        2   2             4
    2 cp  cs  gamma + 2 cs  gamma - 
 
                     2     2       2
        2   2      cp  - cs  + 2 cp  delta
    2 cp  cs  Sqrt[-----------------------] gamma + 
                            2     2
                          cp  - cs
 
                 2     2       2
        4      cp  - cs  + 2 cp  delta            2
    2 cs  Sqrt[-----------------------] gamma) rho
                        2     2
                      cp  - cs
:[font = input; preserveAspect; startGroup; ]
ConvertWeak[%]
:[font = output; output; inactive; preserveAspect; endGroup; ]
2*cp^4*(epsilon - delta*f - 4*f*gamma + 4*f^2*gamma)*
  rho^2
;[o]
    4                                     2
2 cp  (epsilon - delta f - 4 f gamma + 4 f  gamma) 
 
     2
  rho
:[font = input; preserveAspect; startGroup; ]
% /. f -> 3/4
:[font = output; output; inactive; preserveAspect; endGroup; ]
2*cp^4*((-3*delta)/4 + epsilon - (3*gamma)/4)*rho^2
;[o]
    4  -3 delta             3 gamma     2
2 cp  (-------- + epsilon - -------) rho
          4                    4
:[font = input; preserveAspect; startGroup; ]
Solve[% == 0, delta]//Flatten
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup; ]
{delta -> (4*epsilon - 3*gamma)/3}
;[o]
          4 epsilon - 3 gamma
{delta -> -------------------}
                   3
:[font = section; inactive; preserveAspect; startGroup; ]
Love Notation Example 
:[font = text; inactive; preserveAspect; ]
You can convert Love notation expressions to Cij notation and then proceed as above.
:[font = input; preserveAspect; startGroup; ]
EliminateLove[F + L]
:[font = output; output; inactive; preserveAspect; endGroup; ]
C13 + C44
;[o]
C13 + C44
:[font = input; preserveAspect; startGroup; ]
ConvertCij[%]
:[font = output; output; inactive; preserveAspect; endGroup; ]
cp^2*(deltaTilde + f)*rho
;[o]
  2
cp  (deltaTilde + f) rho
:[font = input; preserveAspect; startGroup; ]
EliminateDeltaTilde[%]
:[font = output; output; inactive; preserveAspect; endGroup; ]
cp^2*f*((2*delta + f)/f)^(1/2)*rho
;[o]
  2        2 delta + f
cp  f Sqrt[-----------] rho
                f
:[font = input; preserveAspect; startGroup; ]
EliminateF[%]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; ]
(cp^2 - cs^2)*((cp^2 - cs^2 + 2*cp^2*delta)/
     (cp^2 - cs^2))^(1/2)*rho
;[o]
                   2     2       2
   2     2       cp  - cs  + 2 cp  delta
(cp  - cs ) Sqrt[-----------------------] rho
                          2     2
                        cp  - cs
:[font = section; inactive; preserveAspect; startGroup; ]
Kuprazde Notation Example
:[font = text; inactive; preserveAspect; ]
I saw this notation in Kuprazde, page 21.  Ilya says no one uses  it, so you can probably ignore this section.  Again, you  can convert Kuprazde notation expressions to Cij notation and then proceed as above.
:[font = input; preserveAspect; startGroup; ]
EliminateKuprazde[C2]
:[font = output; output; inactive; preserveAspect; endGroup; ]
C11 - C66
;[o]
C11 - C66
:[font = input; preserveAspect; startGroup; ]
ConvertCij[%]
:[font = output; output; inactive; preserveAspect; endGroup; ]
cp^2*(2*epsilon + f - 2*gamma + 2*f*gamma)*rho
;[o]
  2
cp  (2 epsilon + f - 2 gamma + 2 f gamma) rho
:[font = input; preserveAspect; startGroup; ]
EliminateF[%]
:[font = output; output; inactive; preserveAspect; endGroup; ]
(cp^2 - cs^2 + 2*cp^2*epsilon - 2*cs^2*gamma)*rho
;[o]
   2     2       2               2
(cp  - cs  + 2 cp  epsilon - 2 cs  gamma) rho
:[font = input; preserveAspect; startGroup; ]
Collect[%, {cp, cs, rho}]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; ]
cp^2*(1 + 2*epsilon)*rho + cs^2*(-1 - 2*gamma)*rho
;[o]
  2                         2
cp  (1 + 2 epsilon) rho + cs  (-1 - 2 gamma) rho
:[font = section; inactive; preserveAspect; startGroup; ]
Hooke Notation Example
:[font = input; preserveAspect; startGroup; ]
EliminateHooke[C3322 + 2 C3223]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; ]
C11
;[o]
C11
:[font = section; inactive; preserveAspect; startGroup; ]
Eta Example
:[font = input; preserveAspect; startGroup; ]
EliminateEpsilon[epsilon]
:[font = output; output; inactive; preserveAspect; endGroup; ]
delta + eta + 2*delta*eta
;[o]
delta + eta + 2 delta eta
:[font = input; preserveAspect; startGroup; ]
EliminateEta[%]
:[font = output; output; inactive; preserveAspect; endGroup; ]
epsilon
;[o]
epsilon
:[font = input; preserveAspect; startGroup; ]
EliminateEpsilon[(epsilon-delta)/(1 + 2 delta)]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; ]
eta
;[o]
eta
:[font = section; active; preserveAspect; startGroup; ]
Using Trig Functions
:[font = input; preserveAspect; ]
(* Ilya's phase velocity computation *)
:[font = input; preserveAspect; startGroup; ]
expr = ((C11+C44) Sin[theta]^2 + (C33 + C44) Cos[theta]^2 +
	Sqrt[((C11 - C44) Sin[theta]^2 -
		(C33 - C44)Cos[theta]^2)^2 +
	  	4(C13 + C44)^2 Sin[theta]^2 Cos[theta]^2
	])/(2 C33);
exprThom = EliminateDeltaTilde@ConvertCij[expr]
:[font = output; output; inactive; preserveAspect; endGroup; ]
(cp^2*(2 - f)*rho*Cos[theta]^2 + 
    cp^2*(2 + 2*epsilon - f)*rho*Sin[theta]^2 + 
    (cp^4*rho^2*(epsilon - epsilon*Cos[2*theta] - 
           f*Cos[2*theta])^2 + 
       4*cp^4*f*(2*delta + f)*rho^2*Cos[theta]^2*
        Sin[theta]^2)^(1/2))/(2*cp^2*rho)
;[o]
   2                       2
(cp  (2 - f) rho Cos[theta]  + 
 
      2                                   2
    cp  (2 + 2 epsilon - f) rho Sin[theta]  + 
 
           4    2
    Sqrt[cp  rho  (epsilon - epsilon Cos[2 theta] - 
 
                         2
          f Cos[2 theta])  + 
 
          4                    2           2           2
      4 cp  f (2 delta + f) rho  Cos[theta]  Sin[theta] ]
 
             2
    ) / (2 cp  rho)
:[font = input; preserveAspect; ]
(* We can see the common factor of rho cp^2 even if
		 Mathematica can not, so force it out *)
;[s]
3:0,0;55,1;66,2;94,-1;
3:1,10,8,Courier,1,12,0,0,0;1,10,8,Courier,2,12,0,0,0;1,10,8,Courier,1,12,0,0,0;
:[font = input; preserveAspect; startGroup; ]
ratiosq = Limit[exprThom, rho -> 1/cp^2] 
:[font = output; output; inactive; preserveAspect; endGroup; ]
((2 - f)*Cos[theta]^2 + 
    (2 + 2*epsilon - f)*Sin[theta]^2 + 
    ((epsilon - epsilon*Cos[2*theta] - f*Cos[2*theta])^
        2 + 4*f*(2*delta + f)*Cos[theta]^2*Sin[theta]^2)^
     (1/2))/2
;[o]
                   2                                 2
((2 - f) Cos[theta]  + (2 + 2 epsilon - f) Sin[theta]  + 
 
    Sqrt[(epsilon - epsilon Cos[2 theta] - 
 
                        2
         f Cos[2 theta])  + 
 
                                  2           2
      4 f (2 delta + f) Cos[theta]  Sin[theta] ]) / 2
:[font = input; preserveAspect; startGroup; ]
ratiosq1 = SineForm[ratiosq]
:[font = output; output; inactive; preserveAspect; endGroup; ]
1 - f/2 + epsilon*Sin[theta]^2 + 
  (f^2 + 8*delta*f*Sin[theta]^2 - 
      4*epsilon*f*Sin[theta]^2 + 
      4*epsilon^2*Sin[theta]^4 - 
      8*delta*f*Sin[theta]^4 + 8*epsilon*f*Sin[theta]^4)^
    (1/2)/2
;[o]
    f                     2
1 - - + epsilon Sin[theta]  + 
    2
 
        2                       2
  Sqrt[f  + 8 delta f Sin[theta]  - 
 
                           2            2           4
     4 epsilon f Sin[theta]  + 4 epsilon  Sin[theta]  - 
 
                         4                         4
     8 delta f Sin[theta]  + 8 epsilon f Sin[theta] ] / 2
:[font = input; preserveAspect; startGroup; ]
a = 1 - f/2 + Sin[theta]^2 epsilon;
b = (ratiosq1 - a)^2//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; ]
(f^2 + 8*delta*f*Sin[theta]^2 - 
    4*epsilon*f*Sin[theta]^2 + 
    4*epsilon^2*Sin[theta]^4 - 8*delta*f*Sin[theta]^4 + 
    8*epsilon*f*Sin[theta]^4)/4
;[o]
  2                       2                         2
(f  + 8 delta f Sin[theta]  - 4 epsilon f Sin[theta]  + 
 
             2           4                       4
    4 epsilon  Sin[theta]  - 8 delta f Sin[theta]  + 
 
                          4
    8 epsilon f Sin[theta] ) / 4
:[font = input; preserveAspect; startGroup; ]
ratiosq1 - a - Sqrt[b]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; ]
0
;[o]
0
:[font = input; preserveAspect; startGroup; ]
b1 = Collect[b, {epsilon, delta, f}]
:[font = output; output; inactive; preserveAspect; endGroup; ]
f^2/4 + epsilon^2*Sin[theta]^4 + 
  delta*f*(2*Sin[theta]^2 - 2*Sin[theta]^4) + 
  epsilon*f*(-Sin[theta]^2 + 2*Sin[theta]^4)
;[o]
 2
f           2           4
-- + epsilon  Sin[theta]  + 
4
 
                       2               4
  delta f (2 Sin[theta]  - 2 Sin[theta] ) + 
 
                        2               4
  epsilon f (-Sin[theta]  + 2 Sin[theta] )
:[font = input; preserveAspect; startGroup; ]
weakratiosq = ConvertWeak[ratiosq1]//SineForm
:[font = output; output; inactive; preserveAspect; endGroup; ]
1 + 2*delta*Sin[theta]^2 - 2*delta*Sin[theta]^4 + 
  2*epsilon*Sin[theta]^4
;[o]
                      2                     4
1 + 2 delta Sin[theta]  - 2 delta Sin[theta]  + 
 
                      4
  2 epsilon Sin[theta]
:[font = input; preserveAspect; startGroup; ]
Collect[weakratiosq, {epsilon, delta}]
:[font = output; output; inactive; preserveAspect; endGroup; ]
1 + 2*epsilon*Sin[theta]^4 + 
  delta*(2*Sin[theta]^2 - 2*Sin[theta]^4)
;[o]
                        4
1 + 2 epsilon Sin[theta]  + 
 
                     2               4
  delta (2 Sin[theta]  - 2 Sin[theta] )
:[font = input; preserveAspect; startGroup; ]
%//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup; ]
1 + 2*epsilon*Sin[theta]^4 + (delta*Sin[2*theta]^2)/2
;[o]
                                              2
                        4   delta Sin[2 theta]
1 + 2 epsilon Sin[theta]  + -------------------
                                     2
^*)