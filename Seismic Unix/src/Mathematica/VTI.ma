(*^

::[	Information =

	"This is a Mathematica Notebook file.  It contains ASCII text, and can be
	transferred by email, ftp, or other text-file transfer utility.  It should
	be read or edited using a copy of Mathematica or MathReader.  If you 
	received this as email, use your mail application or copy/paste to save 
	everything from the line containing (*^ down to the line containing ^*)
	into a plain text file.  On some systems you may have to give the file a 
	name ending with ".ma" to allow Mathematica to recognize it as a Notebook.
	The line below identifies what version of Mathematica created this file,
	but it can be opened using any other version as well.";

	FrontEndVersion = "X Window System Mathematica Notebook Front End Version 2.2";

	X11StandardFontEncoding; 
	
	fontset = title, inactive, noPageBreakBelow, noPageBreakInGroup, nohscroll, preserveAspect, groupLikeTitle, center, M7, bold, e8,  36, fontName, "times";
	fontset = subtitle, inactive, noPageBreakBelow, noPageBreakInGroup, nohscroll, preserveAspect, groupLikeTitle, center, M7, bold, e6,  24, fontName, "times";
	fontset = subsubtitle, inactive, noPageBreakBelow, noPageBreakInGroup, nohscroll, preserveAspect, groupLikeTitle, center, M7, italic, e6,  18, fontName, "times";
	fontset = section, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeSection, grayBox, M31, bold, a20,  28, fontName, "times";
	fontset = subsection, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeSection, blackBox, M24, bold, a15,  24, fontName, "times";
	fontset = subsubsection, inactive, noPageBreakBelow, nohscroll, preserveAspect, groupLikeSection, whiteBox, M21, bold, a12,  18, fontName, "times";
	fontset = text, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  18, fontName, "times";
	fontset = smalltext, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  14, fontName, "times";
	fontset = input, noPageBreakInGroup, nowordwrap, preserveAspect, groupLikeInput, M42, N23, bold,  18, fontName, "courier";
	fontset = output, output, inactive, noPageBreakInGroup, nowordwrap, preserveAspect, groupLikeOutput, M42, N23, L-5,  18, fontName, "courier";
	fontset = message, inactive, noPageBreakInGroup, nowordwrap, preserveAspect, groupLikeOutput, M42, N23,  18, fontName, "courier";
	fontset = print, inactive, noPageBreakInGroup, nowordwrap, preserveAspect, groupLikeOutput, M42, N23,  18, fontName, "courier";
	fontset = info, inactive, noPageBreakInGroup, nowordwrap, preserveAspect, groupLikeOutput, M42, N23,  18, fontName, "courier";
	fontset = postscript, PostScript, formatAsPostScript, output, inactive, noPageBreakInGroup, nowordwrap, preserveAspect, groupLikeGraphics, M7, l34, w282, h287,  18, fontName, "courier";
	fontset = name, inactive, noPageBreakInGroup, nohscroll, preserveAspect, M7, italic, B65535,  14, fontName, "times";
	fontset = header, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7, italic,  10, fontName, "times";
	fontset = leftheader,  10, fontName, "times";
	fontset = footer, inactive, nohscroll, noKeepOnOnePage, preserveAspect, center, M7, italic,  10, fontName, "times";
	fontset = leftfooter,  10, fontName, "times";
	fontset = help, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  14, fontName, "times";
	fontset = clipboard, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  14, fontName, "times";
	fontset = completions, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  14, fontName, "courier";
	fontset = special1, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  10, fontName, "times";
	fontset = special2, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  10, fontName, "times";
	fontset = special3, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  10, fontName, "times";
	fontset = special4, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  10, fontName, "times";
	fontset = special5, inactive, nohscroll, noKeepOnOnePage, preserveAspect, M7,  10, fontName, "times";paletteColors = 128; showRuler; automaticGrouping; currentKernel; 
]
:[font = title; inactive; preserveAspect; startGroup]
VTI Package Examples
:[font = subtitle; inactive; preserveAspect; fontSize = 18]
Copyright, Jack K. Cohen, 1996, All Rights Reserved
;[s]
1:0,0;51,-1;
1:1,0,0 ,times,2,18,0,0,0;
:[font = input; initialization; preserveAspect]
*)
<<VTI.m
Off[General::spell, General::spell1]
(*
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
General Remarks
:[font = text; inactive; preserveAspect; rightWrapOffset = 522; endGroup]
The following variables are reserved and cannot be assigned to:

rho, VP0, VS0, gamma,  delta, deltaTilde, epsilon, eta,  f,  and Cij, where i and j are integers in the range [1,6],
A, C, F, L, N.

Notation:
VP0 = Sqrt[C33/rho]
VS0 = Sqrt[C55/rho]

The Thomsen parameters, epsilon, gamma, delta, along with VP0 and VS0 form the standard five variables.   For ease of expression, define  the following "canonical" variables:
VP0, epsilon, gamma,  and the two auxiliary quantities,
	f = 1 -(VS0/VP0)^2 ,
	deltaTilde = (C13 - 2 C55 - C33)/C33.

deltaTilde agrees with delta in the weak limit.  The function
	EliminateDeltaTilde
eliminates deltaTilde in favor of delta.  The relation between
deltaTilde and  delta is best understood by the relation
	f^2 + 2f delta = (f + deltaTilde)^2
	
The user has access to the  additional auxiliary variable:
	eta = (epsilon - delta)/(1 + 2 delta).
	
The definitions of the Thomsen parameters, leave the inversion for C13 ambiguous to within a plus or minus sign that arises from solving the necessary quadratic equation.  We have silently chosen the plus sign, which is by far the common case in real anisotropic media.
:[font = section; inactive; preserveAspect; rightWrapOffset = 542; startGroup]
Cij to Thomsen Parameters
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Some Test Expressions
:[font = input; initialization; preserveAspect; endGroup]
*)
trace = C11 + C22 + C33 + C44 + C55 + C66;
offdiag = C13 + C23 + C12;
(*
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
The Standard Voigt Parameters for VTI
:[font = text; inactive; preserveAspect]
VTI media require only 5 of the 21 hookean constants.  In Voigt notation these are C11, C13, C33, C55, and C66.  Notice that we use C55 instead of the more common C44 (which is equal to it).  This is done with a view towards more general media such as orthorhombic .   However, the user can enter C44 or other Cij in expressions:
:[font = input; preserveAspect; startGroup]
?VoigtVTI
:[font = print; inactive; preserveAspect; endGroup]
VoigtVTI[expression]

 Reduce expressions in Cij to the canonical VTI set,
   C11, C13, C33, C55, C66.  Note: C44 = C55. 
:[font = input; preserveAspect; startGroup]
VoigtVTI[C14+C24+C34+C44+C54+C64]
:[font = output; output; inactive; preserveAspect; endGroup]
C55
;[o]
C55
:[font = text; inactive; preserveAspect]
Remark:  Other commands in the package that take Voigt notation input do this conversion automatically.
:[font = input; preserveAspect; startGroup]
voigtTrace = VoigtVTI[trace]
:[font = output; output; inactive; preserveAspect; endGroup]
2*C11 + C33 + 2*C55 + C66
;[o]
2 C11 + C33 + 2 C55 + C66
:[font = input; preserveAspect; startGroup]
voigtOffDiag = VoigtVTI[offdiag]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
C11 + 2*C13 - 2*C66
;[o]
C11 + 2 C13 - 2 C66
:[font = subsection; inactive; preserveAspect; startGroup]
Canonical vs. Standard representation
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Converting to canonical representation
:[font = input; preserveAspect; startGroup]
?ConvertCanonical
:[font = print; inactive; preserveAspect; endGroup]
ConvertCanonical[expression]

 Convert expression from Voigt Cij notation to Thomsen
   notation in terms of the canonical variables, VP0,
   f, gamma, epsilon, and deltaTilde. 
:[font = input; initialization; preserveAspect; startGroup]
*)
canonicalTrace = ConvertCanonical[trace]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 4*epsilon - 3*f + 2*gamma - 
 
    2*f*gamma)*rho*VP0^2
;[o]
(6 + 4 epsilon - 3 f + 2 gamma - 
 
                      2
    2 f gamma) rho VP0
:[font = input; initialization; preserveAspect; startGroup]
*)
canonicalOffdiag = ConvertCanonical[offdiag]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
(-3 + 2*deltaTilde + 2*epsilon + 
 
    6*f - 4*gamma + 4*f*gamma)*rho*
 
  VP0^2
;[o]
(-3 + 2 deltaTilde + 2 epsilon + 
 
    6 f - 4 gamma + 4 f gamma) rho 
 
     2
  VP0
:[font = text; inactive; preserveAspect]
Let's do an internal check by converting back to Voigt notation:
:[font = input; preserveAspect; startGroup]
?ConvertToVoigt
:[font = print; inactive; preserveAspect; endGroup]
ConvertToVoigt[expression]

 Convert expression from Thomsen notation to Voigt Cij
   notation. 
:[font = input; preserveAspect; startGroup]
ConvertToVoigt[canonicalTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
2*C11 + C33 + 2*C55 + C66
;[o]
2 C11 + C33 + 2 C55 + C66
:[font = text; inactive; preserveAspect]
Check it:
:[font = input; preserveAspect; startGroup]
% - voigtTrace
:[font = output; output; inactive; preserveAspect; endGroup]
2*C11 + C33 + 2*C55 + C66 - voigtTrace
;[o]
2 C11 + C33 + 2 C55 + C66 - voigtTrace
:[font = input; preserveAspect; startGroup]
ConvertToVoigt[canonicalOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
C11 + 2*C13 - 2*C66
;[o]
C11 + 2 C13 - 2 C66
:[font = input; preserveAspect; startGroup]
% - voigtOffDiag
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
C11 + 2*C13 - 2*C66 - voigtOffDiag
;[o]
C11 + 2 C13 - 2 C66 - voigtOffDiag
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Converting to standard representation
:[font = input; preserveAspect; startGroup]
?ConvertStandard
:[font = print; inactive; preserveAspect; endGroup]
ConvertStandard[expression]

 Convert expression from Voigt Cij notation to Thomsen
   notation in terms of the standard variables, VP0,
   VS0, gamma, epsilon, and delta. 
:[font = input; initialization; preserveAspect; startGroup]
*)
standardTrace = ConvertStandard[trace]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(3*VP0^2 + 4*epsilon*VP0^2 + 
 
    3*VS0^2 + 2*gamma*VS0^2)
;[o]
          2                2
rho (3 VP0  + 4 epsilon VP0  + 
 
         2              2
    3 VS0  + 2 gamma VS0 )
:[font = input; preserveAspect; startGroup]
Collect[%, {rho, VP0, VS0}]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*((3 + 4*epsilon)*VP0^2 + (3 + 2*gamma)*VS0^2)
;[o]
                        2                    2
rho ((3 + 4 epsilon) VP0  + (3 + 2 gamma) VS0 )
:[font = text; inactive; preserveAspect]
Again check:
:[font = input; preserveAspect; startGroup]
ConvertToVoigt[standardTrace] - voigtTrace
:[font = output; output; inactive; preserveAspect; endGroup]
2*C11 + C33 + 2*C55 + C66 - voigtTrace
;[o]
2 C11 + C33 + 2 C55 + C66 - voigtTrace
:[font = input; initialization; preserveAspect; startGroup]
*)
standardOffdiag = ConvertStandard[offdiag]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
rho*VP0^2*(3 + 2*epsilon - 
 
    4*gamma - (6*VS0^2)/VP0^2 + 
 
    4*gamma*(1 - VS0^2/VP0^2) + 
 
    2*(1 - VS0^2/VP0^2)*
 
     (-1 + (1 + 
 
          (2*delta)/
 
           (1 - VS0^2/VP0^2))^(1/2))
 
    )
;[o]
       2
rho VP0  (3 + 2 epsilon - 4 gamma - 
 
         2                   2
    6 VS0                 VS0
    ------ + 4 gamma (1 - ----) + 
        2                    2
     VP0                  VP0
 
              2
           VS0
    2 (1 - ----) 
              2
           VP0
 
                    2 delta
     (-1 + Sqrt[1 + --------]))
                           2
                        VS0
                    1 - ----
                           2
                        VP0
:[font = text; inactive; preserveAspect]
Converting back turns into a wrestling match:
:[font = input; preserveAspect; startGroup]
back = ConvertToVoigt[standardOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
C11 - 2*C55 + 2*C33*((C13 + C55)^2/(C33 - C55)^2)^
 
    (1/2) - 2*C55*((C13 + C55)^2/(C33 - C55)^2)^
 
    (1/2) - 2*C66
;[o]
                                    2
                         (C13 + C55)
C11 - 2 C55 + 2 C33 Sqrt[------------] - 
                                    2
                         (C33 - C55)
 
                        2
             (C13 + C55)
  2 C55 Sqrt[------------] - 2 C66
                        2
             (C33 - C55)
:[font = text; inactive; preserveAspect]
OK, Mathematica won't extract square roots of apparent perfect squares because of the possible sign ambiguities.  PowerExpand forces the positive square roots, but one should
always check that you are getting what you want.  Here we are fine:
:[font = input; preserveAspect; startGroup]
PowerExpand[back]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
C11 + 2*C13 - 2*C66
;[o]
C11 + 2 C13 - 2 C66
:[font = input; preserveAspect; startGroup]
% - voigtOffDiag
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
C11 + 2*C13 - 2*C66 - voigtOffDiag
;[o]
C11 + 2 C13 - 2 C66 - voigtOffDiag
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
UseVS0, UseF
:[font = text; inactive; preserveAspect]
We can also eliminate just one of the non-standard variables.  For example,  if  we want to see the speed VS0 instead of f:
:[font = input; preserveAspect; startGroup]
?UseVS0
:[font = print; inactive; preserveAspect; endGroup]
UseVS0[expression]

 Replace f in expression by VP0 and VS0. 
:[font = input; preserveAspect; startGroup]
UseVS0[canonicalTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(3*VP0^2 + 4*epsilon*VP0^2 + 3*VS0^2 + 
 
    2*gamma*VS0^2)
;[o]
          2                2        2              2
rho (3 VP0  + 4 epsilon VP0  + 3 VS0  + 2 gamma VS0 )
:[font = text; inactive; preserveAspect]
Change back to the canonical representation by eliminating VS0 in favor of f:
:[font = input; preserveAspect; startGroup]
?UseF
:[font = print; inactive; preserveAspect; endGroup]
UseF[expression]

 Replace VS0^2 in expression by VP0^2 and f.  An odd
   power of VS0 will result in having a single VS0
   factor in the result. 
:[font = input; preserveAspect; startGroup]
UseF[%%]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 4*epsilon - 3*f + 2*gamma - 2*f*gamma)*rho*VP0^2
;[o]
                                                   2
(6 + 4 epsilon - 3 f + 2 gamma - 2 f gamma) rho VP0
:[font = text; inactive; preserveAspect]
Check we got the right thing:
:[font = input; preserveAspect; startGroup]
% - canonicalTrace
:[font = output; output; inactive; preserveAspect; endGroup]
0
;[o]
0
:[font = text; inactive; preserveAspect]
Notice that we don't fully eliminate VS0 if it appears to an odd power:
:[font = input; preserveAspect; startGroup]
UseF[VS0^5]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(1 - f)^2*VP0^4*VS0
;[o]
       2    4
(1 - f)  VP0  VS0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
UseDelta, UseDeltaTilde
:[font = text; inactive; preserveAspect]
Eliminate deltaTilde in favor of delta:
:[font = input; preserveAspect; startGroup]
?UseDelta
:[font = print; inactive; preserveAspect; endGroup]
UseDelta[expression]

 Replace deltaTilde in expression by delta and f. 
:[font = input; preserveAspect; startGroup]
UseDelta[canonicalOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
(-3 + 2*epsilon + 4*f + 
 
    2*f*((2*delta + f)/f)^(1/2) - 4*gamma + 4*f*gamma
 
    )*rho*VP0^2
;[o]
                                 2 delta + f
(-3 + 2 epsilon + 4 f + 2 f Sqrt[-----------] - 
                                      f
 
                                2
    4 gamma + 4 f gamma) rho VP0
:[font = text; inactive; preserveAspect]
And again change back to the canonical representation by eliminating delta in favor of deltaTilde:
:[font = input; preserveAspect; startGroup]
?UseDeltaTilde
:[font = print; inactive; preserveAspect; endGroup]
UseDeltaTilde[expression]

 Replace delta in expression by deltaTilde and f. 
:[font = input; preserveAspect; startGroup]
UseDeltaTilde[%%]
:[font = output; output; inactive; preserveAspect; endGroup]
(-3 + 2*deltaTilde + 2*epsilon + 6*f - 4*gamma + 
 
    4*f*gamma)*rho*VP0^2
;[o]
(-3 + 2 deltaTilde + 2 epsilon + 6 f - 4 gamma + 
 
                      2
    4 f gamma) rho VP0
:[font = text; inactive; preserveAspect]
Again check it:
:[font = input; preserveAspect; startGroup]
% - canonicalOffdiag
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
0
;[o]
0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
UseEta, UseEpsilon
:[font = text; inactive; preserveAspect]
In the study of the normal moveout function in a VTI medium, Akhalifah and Tsvankin introduced the anisotropic parameter eta which together with the moveout velocity at zero phase angle, allows the entire moveout function to be characterized
by these two parameters.  That is, the additional parameters that could logically appear (eg., f and delta) influence the  moveout function only slightly.
:[font = input; preserveAspect; startGroup]
?UseEta
:[font = print; inactive; preserveAspect; endGroup]
UseEta[expression]

 Replace epsilon in expression by eta and delta. 
:[font = input; preserveAspect; startGroup]
UseEta[2epsilon + 3delta]
:[font = output; output; inactive; preserveAspect; endGroup]
5*delta + 2*eta + 4*delta*eta
;[o]
5 delta + 2 eta + 4 delta eta
:[font = input; preserveAspect; startGroup]
Collect[%, eta]
:[font = output; output; inactive; preserveAspect; endGroup]
5*delta + (2 + 4*delta)*eta
;[o]
5 delta + (2 + 4 delta) eta
:[font = text; inactive; preserveAspect]
UseEpsilon does the reverse:
:[font = input; preserveAspect; startGroup]
?UseEpsilon
:[font = print; inactive; preserveAspect; endGroup]
UseEpsilon[expression]

 Replace eta in expression by delta and epsilon. 
:[font = input; preserveAspect; startGroup]
UseEpsilon[%%]
:[font = output; output; inactive; preserveAspect; endGroup]
3*delta + 2*epsilon
;[o]
3 delta + 2 epsilon
:[font = input; preserveAspect; startGroup]
UseEta[(epsilon-delta)/(1 + 2 delta)]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
eta
;[o]
eta
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Love Notation
:[font = text; inactive; preserveAspect]
You can convert Love notation expressions to Cij notation and then proceed as above.
:[font = input; preserveAspect; startGroup]
?LoveToVoigt
:[font = print; inactive; preserveAspect; endGroup]
LoveToVoigt[expression]

 Replace Love parameters in expression by Cij
   parameters. 
:[font = input; preserveAspect; startGroup]
LoveToVoigt[F + L]
:[font = output; output; inactive; preserveAspect; endGroup]
C13 + C55
;[o]
C13 + C55
:[font = input; preserveAspect; startGroup]
ConvertCanonical[%]
:[font = output; output; inactive; preserveAspect; endGroup]
(deltaTilde + f)*rho*VP0^2
;[o]
                        2
(deltaTilde + f) rho VP0
:[font = input; preserveAspect; startGroup]
UseDelta[%]
:[font = output; output; inactive; preserveAspect; endGroup]
f*((2*delta + f)/f)^(1/2)*rho*VP0^2
;[o]
       2 delta + f         2
f Sqrt[-----------] rho VP0
            f
:[font = input; preserveAspect; startGroup]
UseVS0[%]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(VP0^2 - VS0^2)*((VP0^2 + 2*delta*VP0^2 - VS0^2)/
 
     (VP0^2 - VS0^2))^(1/2)
;[o]
                          2              2      2
        2      2       VP0  + 2 delta VP0  - VS0
rho (VP0  - VS0 ) Sqrt[--------------------------]
                                 2      2
                              VP0  - VS0
:[font = input; preserveAspect; startGroup]
WeakVTI[%]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(VP0^2 + delta*VP0^2 - VS0^2)
;[o]
        2            2      2
rho (VP0  + delta VP0  - VS0 )
:[font = input; preserveAspect; startGroup]
UseF[%]
:[font = output; output; inactive; preserveAspect; endGroup]
(delta + f)*rho*VP0^2
;[o]
                   2
(delta + f) rho VP0
:[font = text; inactive; preserveAspect]
Some people love Love notation:
:[font = input; preserveAspect; startGroup]
?VoigtToLove
:[font = print; inactive; preserveAspect; endGroup]
VoigtToLove[expression]

 Replace Cij parameters in expression by Love
   parameters. 
:[font = input; preserveAspect; startGroup]
VoigtToLove[C13 + C44]
:[font = output; output; inactive; preserveAspect; endGroup]
F + L
;[o]
F + L
:[font = text; inactive; preserveAspect]
Verify that the expression 
	(1 + F)(-1 + F + 2 L)/(2 - 2 L)
is equal to Thomsen's delta when the Love parameter, C=1:
:[font = input; preserveAspect; startGroup]
(delta//ConvertToVoigt//VoigtToLove)/.C->1
:[font = output; output; inactive; preserveAspect; endGroup]
((1 + F)*(-1 + F + 2*L))/(2*(1 - L))
;[o]
(1 + F) (-1 + F + 2 L)
----------------------
      2 (1 - L)
:[font = text; inactive; preserveAspect; rightWrapOffset = 537; endGroup; endGroup; endGroup]
Remark:  The parentheses are needed above since otherwise the substitution will be attempted before the "afterthought" // operations.
:[font = section; inactive; preserveAspect; startGroup]
Expansions in VTI Parameters
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Weak VTI
:[font = text; inactive; preserveAspect]
Expand the canonical conversion in the limit of weak VTI.  In this limit, delta and deltaTilde are equal, so that the representation is presented in the variables VP0, f, gamma, delta, and epsilon.
:[font = input; preserveAspect; startGroup]
?WeakVTI
:[font = print; inactive; preserveAspect; endGroup]
WeakVTI[expression, (order)]

 Convert expressions in Thomsen notation (either
   standard or canonical) to the weak TI limit in
   terms of VP0, f, gamma, epsilon, and delta.  The
   order parameter gives the order to keep in gamma,
   epsilon, delta and defaults to 1 (the literal weak
   VTI limit). 
:[font = input; preserveAspect; startGroup]
weakOffdiag = WeakVTI[canonicalOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
(-3 + 2*delta + 2*epsilon + 6*f - 4*gamma + 
 
    4*f*gamma)*rho*VP0^2
;[o]
(-3 + 2 delta + 2 epsilon + 6 f - 4 gamma + 
 
                      2
    4 f gamma) rho VP0
:[font = text; inactive; preserveAspect]
Repeat from the standard representation:
:[font = input; preserveAspect; startGroup]
WeakVTI[standardOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(3*VP0^2 + 2*delta*VP0^2 + 2*epsilon*VP0^2 - 
 
    6*VS0^2 - 4*gamma*VS0^2)
;[o]
          2              2                2
rho (3 VP0  + 2 delta VP0  + 2 epsilon VP0  - 
 
         2              2
    6 VS0  - 4 gamma VS0 )
:[font = text; inactive; preserveAspect]
Check by converting the previous result to standard form:
:[font = input; preserveAspect; startGroup]
% - ConvertStandard[weakOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
0
;[o]
0
:[font = text; inactive; preserveAspect]
Remark:  Such results will not always be identically zero,  all we should expect is agreement in the weak limit.
:[font = text; inactive; preserveAspect]
The WeakVTI function "understands" eta:
:[font = input; preserveAspect; startGroup]
(1+2y(2-3y)eta-12y^2 eta^2)/
 ((1-2y)(1-y-2y eta)(1-2y(2-y)eta+4y^2 eta^2))//
 WeakVTI
:[font = output; output; inactive; preserveAspect; endGroup]
(-1 + y - 10*eta*y + 16*eta*y^2 - 8*eta*y^3)/
 
  (-1 + 4*y - 5*y^2 + 2*y^3)
;[o]
                            2          3
-1 + y - 10 eta y + 16 eta y  - 8 eta y
----------------------------------------
                       2      3
         -1 + 4 y - 5 y  + 2 y
:[font = text; inactive; preserveAspect]
In the weak VTI limit,  the distinction between delta and deltaTilde vanishes:
:[font = input; preserveAspect; startGroup]
WeakVTI[deltaTilde]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
delta
;[o]
delta
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Moderate VTI
:[font = text; inactive; preserveAspect]
The weakVTI function can actually produce expansions in the "moderate" VTI regime by using its second argument.  Here is the second and third order expansion of deltaTilde:
:[font = input; preserveAspect; startGroup]
WeakVTI[deltaTilde, 2]
:[font = output; output; inactive; preserveAspect; endGroup]
delta - delta^2/(2*f)
;[o]
             2
        delta
delta - ------
         2 f
:[font = input; preserveAspect; startGroup]
WeakVTI[deltaTilde, 3]
:[font = output; output; inactive; preserveAspect; endGroup]
delta + delta^3/(2*f^2) - delta^2/(2*f)
;[o]
             3        2
        delta    delta
delta + ------ - ------
            2     2 f
         2 f
:[font = input; preserveAspect; startGroup]
% + O[delta]^4
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
SeriesData[delta, 0, {1, -1/(2*f), 1/(2*f^2)}, 1, 4, 
 
  1]
;[o]
             2        3
        delta    delta            4
delta - ------ + ------ + O[delta]
         2 f         2
                  2 f
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
The isotropic limit
:[font = text; inactive; preserveAspect]
Taking  the second argument of WeakVTI as zero, gives isotropic results:
:[font = input; preserveAspect; startGroup]
WeakVTI[standardTrace, 0]
:[font = output; output; inactive; preserveAspect; endGroup]
3*rho*(VP0^2 + VS0^2)
;[o]
          2      2
3 rho (VP0  + VS0 )
:[font = input; preserveAspect; startGroup]
WeakVTI[canonicalTrace, 0]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 - 3*f)*rho*VP0^2
;[o]
                 2
(6 - 3 f) rho VP0
:[font = input; preserveAspect; startGroup]
%//UseVS0
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
3*rho*(VP0^2 + VS0^2)
;[o]
          2      2
3 rho (VP0  + VS0 )
:[font = section; inactive; preserveAspect; startGroup]
Applications
:[font = text; inactive; preserveAspect; rightWrapOffset = 497; endGroup; endGroup]
True geophysical applications are in separate notebooks, such as:

	VTI_SH.ma
	VTI_QP.ma
	WeakVTI.ma
^*)