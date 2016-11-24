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
Ortho Package Examples
:[font = subtitle; inactive; preserveAspect; fontSize = 18]
Copyright, Jack K. Cohen, 1996, All Rights Reserved
;[s]
1:0,0;51,-1;
1:1,0,0 ,times,2,18,0,0,0;
:[font = input; initialization; preserveAspect]
*)
<<Ortho.m
Off[General::spell, General::spell1]
(*
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
General Remarks
:[font = text; inactive; preserveAspect; rightWrapOffset = 522; endGroup]
The following variables are reserved and cannot be assigned to:

rho, VP0, VS0, VS1, gamma1,  gamma2, gammaT, gammaS, delta1, delta2, delta3, delta1Tilde, delta2Tilde, delta3Tilde, epsilon1, epsilon2, eta1, eta2,  f1,  f2,  and Cij, where i and j are integers in the range [1,6].
Notation:
VP0 = Sqrt[C33/rho]
VS0 = Sqrt[C55/rho]
VS1 = Sqrt[C44/rho]
gammaT = (C55-C44)/(2C44) = -gammaS/(1 + 2 gammaS)

The Thomsen-Tsvankin parameters, epsilon1, epsilon2, gamma1,
gamma1,  delta1, delta2, delta3, along with VP0 and VS0 form
the standard five variables.  The package also recognizes the
"splitting" parameter, gammaS.  For ease of expression, define  the following "canonical" variables:
VP0, epsilon1, epsilon2, gamma2, gammaT
and the four auxiliary quantities,
	f1 = 1 -(VS1/VP0)^2 ,
and
	delta2Tilde = (C13 - 2 C55 - C33)/C33,
	delta1Tilde = (C23 - 2 C44- C33)/C33,
	delta3Tilde = (C12 - 2 C66 - C11)/C11,

The deltaTildes agree with the deltas in the weak limit.  The function
	EliminateDeltaTildes
eliminates the deltaTildes in favor of the deltas.  The relation between
deltaTilde and  delta is best understood by the relation
	f^2 + 2f delta = (f + deltaTilde)^2

where:
	f2 = 1- (VS1/VP0)^2 (1 + 2 gammaT),
	f1 = 1 - (VS1/VP0)^2,
	f3 = 1 - (VS1/VP0)^2 (1 + 2 gamma1).
	
The user has access to the  additional auxiliary variables:
	VS1, f1
	eta1 = (epsilon1 - delta1)/(1 + 2 delta1),
	eta2 = (epsilon2 - delta2)/(1 + 2 delta2),
	gammaS = (C44 - C55)/(2C55) =
		 -gammaT/(1 + 2 gammaT).
		 
When replacing f2, expressions are usually simpler in terms of VS1 and gammaT, than with VS0 and gamma2 or gammaS and the user is given a choice as illustrated in the examples below
:[font = section; inactive; preserveAspect; rightWrapOffset = 542; startGroup]
Cij to Thomsen-Tsvankin Parameters
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Some Test Expressions
:[font = input; initialization; preserveAspect; endGroup]
*)
trace = C11 + C22 + C33 + C44 + C55 + C66;
offdiag = C13 + C23 + C12;
(*
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
The Standard Voigt Parameters for Ortho
:[font = text; inactive; preserveAspect]
Orthorhombic media require only 9 of the 21 hookean constants.  In Voigt notation these are C11, C22, C33, C44,  C55, C66, C13, C23, and C12.   However, the user can enter C14 or other Cij in expressions:
:[font = input; preserveAspect; startGroup]
?VoigtOrtho
:[font = print; inactive; preserveAspect; endGroup]
VoigtOrtho[expression]

 Reduce expressions in Cij to the canonical Ortho set,
   C11, C22, C33, C44, C55, C66, C13, C23, C12. 
:[font = input; preserveAspect; startGroup]
VoigtOrtho[C14+C24+C34+C44+C54+C64]
:[font = output; output; inactive; preserveAspect; endGroup]
C44
;[o]
C44
:[font = text; inactive; preserveAspect; endGroup]
Remark:  Other commands in the package that take Voigt notation input do this conversion automatically.
:[font = subsection; inactive; preserveAspect; startGroup]
Canonical vs. Standard representation
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Converting to canonical representation
:[font = input; preserveAspect; startGroup]
?ConvertCanonical
:[font = print; inactive; preserveAspect; endGroup]
ConvertCanonical[expression]

 Convert expression from Voigt Cij notation to
   Thomsen-Tsvankin notation in terms of the canonical
   variables: VP0, f1, gamma2, gammaT, epsilon1,
   epsilon2, delta1Tilde, delta2Tilde, and delta3Tilde.
:[font = input; initialization; preserveAspect; startGroup]
*)
canonicalTrace = ConvertCanonical[trace]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 2*epsilon1 + 2*epsilon2 - 
 
    3*f1 + 2*gammaT - 2*f1*gammaT + 
 
    2*gamma2 - 2*f1*gamma2)*rho*
 
  VP0^2
;[o]
(6 + 2 epsilon1 + 2 epsilon2 - 
 
    3 f1 + 2 gammaT - 2 f1 gammaT + 
 
                                   2
    2 gamma2 - 2 f1 gamma2) rho VP0
:[font = input; initialization; preserveAspect; startGroup]
*)
canonicalOffdiag = ConvertCanonical[offdiag]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
(-3 + delta1Tilde + delta2Tilde + 
 
    delta3Tilde + 2*epsilon2 + 
 
    2*delta3Tilde*epsilon2 + 6*f1 - 
 
    4*gammaT + 4*f1*gammaT - 
 
    4*gamma2 + 4*f1*gamma2)*rho*
 
  VP0^2
;[o]
(-3 + delta1Tilde + delta2Tilde + 
 
    delta3Tilde + 2 epsilon2 + 
 
    2 delta3Tilde epsilon2 + 6 f1 - 
 
    4 gammaT + 4 f1 gammaT - 
 
                                   2
    4 gamma2 + 4 f1 gamma2) rho VP0
:[font = text; inactive; preserveAspect]
Let's do an internal check by converting back to Voigt notation:
:[font = input; preserveAspect; startGroup]
?ConvertToVoigt
:[font = print; inactive; preserveAspect; endGroup]
ConvertToVoigt[expression]

 Convert expression from Thomsen-Tsvankin notation to
   Voigt Cij notation. 
:[font = input; preserveAspect; startGroup]
ConvertToVoigt[canonicalTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
C11 + C22 + C33 + C44 + C55 + C66
;[o]
C11 + C22 + C33 + C44 + C55 + C66
:[font = input; preserveAspect; startGroup]
ConvertToVoigt[canonicalOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
C12 + C13 + C23
;[o]
C12 + C13 + C23
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Converting to standard representation
:[font = input; preserveAspect; startGroup]
?ConvertStandard
:[font = print; inactive; preserveAspect; endGroup]
ConvertStandard[expression]

 Convert expression from Voigt Cij notation to
   Thomsen-Tsvankin notation in terms of the standard
   variables: VP0, VS0, gamma1, gamma2, epsilon1,
   epsilon2, delta1, delta2, and delta3. 
:[font = input; initialization; preserveAspect; startGroup]
*)
standardTrace = ConvertStandard[trace]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
(rho*(3*VP0^2 + 2*epsilon1*VP0^2 + 
 
      2*epsilon2*VP0^2 + 
 
      6*gamma2*VP0^2 + 
 
      4*epsilon1*gamma2*VP0^2 + 
 
      4*epsilon2*gamma2*VP0^2 + 
 
      3*VS0^2 + 4*gamma1*VS0^2 + 
 
      4*gamma2*VS0^2 + 
 
      4*gamma1*gamma2*VS0^2))/
 
  (1 + 2*gamma2)
;[o]
           2                 2
(rho (3 VP0  + 2 epsilon1 VP0  + 
 
                    2
      2 epsilon2 VP0  + 
 
                  2
      6 gamma2 VP0  + 
 
                           2
      4 epsilon1 gamma2 VP0  + 
 
                           2
      4 epsilon2 gamma2 VP0  + 
 
           2               2
      3 VS0  + 4 gamma1 VS0  + 
 
                  2
      4 gamma2 VS0  + 
 
                         2
      4 gamma1 gamma2 VS0 )) / 
 
  (1 + 2 gamma2)
:[font = input; preserveAspect; startGroup]
Collect[%, {rho, VP0, VS0}]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*((3/(1 + 2*gamma2) + (2*epsilon1)/(1 + 2*gamma2) + 
 
       (2*epsilon2)/(1 + 2*gamma2) + 
 
       (6*gamma2)/(1 + 2*gamma2) + 
 
       (4*epsilon1*gamma2)/(1 + 2*gamma2) + 
 
       (4*epsilon2*gamma2)/(1 + 2*gamma2))*VP0^2 + 
 
    (3/(1 + 2*gamma2) + (4*gamma1)/(1 + 2*gamma2) + 
 
       (4*gamma2)/(1 + 2*gamma2) + 
 
       (4*gamma1*gamma2)/(1 + 2*gamma2))*VS0^2)
;[o]
           3          2 epsilon1     2 epsilon2
rho ((------------ + ------------ + ------------ + 
      1 + 2 gamma2   1 + 2 gamma2   1 + 2 gamma2
 
         6 gamma2     4 epsilon1 gamma2
       ------------ + ----------------- + 
       1 + 2 gamma2     1 + 2 gamma2
 
       4 epsilon2 gamma2     2
       -----------------) VP0  + 
         1 + 2 gamma2
 
          3           4 gamma1       4 gamma2
    (------------ + ------------ + ------------ + 
     1 + 2 gamma2   1 + 2 gamma2   1 + 2 gamma2
 
       4 gamma1 gamma2     2
       ---------------) VS0 )
        1 + 2 gamma2
:[font = text; inactive; preserveAspect]
Again check:
:[font = input; preserveAspect; startGroup]
ConvertToVoigt[standardTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
C11 + C22 + C33 + C44 + C55 + C66
;[o]
C11 + C22 + C33 + C44 + C55 + C66
:[font = input; initialization; preserveAspect; startGroup]
*)
standardOffdiag = ConvertStandard[offdiag]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
rho*VP0^2*(-3 + 2*epsilon2 + 
 
    (4*(gamma1 - gamma2))/
 
     (1 + 2*gamma1) - 4*gamma2 + 
 
    6*(1 - ((1 + 2*gamma1)*VS0^2)/
 
        ((1 + 2*gamma2)*VP0^2)) + 
 
    4*gamma2*(1 - 
 
       ((1 + 2*gamma1)*VS0^2)/
 
        ((1 + 2*gamma2)*VP0^2)) + 
 
    (4*(-gamma1 + gamma2)*
 
       (1 - ((1 + 2*gamma1)*VS0^2)/
 
          ((1 + 2*gamma2)*VP0^2)))/
 
     (1 + 2*gamma1) + 
 
    (1 - VS0^2/VP0^2)*
 
     (-1 + (1 + 
 
          (2*delta2)/
 
           (1 - VS0^2/VP0^2))^(1/2))
 
      + (1 - ((1 + 2*gamma1)*VS0^2)/
 
        ((1 + 2*epsilon2)*VP0^2))*
 
     (-1 + (1 + 
 
          (2*delta3)/
 
           (1 - 
 
             ((1 + 2*gamma1)*VS0^2)/
 
             ((1 + 2*epsilon2)*
 
            VP0^2)))^(1/2)) + 
 
    2*epsilon2*
 
     (1 - ((1 + 2*gamma1)*VS0^2)/
 
        ((1 + 2*epsilon2)*VP0^2))*
 
     (-1 + (1 + 
 
          (2*delta3)/
 
           (1 - 
 
             ((1 + 2*gamma1)*VS0^2)/
 
             ((1 + 2*epsilon2)*
 
            VP0^2)))^(1/2)) + 
 
    (1 - ((1 + 2*gamma1)*VS0^2)/
 
        ((1 + 2*gamma2)*VP0^2))*
 
     (-1 + (1 + 
 
          (2*delta1)/
 
           (1 - 
 
             ((1 + 2*gamma1)*VS0^2)/
 
             ((1 + 2*gamma2)*VP0^2))
 
          )^(1/2)))
;[o]
       2
rho VP0  (-3 + 2 epsilon2 + 
 
    4 (gamma1 - gamma2)
    ------------------- - 
       1 + 2 gamma1
 
    4 gamma2 + 
 
                             2
           (1 + 2 gamma1) VS0
    6 (1 - -------------------) + 
                             2
           (1 + 2 gamma2) VP0
 
    4 gamma2 (1 - 
 
                         2
       (1 + 2 gamma1) VS0
       -------------------) + 
                         2
       (1 + 2 gamma2) VP0
 
    (4 (-gamma1 + gamma2) 
 
                              2
            (1 + 2 gamma1) VS0
       (1 - -------------------)) / 
                              2
            (1 + 2 gamma2) VP0
 
     (1 + 2 gamma1) + 
 
            2
         VS0
    (1 - ----) 
            2
         VP0
 
                    2 delta2
     (-1 + Sqrt[1 + --------]) + 
                           2
                        VS0
                    1 - ----
                           2
                        VP0
 
                            2
          (1 + 2 gamma1) VS0
    (1 - ---------------------) 
                             2
         (1 + 2 epsilon2) VP0
 
     (-1 + Sqrt[1 + 
 
                 2 delta3
         -------------------------])
                                2
              (1 + 2 gamma1) VS0
         1 - ---------------------
                                 2
             (1 + 2 epsilon2) VP0
 
      + 2 epsilon2 
 
                             2
           (1 + 2 gamma1) VS0
     (1 - ---------------------) 
                              2
          (1 + 2 epsilon2) VP0
 
     (-1 + Sqrt[1 + 
 
                 2 delta3
         -------------------------])
                                2
              (1 + 2 gamma1) VS0
         1 - ---------------------
                                 2
             (1 + 2 epsilon2) VP0
 
                               2
             (1 + 2 gamma1) VS0
      + (1 - -------------------) 
                               2
             (1 + 2 gamma2) VP0
 
     (-1 + Sqrt[1 + 
 
                2 delta1
         -----------------------]))
                               2
             (1 + 2 gamma1) VS0
         1 - -------------------
                               2
             (1 + 2 gamma2) VP0
:[font = text; inactive; preserveAspect]
Converting back turns into a wrestling match:
:[font = input; preserveAspect; startGroup]
backdiag = ConvertToVoigt[standardOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
-C44 + C33*((C23 + C44)^2/(C33 - C44)^2)^(1/2) - 
 
  C44*((C23 + C44)^2/(C33 - C44)^2)^(1/2) - C55 + 
 
  C33*((C13 + C55)^2/(C33 - C55)^2)^(1/2) - 
 
  C55*((C13 + C55)^2/(C33 - C55)^2)^(1/2) - C66 + 
 
  C11*((C12 + C66)^2/(C11 - C66)^2)^(1/2) - 
 
  C66*((C12 + C66)^2/(-C11 + C66)^2)^(1/2)
;[o]
                           2
                (C23 + C44)
-C44 + C33 Sqrt[------------] - 
                           2
                (C33 - C44)
 
                      2
           (C23 + C44)
  C44 Sqrt[------------] - C55 + 
                      2
           (C33 - C44)
 
                      2                        2
           (C13 + C55)              (C13 + C55)
  C33 Sqrt[------------] - C55 Sqrt[------------] - 
                      2                        2
           (C33 - C55)              (C33 - C55)
 
                            2
                 (C12 + C66)
  C66 + C11 Sqrt[------------] - 
                            2
                 (C11 - C66)
 
                      2
           (C12 + C66)
  C66 Sqrt[-------------]
                       2
           (-C11 + C66)
:[font = text; inactive; preserveAspect]
OK, Mathematica won't extract square roots of apparent perfect squares just because of the possible sign ambiguities.  PowerExpand forces the positive square roots, but look at that last term--it is going to expand to the wrong sign!   Look:
:[font = input; preserveAspect; startGroup]
PowerExpand[backdiag]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
(C11*C12 + C11*C13 + C11*C23 + C12*C66 - C13*C66 - 
 
    C23*C66 + 2*C66^2)/(C11 - C66)
;[o]
(C11 C12 + C11 C13 + C11 C23 + C12 C66 - C13 C66 - 
 
                   2
    C23 C66 + 2 C66 ) / (C11 - C66)
:[font = text; inactive; preserveAspect]
So we have to intervene before calling PowerExpand:
:[font = input; preserveAspect; startGroup]
backdiag/.-C11 + C66 ->-diff16//PowerExpand;
%/.diff16 -> C11 - C66//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
C12 + C13 + C23
;[o]
C12 + C13 + C23
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
UseVS1, UseF1
:[font = text; inactive; preserveAspect]
Instead of going to full standard variables, we may wish to just introduce some of them.  For example,  if  we want to see the speed VS1 instead of f1:
:[font = input; preserveAspect; startGroup]
?UseVS1
:[font = print; inactive; preserveAspect; endGroup]
UseVS1[expression]

 Replace f1 in expression by VP0 and VS1 and replace
   VS0 by these and gammaT. 
:[font = input; preserveAspect; startGroup]
UseVS1[canonicalTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(3*VP0^2 + 2*epsilon1*VP0^2 + 2*epsilon2*VP0^2 + 
 
    3*VS1^2 + 2*gammaT*VS1^2 + 2*gamma2*VS1^2)
;[o]
          2                 2                 2
rho (3 VP0  + 2 epsilon1 VP0  + 2 epsilon2 VP0  + 
 
         2               2               2
    3 VS1  + 2 gammaT VS1  + 2 gamma2 VS1 )
:[font = text; inactive; preserveAspect]
Change back to the canonical representation by eliminating VS1 in favor of f1:
:[font = input; preserveAspect; startGroup]
?UseF1
:[font = print; inactive; preserveAspect; endGroup]
UseF1[expression]

 Replace VS1^2 in expression by VP0^2 and f1 and
   replace VS0 by these and gammaT.  An odd power of
   VS1 will result in having a single VS1 factor in the
   result. 
:[font = input; preserveAspect; startGroup]
UseF1[%%]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 2*epsilon1 + 2*epsilon2 - 3*f1 + 2*gammaT - 
 
    2*f1*gammaT + 2*gamma2 - 2*f1*gamma2)*rho*VP0^2
;[o]
(6 + 2 epsilon1 + 2 epsilon2 - 3 f1 + 2 gammaT - 
 
                                                 2
    2 f1 gammaT + 2 gamma2 - 2 f1 gamma2) rho VP0
:[font = text; inactive; preserveAspect]
Check we got the right thing:
:[font = input; preserveAspect; startGroup]
% - canonicalTrace//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
0
;[o]
0
:[font = text; inactive; preserveAspect]
Notice that we don't fully eliminate VS1 if it appears to an odd power, directly or indirectly:
:[font = input; preserveAspect; startGroup]
UseF1[VS1^5]
:[font = output; output; inactive; preserveAspect; endGroup]
(1 - f1)^2*VP0^4*VS1
;[o]
        2    4
(1 - f1)  VP0  VS1
:[font = input; preserveAspect; startGroup]
UseF1[VS0^5]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(1 - f1)^2*(1 + 2*gammaT)^(5/2)*VP0^4*VS1
;[o]
        2               5/2    4
(1 - f1)  (1 + 2 gammaT)    VP0  VS1
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
UseVS0, UseF2
:[font = text; inactive; preserveAspect]
The speed VS0 can lead to messier expressions, but it is available in the same way:
:[font = input; preserveAspect; startGroup]
?UseVS0
:[font = print; inactive; preserveAspect; endGroup]
UseVS0[expression]

 Replace VS1 in expression by VS0 and gammaS and
   replace f1 by these and VP0. 
:[font = input; preserveAspect; startGroup]
UseVS0[canonicalTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(3*VP0^2 + 2*epsilon1*VP0^2 + 2*epsilon2*VP0^2 + 
 
    3*VS0^2 + 4*gammaS*VS0^2 + 2*gamma2*VS0^2 + 
 
    4*gammaS*gamma2*VS0^2)
;[o]
          2                 2                 2
rho (3 VP0  + 2 epsilon1 VP0  + 2 epsilon2 VP0  + 
 
         2               2               2
    3 VS0  + 4 gammaS VS0  + 2 gamma2 VS0  + 
 
                       2
    4 gammaS gamma2 VS0 )
:[font = text; inactive; preserveAspect]
Change back to the canonical representation by eliminating VS0 in favor of f1:
:[font = input; preserveAspect; startGroup]
UseF1[%]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 2*epsilon1 + 2*epsilon2 - 3*f1 + 2*gammaT - 
 
    2*f1*gammaT + 2*gamma2 - 2*f1*gamma2)*rho*VP0^2
;[o]
(6 + 2 epsilon1 + 2 epsilon2 - 3 f1 + 2 gammaT - 
 
                                                 2
    2 f1 gammaT + 2 gamma2 - 2 f1 gamma2) rho VP0
:[font = text; inactive; preserveAspect]
Check we got the right thing:
:[font = input; preserveAspect; startGroup]
% - canonicalTrace//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
0
;[o]
0
:[font = text; inactive; preserveAspect]
f2 is the Tsvankin f quantity: 1 - VS0^2/VP0^2
:[font = input; preserveAspect; startGroup]
?UseF2
:[font = print; inactive; preserveAspect; endGroup]
UseF2[expression]

 Replace f1 by f2 and gammaS and eliminate gamma1 and
   gammaT in favor of gammaS. 
:[font = input; preserveAspect; startGroup]
UseF2[canonicalTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 2*epsilon1 + 2*epsilon2 - 3*f2 + 4*gammaS - 
 
    4*f2*gammaS + 2*gamma2 - 2*f2*gamma2 + 
 
    4*gammaS*gamma2 - 4*f2*gammaS*gamma2)*rho*VP0^2
;[o]
(6 + 2 epsilon1 + 2 epsilon2 - 3 f2 + 4 gammaS - 
 
    4 f2 gammaS + 2 gamma2 - 2 f2 gamma2 + 
 
                                                 2
    4 gammaS gamma2 - 4 f2 gammaS gamma2) rho VP0
:[font = text; inactive; preserveAspect]
Convert back to f1
:[font = input; preserveAspect; startGroup]
UseF1[%]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 2*epsilon1 + 2*epsilon2 - 3*f1 + 2*gammaT - 
 
    2*f1*gammaT + 2*gamma2 - 2*f1*gamma2)*rho*VP0^2
;[o]
(6 + 2 epsilon1 + 2 epsilon2 - 3 f1 + 2 gammaT - 
 
                                                 2
    2 f1 gammaT + 2 gamma2 - 2 f1 gamma2) rho VP0
:[font = text; inactive; preserveAspect]
And check:
:[font = input; preserveAspect; startGroup]
% - canonicalTrace//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
0
;[o]
0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
UseGammaS, UseGammaT, UseGamma1
:[font = text; inactive; preserveAspect]
Eliminate gammaT in favor of gammaS:
:[font = input; preserveAspect; startGroup]
?UseGammaS
:[font = print; inactive; preserveAspect; endGroup]
UseGammaS[expression]

 Replace gammaT and gamma1 in expression by gammaS and
   gamma2. 
:[font = input; preserveAspect; startGroup]
UseGammaS[canonicalTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 2*epsilon1 + 2*epsilon2 - 3*f1 - 
 
    (2*gammaS)/(1 + 2*gammaS) + 
 
    (2*f1*gammaS)/(1 + 2*gammaS) + 2*gamma2 - 
 
    2*f1*gamma2)*rho*VP0^2
;[o]
                                        2 gammaS
(6 + 2 epsilon1 + 2 epsilon2 - 3 f1 - ------------ + 
                                      1 + 2 gammaS
 
    2 f1 gammaS                                   2
    ------------ + 2 gamma2 - 2 f1 gamma2) rho VP0
    1 + 2 gammaS
:[font = text; inactive; preserveAspect]
And change back:
:[font = input; preserveAspect; startGroup]
?UseGammaT
:[font = print; inactive; preserveAspect; endGroup]
UseGammaT[expression]

 Replace gammaS and gamma1 in expression by gammaT and
   gamma2. 
:[font = input; preserveAspect; startGroup]
UseGammaT[%%]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 2*epsilon1 + 2*epsilon2 - 3*f1 + 2*gammaT - 
 
    2*f1*gammaT + 2*gamma2 - 2*f1*gamma2)*rho*VP0^2
;[o]
(6 + 2 epsilon1 + 2 epsilon2 - 3 f1 + 2 gammaT - 
 
                                                 2
    2 f1 gammaT + 2 gamma2 - 2 f1 gamma2) rho VP0
:[font = input; preserveAspect; startGroup]
% - canonicalTrace
:[font = output; output; inactive; preserveAspect; endGroup]
0
;[o]
0
:[font = text; inactive; preserveAspect]
Eliminate gammaT and gammaS in favor of gamma1:
:[font = input; preserveAspect; startGroup]
?UseGamma1
:[font = print; inactive; preserveAspect; endGroup]
UseGamma1[expression]

 Replace gammaS and gammaT in expression by gamma1 and
   gamma2. 
:[font = input; preserveAspect; startGroup]
UseGamma1[17gammaS + canonicalTrace]
:[font = output; output; inactive; preserveAspect; endGroup]
(17*(gamma1 - gamma2))/(1 + 2*gamma2) + 
 
  (6 + 2*epsilon1 + 2*epsilon2 - 3*f1 + 
 
     (2*f1*(gamma1 - gamma2))/(1 + 2*gamma1) + 
 
     2*gamma2 - 2*f1*gamma2 + 
 
     (2*(-gamma1 + gamma2))/(1 + 2*gamma1))*rho*VP0^2
;[o]
17 (gamma1 - gamma2)
-------------------- + 
    1 + 2 gamma2
 
  (6 + 2 epsilon1 + 2 epsilon2 - 3 f1 + 
 
     2 f1 (gamma1 - gamma2)
     ---------------------- + 2 gamma2 - 2 f1 gamma2 + 
          1 + 2 gamma1
 
     2 (-gamma1 + gamma2)         2
     --------------------) rho VP0
         1 + 2 gamma1
:[font = input; preserveAspect; startGroup]
UseGammaT[%]
:[font = output; output; inactive; preserveAspect; endGroup]
(-17*gammaT)/(1 + 2*gammaT) + 
 
  (6 + 2*epsilon1 + 2*epsilon2 - 3*f1 + 2*gammaT - 
 
     2*f1*gammaT + 2*gamma2 - 2*f1*gamma2)*rho*VP0^2
;[o]
 -17 gammaT
------------ + (6 + 2 epsilon1 + 2 epsilon2 - 3 f1 + 
1 + 2 gammaT
 
     2 gammaT - 2 f1 gammaT + 2 gamma2 - 2 f1 gamma2) 
 
          2
   rho VP0
:[font = input; preserveAspect; startGroup]
% - canonicalTrace - 17gammaS
:[font = output; output; inactive; preserveAspect; endGroup]
-17*gammaS - (17*gammaT)/(1 + 2*gammaT)
;[o]
              17 gammaT
-17 gammaS - ------------
             1 + 2 gammaT
:[font = input; preserveAspect; startGroup]
UseGammaS[%]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
0
;[o]
0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
UseDelta, UseDeltaTildes
:[font = text; inactive; preserveAspect]
Eliminate the deltaTildes in favor of the deltas:
:[font = input; preserveAspect; startGroup]
?UseDeltas
:[font = print; inactive; preserveAspect; endGroup]
UseDeltas[expression]

 Replace delta1Tilde, delta2Tilde, delta3Tilde in
   expression by deltas, gammas, and f1. 
:[font = input; preserveAspect; startGroup]
UseDeltas[canonicalOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
(-3 + 2*epsilon2 + 6*f1 + 
 
    (-1 + (1 + (2*delta1)/f1)^(1/2))*f1 - 4*gammaT + 
 
    4*f1*gammaT + (f1 - 2*gammaT + 2*f1*gammaT)*
 
     (-1 + (1 + (2*delta2)/
 
           (f1 - 2*gammaT + 2*f1*gammaT))^(1/2)) - 
 
    4*gamma2 + 4*f1*gamma2 + 
 
    (1 + ((-1 + f1)*(1 + 2*gamma2))/(1 + 2*epsilon2))*
 
     (-1 + (1 + (2*delta3)/
 
           (1 + ((-1 + f1)*(1 + 2*gamma2))/
 
              (1 + 2*epsilon2)))^(1/2)) + 
 
    2*epsilon2*(1 + ((-1 + f1)*(1 + 2*gamma2))/
 
        (1 + 2*epsilon2))*
 
     (-1 + (1 + (2*delta3)/
 
           (1 + ((-1 + f1)*(1 + 2*gamma2))/
 
              (1 + 2*epsilon2)))^(1/2)))*rho*VP0^2
;[o]
(-3 + 2 epsilon2 + 6 f1 + 
 
                   2 delta1
    (-1 + Sqrt[1 + --------]) f1 - 4 gammaT + 
                      f1
 
    4 f1 gammaT + (f1 - 2 gammaT + 2 f1 gammaT) 
 
                             2 delta2
     (-1 + Sqrt[1 + ---------------------------]) - 
                    f1 - 2 gammaT + 2 f1 gammaT
 
    4 gamma2 + 4 f1 gamma2 + 
 
         (-1 + f1) (1 + 2 gamma2)
    (1 + ------------------------) 
              1 + 2 epsilon2
 
                              2 delta3
     (-1 + Sqrt[1 + ----------------------------]) + 
                        (-1 + f1) (1 + 2 gamma2)
                    1 + ------------------------
                             1 + 2 epsilon2
 
                    (-1 + f1) (1 + 2 gamma2)
    2 epsilon2 (1 + ------------------------) 
                         1 + 2 epsilon2
 
                              2 delta3
     (-1 + Sqrt[1 + ----------------------------])) 
                        (-1 + f1) (1 + 2 gamma2)
                    1 + ------------------------
                             1 + 2 epsilon2
 
         2
  rho VP0
:[font = text; inactive; preserveAspect]
And again change back to the canonical representation by eliminating the deltas in favor of the deltaTildes:
:[font = input; preserveAspect; startGroup]
?UseDeltaTildes
:[font = print; inactive; preserveAspect; endGroup]
UseDeltaTildes[expression]

 Replace delta1, delta2, delta3 in expression by
   deltaTildes, gammas, and f1. 
:[font = input; preserveAspect; startGroup]
UseDeltaTildes[%%]
:[font = output; output; inactive; preserveAspect; endGroup]
(-3 + delta1Tilde + delta2Tilde + delta3Tilde + 
 
    2*epsilon2 + 2*delta3Tilde*epsilon2 + 6*f1 - 
 
    4*gammaT + 4*f1*gammaT - 4*gamma2 + 4*f1*gamma2)*
 
  rho*VP0^2
;[o]
(-3 + delta1Tilde + delta2Tilde + delta3Tilde + 
 
    2 epsilon2 + 2 delta3Tilde epsilon2 + 6 f1 - 
 
    4 gammaT + 4 f1 gammaT - 4 gamma2 + 4 f1 gamma2) 
 
         2
  rho VP0
:[font = text; inactive; preserveAspect]
Again check it:
:[font = input; preserveAspect; startGroup]
% - canonicalOffdiag//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
0
;[o]
0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
UseEtas, UseEpsilons
:[font = text; inactive; preserveAspect]
In the study of the normal moveout function in a VTI medium, Akhalifah and Tsvankin introduced the anisotropic parameter eta which together with the moveout velocity at zero phase angle, allows the entire moveout function to be characterized
by these two parameters.  That is, the additional parameters that could logically appear (eg., f and delta) influence the  moveout function only slightly.
:[font = input; preserveAspect; startGroup]
?UseEtas
:[font = print; inactive; preserveAspect; endGroup]
UseEtas[expression]

 Replace epsilons in expression by etas and deltas. 
:[font = input; preserveAspect; startGroup]
UseEtas[2epsilon1 + 3epsilon2]
:[font = output; output; inactive; preserveAspect; endGroup]
2*delta1 + 3*delta2 + 2*eta1 + 4*delta1*eta1 + 
 
  3*eta2 + 6*delta2*eta2
;[o]
2 delta1 + 3 delta2 + 2 eta1 + 4 delta1 eta1 + 
 
  3 eta2 + 6 delta2 eta2
:[font = input; preserveAspect; startGroup]
Collect[%, {eta1, eta2}]
:[font = output; output; inactive; preserveAspect; endGroup]
2*delta1 + 3*delta2 + (2 + 4*delta1)*eta1 + 
 
  (3 + 6*delta2)*eta2
;[o]
2 delta1 + 3 delta2 + (2 + 4 delta1) eta1 + 
 
  (3 + 6 delta2) eta2
:[font = text; inactive; preserveAspect]
UseEpsilons does the reverse:
:[font = input; preserveAspect; startGroup]
?UseEpsilons
:[font = print; inactive; preserveAspect; endGroup]
UseEpsilons[expression]

 Replace etas in expression by deltas and epsilons. 
:[font = input; preserveAspect; startGroup]
UseEpsilons[%%]
:[font = output; output; inactive; preserveAspect; endGroup]
2*epsilon1 + 3*epsilon2
;[o]
2 epsilon1 + 3 epsilon2
:[font = input; preserveAspect; startGroup]
UseEtas[(epsilon2-delta2)/(1 + 2 delta2)]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup; endGroup]
eta2
;[o]
eta2
:[font = section; inactive; preserveAspect; startGroup]
Expansions in Ortho Parameters
:[font = subsection; inactive; preserveAspect; startGroup]
Weak Ortho
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Basic usage
:[font = text; inactive; preserveAspect]
Expand the canonical conversion in the limit of weak Ortho.  In this limit, the deltas and deltaTildes are equal, and gammaS = gamma2-gamma1, gammaT = - gammaS, so that the representation is presented in the variables VS1, f1, gamma1, gamma2, delta1, delta2, delta3, epsilon1, and epsilon2.
:[font = input; preserveAspect; startGroup]
?WeakOrtho
:[font = print; inactive; preserveAspect; endGroup]
WeakOrtho[expression, (order)]

 Convert expressions in Thomsen-Tsvankin notation
   (either standard or canonical) to the weak TI limit
   in terms of VP0, f1, gamma1, gamma2, epsilon1,
   epsilon2, delta1, delta2, and delta3.  The order
   parameter gives the order to keep in the gammas,
   epsilons, and deltas.  It defaults to 1 (the literal
   weak Ortho limit). 
:[font = input; preserveAspect; startGroup]
weakOffdiag1 = WeakOrtho[canonicalOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
(-3 + delta1 + delta2 + delta3 + 2*epsilon2 + 6*f1 + 
 
    4*gamma1 - 4*f1*gamma1 - 8*gamma2 + 8*f1*gamma2)*
 
  rho*VP0^2
;[o]
(-3 + delta1 + delta2 + delta3 + 2 epsilon2 + 6 f1 + 
 
    4 gamma1 - 4 f1 gamma1 - 8 gamma2 + 8 f1 gamma2) 
 
         2
  rho VP0
:[font = text; inactive; preserveAspect]
Repeat from the standard representation:
:[font = input; preserveAspect; startGroup]
WeakOrtho[standardOffdiag]
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(3*VP0^2 + delta1*VP0^2 + delta2*VP0^2 + 
 
    delta3*VP0^2 + 2*epsilon2*VP0^2 - 6*VS0^2 - 
 
    8*gamma1*VS0^2 + 4*gamma2*VS0^2)
;[o]
          2             2             2
rho (3 VP0  + delta1 VP0  + delta2 VP0  + 
 
              2                 2        2
    delta3 VP0  + 2 epsilon2 VP0  - 6 VS0  - 
 
                2               2
    8 gamma1 VS0  + 4 gamma2 VS0 )
:[font = text; inactive; preserveAspect]
Check by converting the previous result to standard form:
:[font = input; preserveAspect; startGroup]
% - ConvertStandard[weakOffdiag1]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
(8*(-gamma1^2 + gamma2^2)*rho*VS0^2)/(1 + 2*gamma2)
;[o]
          2         2         2
8 (-gamma1  + gamma2 ) rho VS0
-------------------------------
         1 + 2 gamma2
:[font = text; inactive; preserveAspect]
The result is not identically zero, but it does vanish in the weak limit, which is all we should expect:
:[font = input; preserveAspect; startGroup]
WeakOrtho[%]
:[font = output; output; inactive; preserveAspect; endGroup]
0
;[o]
0
:[font = text; inactive; preserveAspect]
The WeakOrtho function "understands" the etas:
:[font = input; preserveAspect; startGroup]
(1+2y(2-3y)eta2-12y^2 eta2^2)/
 ((1-2y)(1-y-2y eta2)(1-2y(2-y)eta2+4y^2 eta2^2))//
 WeakOrtho
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(-1 + y - 10*eta2*y + 16*eta2*y^2 - 8*eta2*y^3)/
 
  (-1 + 4*y - 5*y^2 + 2*y^3)
;[o]
                              2           3
-1 + y - 10 eta2 y + 16 eta2 y  - 8 eta2 y
-------------------------------------------
                        2      3
          -1 + 4 y - 5 y  + 2 y
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Expansions of Canonical Parameters
:[font = text; inactive; preserveAspect]
In the weak Ortho limit, the non-standard parameters have simple expressions in terms of the standard ones.  First of all, the distinction between the deltas and the deltaTildes vanishes:
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta1Tilde]
:[font = output; output; inactive; preserveAspect; endGroup]
delta1
;[o]
delta1
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta2Tilde]
:[font = output; output; inactive; preserveAspect; endGroup]
delta2
;[o]
delta2
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta3Tilde]
:[font = output; output; inactive; preserveAspect; endGroup]
delta3
;[o]
delta3
:[font = text; inactive; preserveAspect]
And gammaS = gamma1 - gamma2 = -gammaT:
:[font = input; preserveAspect; startGroup]
WeakOrtho[gammaS]
:[font = output; output; inactive; preserveAspect; endGroup]
gamma1 - gamma2
;[o]
gamma1 - gamma2
:[font = input; preserveAspect; startGroup]
WeakOrtho[gammaT]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
-gamma1 + gamma2
;[o]
-gamma1 + gamma2
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Moderate Ortho
:[font = text; inactive; preserveAspect]
The weakOrtho function can actually produce expansions in the "moderate" Ortho regime by using its second argument.  Here are second and third order expansions of the deltaTildes:
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta1Tilde, 2]
:[font = output; output; inactive; preserveAspect; endGroup]
delta1 - delta1^2/(2*f1)
;[o]
               2
         delta1
delta1 - -------
          2 f1
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta1Tilde, 3]
:[font = output; output; inactive; preserveAspect; endGroup]
delta1 + delta1^3/(2*f1^2) - delta1^2/(2*f1)
;[o]
               3         2
         delta1    delta1
delta1 + ------- - -------
              2     2 f1
          2 f1
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta2Tilde, 2]
:[font = output; output; inactive; preserveAspect; endGroup]
delta2 - delta2^2/(2*f1)
;[o]
               2
         delta2
delta2 - -------
          2 f1
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta2Tilde, 3]
:[font = output; output; inactive; preserveAspect; endGroup]
(delta2*(delta2^2 - delta2*f1 + 2*f1^2 + 
 
      2*delta2*gamma1 - 2*delta2*f1*gamma1 - 
 
      2*delta2*gamma2 + 2*delta2*f1*gamma2))/(2*f1^2)
;[o]
               2                   2
(delta2 (delta2  - delta2 f1 + 2 f1  + 
 
      2 delta2 gamma1 - 2 delta2 f1 gamma1 - 
 
                                                    2
      2 delta2 gamma2 + 2 delta2 f1 gamma2)) / (2 f1 )
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta3Tilde, 2]
:[font = output; output; inactive; preserveAspect; endGroup]
delta3 - delta3^2/(2*f1)
;[o]
               2
         delta3
delta3 - -------
          2 f1
:[font = input; preserveAspect; startGroup]
WeakOrtho[delta3Tilde, 3]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(delta3*(delta3^2 + 2*delta3*epsilon2 - delta3*f1 - 
 
      2*delta3*epsilon2*f1 + 2*f1^2 - 
 
      2*delta3*gamma2 + 2*delta3*f1*gamma2))/(2*f1^2)
;[o]
               2
(delta3 (delta3  + 2 delta3 epsilon2 - delta3 f1 - 
 
                                 2
      2 delta3 epsilon2 f1 + 2 f1  - 2 delta3 gamma2 + 
 
                                  2
      2 delta3 f1 gamma2)) / (2 f1 )
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
The isotropic limit
:[font = text; inactive; preserveAspect]
Taking  the second argument of WeakOrtho as zero, gives isotropic results:
:[font = input; preserveAspect; startGroup]
WeakOrtho[standardTrace, 0]
:[font = output; output; inactive; preserveAspect; endGroup]
3*rho*(VP0^2 + VS0^2)
;[o]
          2      2
3 rho (VP0  + VS0 )
:[font = input; preserveAspect; startGroup]
WeakOrtho[canonicalTrace, 0]
:[font = output; output; inactive; preserveAspect; endGroup]
(6 - 3*f1)*rho*VP0^2
;[o]
                  2
(6 - 3 f1) rho VP0
:[font = input; preserveAspect; startGroup]
%//UseVS0//WeakOrtho[#,0]&
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
3*rho*(VP0^2 + VS0^2)
;[o]
          2      2
3 rho (VP0  + VS0 )
:[font = section; inactive; preserveAspect; startGroup]
TI Limits
:[font = subsection; inactive; preserveAspect; startGroup]
VTI
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
 VoigtVTI
:[font = text; inactive; preserveAspect]
VTI media require only 5 of the 21 hookean constants.  In Voigt notation these are C11, C33,  C55, C66,  and C13.   However, the user can enter C44 (commonly used) or other Cij in expressions:
:[font = input; preserveAspect; startGroup]
?VoigtVTI
:[font = print; inactive; preserveAspect; endGroup]
VoigtVTI[expression]

 Reduce expressions in Cij to the canonical VTI set,
   C11, C33, C55, C66, C13. 
:[font = input; preserveAspect; startGroup]
VoigtVTI[C14+C24+C34+C44+C54+C64]
:[font = output; output; inactive; preserveAspect; endGroup]
C55
;[o]
C55
:[font = input; preserveAspect; startGroup]
VoigtVTI[C11+C12+C13]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
2*C11 + C13 - 2*C66
;[o]
2 C11 + C13 - 2 C66
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
VTILimit
:[font = text; inactive; preserveAspect; rightWrapOffset = 577]
This function assumes the VTI symmetry:
     gamma1 = gamma2, delta1 = delta2, epsilon1 = epsilon2, delta3 = 0.
Results are expressed in gamma2, delta2, epsilon2 instead of the pure
Thomsen notation, gamma, delta, epsilon, so that the other functions
in the package can work on the results (as in the last example below).

If your problem really only involves VTI, you should use the VTI.m package.
:[font = input; preserveAspect; startGroup]
?VTILimit
:[font = print; inactive; preserveAspect; endGroup]
VTILimit[expression]

 Convert expression in Thomsen-Tsvankin parameters to
   Thomsen parameters assuming VTI symmetry. 
:[font = input; preserveAspect; startGroup]
canonicalTrace
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 2*epsilon1 + 2*epsilon2 - 3*f1 + 2*gammaT - 
 
    2*f1*gammaT + 2*gamma2 - 2*f1*gamma2)*rho*VP0^2
;[o]
(6 + 2 epsilon1 + 2 epsilon2 - 3 f1 + 2 gammaT - 
 
                                                 2
    2 f1 gammaT + 2 gamma2 - 2 f1 gamma2) rho VP0
:[font = input; preserveAspect; startGroup]
canonicalTrace//VTILimit
:[font = output; output; inactive; preserveAspect; endGroup]
(6 + 4*epsilon2 - 3*f2 + 2*gamma2 - 2*f2*gamma2)*rho*
 
  VP0^2
;[o]
(6 + 4 epsilon2 - 3 f2 + 2 gamma2 - 2 f2 gamma2) rho 
 
     2
  VP0
:[font = input; preserveAspect; startGroup]
standardTrace//VTILimit
:[font = output; output; inactive; preserveAspect; endGroup]
rho*(3*VP0^2 + 4*epsilon2*VP0^2 + 3*VS0^2 + 
 
    2*gamma2*VS0^2)
;[o]
          2                 2        2
rho (3 VP0  + 4 epsilon2 VP0  + 3 VS0  + 
 
                2
    2 gamma2 VS0 )
:[font = input; preserveAspect; startGroup]
%//UseF2
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
(6 + 4*epsilon2 - 3*f2 + 2*gamma2 - 2*f2*gamma2)*rho*
 
  VP0^2
;[o]
(6 + 4 epsilon2 - 3 f2 + 2 gamma2 - 2 f2 gamma2) rho 
 
     2
  VP0
:[font = subsection; inactive; preserveAspect]
HTI (Not Ready)
:[font = subsection; inactive; preserveAspect; endGroup]
HTI2 (Not Ready)
:[font = section; inactive; preserveAspect; startGroup]
Applications (Not Ready)
:[font = text; inactive; preserveAspect]
True geophysical applications will go into separate notebooks whose names and descriptions will go here.
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Weak EigenAnalysis (see WeakOrtho.ma)
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Christoffel Matrix
:[font = text; inactive; preserveAspect; endGroup]
%%% Use WeakOrtho, etc.
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
VTI EigenAnalysis
:[font = text; inactive; preserveAspect; rightWrapOffset = 527; endGroup]
%%% epsilon2->epsilon1, delta2->delta1, delta3->0, gammaS->0
:[font = subsubsection; inactive; preserveAspect]
Eigenvalues and Eigenvectors
:[font = subsubsection; inactive; preserveAspect]
Weak Ortho Phase Speeds
:[font = subsubsection; inactive; preserveAspect]
Weak Ortho Polarizations
:[font = subsubsection; inactive; preserveAspect]
Weak Ortho Group Velocities
:[font = subsubsection; inactive; preserveAspect]
Weak Results Using Ray Parameter
:[font = subsubsection; inactive; preserveAspect]
HTI EigenAnalysis
:[font = subsubsection; inactive; preserveAspect; endGroup; endGroup; endGroup]
HTI2 EigenAnalysis
^*)