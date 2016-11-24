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
	fontset = subtitle, inactive, noPageBreakBelow, noPageBreakInGroup, nohscroll, preserveAspect, groupLikeTitle, center, M7, bold, e6,  36, fontName, "times";
	fontset = subsubtitle, inactive, noPageBreakBelow, noPageBreakInGroup, nohscroll, preserveAspect, groupLikeTitle, center, M7, italic, e6,  24, fontName, "times";
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
VTI SH Mode
:[font = subtitle; inactive; preserveAspect; fontSize = 24]
Using the q(p) method
:[font = subsubtitle; inactive; preserveAspect; fontSize = 18]
Copyright, Jack K. Cohen, 1996, All Rights Reserved
:[font = input; initialization; preserveAspect]
*)
<<VTI.m
Off[General::spell, General::spell1]
(*
:[font = text; inactive; preserveAspect]
This notebook is an application notebook.  For basic usage of the VTI package, see VTI.ma.  Here, we exercise the 2D q(p) approach to determining moveout, phase and group velocities as functions of ray parameter, p, for the quasi-P mode.  This notebook contains the results of my "Analytic study of the effective parameters for determination of the nmo velocity function in transversely isotropic media"  paper, CWP-191P.
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
The Theory
:[font = subsection; inactive; preserveAspect; rightWrapOffset = 554; startGroup]
Moveout function as a function of ray parameter
:[font = text; inactive; preserveAspect; endGroup]
In terms of the eigenvalues, lambda, of the matrix,
	M = Cijkl pj pl,
where pi denotes the ith component of the slowness vector,
the eikonal equation is lambda = rho.  For propagation in
a symmetry plane, the {p1,p2,p3} vector becomes {p,0,q}
and the eikonal equation is solved to get q = q(p).  Then, the Vnmo normal moveout function is determined as
	Vnmo^2(p) = q''/(pq' - q),
where the prime denotes d/dp.
:[font = subsection; inactive; preserveAspect; rightWrapOffset = 536; startGroup]
Phase speed as a function of ray parameter
:[font = text; inactive; preserveAspect]
Unlike the earlier Tsvankin and Cohen approaches, phase velocity, v, isn't required to compute the moveout function.  However, it is determined by q(p) via the formula:
	v^2(p) = 1/(p^2 + q(p)^2).
Hence we can get phase velocity results if we want them. 

Also, we can get the Cohen expression for Vnmo in terms of phase velocity, which is the equivalent of the Tsvankin formula involving phase velocity as a function of phase angle from which it was earlier  derived:
:[font = input; preserveAspect; startGroup]
?VNMOsq
:[font = print; inactive; preserveAspect; endGroup]
VNMOsq[Q[p], {p}]

 Computes the moveout function in a symmetry plane
   as a function of ray parameter, given the
   vertical slowness, Q[p]. 
:[font = input; preserveAspect; startGroup]
Q[p_] = Sqrt[1/v[p]^2 - p^2];
VNMOsq[Q[p], {p}]//Together
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(-v[p]^4 - 2*p*v[p]^3*Derivative[1][v][p] + 
 
    2*Derivative[1][v][p]^2 - 
 
    3*p^2*v[p]^2*Derivative[1][v][p]^2 - 
 
    v[p]*Derivative[2][v][p] + 
 
    p^2*v[p]^3*Derivative[2][v][p])/
 
  (v[p]*(-1 + p^2*v[p]^2)*
 
    (v[p] + p*Derivative[1][v][p]))
;[o]
      4           3                2
(-v[p]  - 2 p v[p]  v'[p] + 2 v'[p]  - 
 
       2     2      2
    3 p  v[p]  v'[p]  - v[p] v''[p] + 
 
     2     3
    p  v[p]  v''[p]) / 
 
               2     2
  (v[p] (-1 + p  v[p] ) (v[p] + p v'[p]))
:[font = subsection; inactive; preserveAspect; rightWrapOffset = 537; startGroup]
Group velocity as a function of ray parameter
:[font = text; inactive; preserveAspect; endGroup]
Group velocity is given in terms of the eigenvalue lambda[pvec] (discussed above) as:
	g = 1/2rho * Gradp lambda
	   = 1/2rho * (d lambda/dp, d lambda/dq)
	where q is evaluated at q = Q[p].
:[font = subsection; inactive; preserveAspect; rightWrapOffset = 576; startGroup]
Polarization as a function of ray parameter
:[font = text; inactive; preserveAspect; endGroup]
The polarization is the eigenvector corresponding to lambda.  The eigenvectors can be taken as an orthonormal basis of R3.
:[font = subsection; inactive; preserveAspect; rightWrapOffset = 537; startGroup]
Phase angle representations
:[font = text; inactive; preserveAspect; endGroup; endGroup]
The phase speed is simply v(theta) = Sqrt[lambda[nvec]/rho] where in lambda, the slowness vector is replaced by a unit vector.   The group velocity and moveout velocity are obtained in terms of phase angle via the relation: p = sin(theta)/v(theta).
:[font = section; inactive; initialization; preserveAspect; startGroup]
Initialization
:[font = subsection; inactive; preserveAspect; startGroup]
Rules
:[font = input; initialization; preserveAspect; endGroup]
*)
pRules2D = {p1->p,p2->0,p3->q};
nRules2D = {p->Sin[theta],q->Cos[theta]};
zRule = p -> Sqrt[z]/VP0;
ATRules = {
	epsilon -> delta + eta(1+2delta),
	VP0->Vnmo0/Sqrt[1+2 delta],
	p -> Sqrt[y]/Vnmo0
};
(*
:[font = subsection; inactive; preserveAspect; startGroup]
EigenAnalysis of VTI Matrix
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Eigenvalue
:[font = input; initialization; preserveAspect]
*)
MVTI =
(C11 p1^2 + C66 p2^2 + C55 p3^2)ii +
(C11 p2^2 + C55 p3^2 + C66 p1^2)jj +
(C33 p3^2 + C55(p1^2 + p2^2))kk +
(C11 - C66)p1 p2 (ij+ji) +
(C13 + C55)p2 p3 (jk+kj) +
(C13 + C55)p3 p1 (ki+ik);
MVTI2D=MVTI/.pRules2D;
{evalVTI, evecVTI} = Eigensystem@MVTI2D;
(*
:[font = text; inactive; preserveAspect]
Check which entry is QP mode:
:[font = input; preserveAspect; startGroup]
evalVTI[[3]]
:[font = output; output; inactive; preserveAspect; endGroup]
(C11*p^2 + C55*p^2 + C33*q^2 + C55*q^2 + 
 
    (C11^2*p^4 - 2*C11*C55*p^4 + C55^2*p^4 + 
 
       4*C13^2*p^2*q^2 - 2*C11*C33*p^2*q^2 + 
 
       2*C11*C55*p^2*q^2 + 8*C13*C55*p^2*q^2 + 
 
       2*C33*C55*p^2*q^2 + 2*C55^2*p^2*q^2 + 
 
       C33^2*q^4 - 2*C33*C55*q^4 + C55^2*q^4)^(1/2))/2
;[o]
      2        2        2        2
(C11 p  + C55 p  + C33 q  + C55 q  + 
 
            2  4              4      2  4
    Sqrt[C11  p  - 2 C11 C55 p  + C55  p  + 
 
           2  2  2              2  2
      4 C13  p  q  - 2 C11 C33 p  q  + 
 
                 2  2              2  2
      2 C11 C55 p  q  + 8 C13 C55 p  q  + 
 
                 2  2        2  2  2      2  4
      2 C33 C55 p  q  + 2 C55  p  q  + C33  q  - 
 
                 4      2  4
      2 C33 C55 q  + C55  q ]) / 2
:[font = input; initialization; preserveAspect; startGroup]
*)
evalQP = evalVTI[[3]]//ConvertCanonical//UseDelta
(*
:[font = output; output; inactive; preserveAspect; endGroup]
p^2*rho*VP0^2 + 
 
  epsilon*p^2*rho*VP0^2 - 
 
  (f*p^2*rho*VP0^2)/2 + 
 
  q^2*rho*VP0^2 - 
 
  (f*q^2*rho*VP0^2)/2 + 
 
  ((4*epsilon^2*p^4 + 
 
        4*epsilon*f*p^4 + f^2*p^4 + 
 
        8*delta*f*p^2*q^2 - 
 
        4*epsilon*f*p^2*q^2 + 
 
        2*f^2*p^2*q^2 + f^2*q^4)*
 
      rho^2*VP0^4)^(1/2)/2
;[o]
 2        2            2        2
p  rho VP0  + epsilon p  rho VP0  - 
 
     2        2
  f p  rho VP0     2        2
  ------------- + q  rho VP0  - 
        2
 
     2        2
  f q  rho VP0
  ------------- + 
        2
 
                 2  4
  Sqrt[(4 epsilon  p  + 
 
                    4    2  4
       4 epsilon f p  + f  p  + 
 
                  2  2
       8 delta f p  q  - 
 
                    2  2
       4 epsilon f p  q  + 
 
          2  2  2    2  4     2    4
       2 f  p  q  + f  q ) rho  VP0
 
     ] / 2
:[font = input; preserveAspect; startGroup]
{a,b,c} = IsolateSqrt[%]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{((2*p^2 + 2*epsilon*p^2 - f*p^2 + 2*q^2 - f*q^2)*rho*
 
     VP0^2)/2, 1/2, (4*epsilon^2*p^4 + 
 
     4*epsilon*f*p^4 + f^2*p^4 + 8*delta*f*p^2*q^2 - 
 
     4*epsilon*f*p^2*q^2 + 2*f^2*p^2*q^2 + f^2*q^4)*
 
   rho^2*VP0^4}
;[o]
     2              2      2      2      2         2
 (2 p  + 2 epsilon p  - f p  + 2 q  - f q ) rho VP0
{---------------------------------------------------, 
                          2
 
  1            2  4                4    2  4
  -, (4 epsilon  p  + 4 epsilon f p  + f  p  + 
  2
 
                2  2                2  2
     8 delta f p  q  - 4 epsilon f p  q  + 
 
        2  2  2    2  4     2    4
     2 f  p  q  + f  q ) rho  VP0 }
:[font = input; preserveAspect; startGroup]
CoefficientList[2a/(rho VP0^2),f]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{2*(p^2 + epsilon*p^2 + q^2), -p^2 - q^2}
;[o]
     2            2    2     2    2
{2 (p  + epsilon p  + q ), -p  - q }
:[font = input; preserveAspect; startGroup]
CoefficientList[c/(rho^2 VP0^4),f]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{4*epsilon^2*p^4, 4*p^2*
 
   (epsilon*p^2 + 2*delta*q^2 - epsilon*q^2), 
 
  (p^2 + q^2)^2}
;[o]
          2  4     2           2            2
{4 epsilon  p , 4 p  (epsilon p  + 2 delta q  - 
 
              2     2    2 2
     epsilon q ), (p  + q ) }
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Eigenvector
:[font = input; preserveAspect; startGroup]
evecVTI[[3]]
:[font = output; output; inactive; preserveAspect; endGroup]
{-(-(C11*p^2) + C55*p^2 + C33*q^2 - C55*q^2 - 
 
      (C11^2*p^4 - 2*C11*C55*p^4 + C55^2*p^4 + 
 
         4*C13^2*p^2*q^2 - 2*C11*C33*p^2*q^2 + 
 
         2*C11*C55*p^2*q^2 + 8*C13*C55*p^2*q^2 + 
 
         2*C33*C55*p^2*q^2 + 2*C55^2*p^2*q^2 + 
 
         C33^2*q^4 - 2*C33*C55*q^4 + C55^2*q^4)^(1/2))
 
    /(2*(C13 + C55)*p*q), 0, 1}
;[o]
          2         2        2        2
{-(-(C11 p ) + C55 p  + C33 q  - C55 q  - 
 
              2  4              4      2  4
      Sqrt[C11  p  - 2 C11 C55 p  + C55  p  + 
 
             2  2  2              2  2
        4 C13  p  q  - 2 C11 C33 p  q  + 
 
                   2  2              2  2
        2 C11 C55 p  q  + 8 C13 C55 p  q  + 
 
                   2  2        2  2  2      2  4
        2 C33 C55 p  q  + 2 C55  p  q  + C33  q  - 
 
                   4      2  4
        2 C33 C55 q  + C55  q ]) / (2 (C13 + C55) p q)
 
   , 0, 1}
:[font = input; initialization; preserveAspect; startGroup]
*)
evecQP = evecVTI[[3]]//ConvertCanonical
(*
:[font = output; output; inactive; preserveAspect; endGroup]
{(2*epsilon*p^2*rho*VP0^2 + f*p^2*rho*VP0^2 - 
 
     f*q^2*rho*VP0^2 + 
 
     ((4*epsilon^2*p^4 + 4*epsilon*f*p^4 + f^2*p^4 + 
 
          4*deltaTilde^2*p^2*q^2 + 
 
          8*deltaTilde*f*p^2*q^2 - 
 
          4*epsilon*f*p^2*q^2 + 2*f^2*p^2*q^2 + f^2*q^4
 
          )*rho^2*VP0^4)^(1/2))/
 
   (2*(deltaTilde + f)*p*q*rho*VP0^2), 0, 1}
;[o]
             2        2      2        2
{(2 epsilon p  rho VP0  + f p  rho VP0  - 
 
        2        2                  2  4
     f q  rho VP0  + Sqrt[(4 epsilon  p  + 
 
                      4    2  4
         4 epsilon f p  + f  p  + 
 
                     2  2  2                   2  2
         4 deltaTilde  p  q  + 8 deltaTilde f p  q  - 
 
                      2  2      2  2  2    2  4     2
         4 epsilon f p  q  + 2 f  p  q  + f  q ) rho  
 
          4                                    2
       VP0 ]) / (2 (deltaTilde + f) p q rho VP0 ), 0, 
 
  1}
:[font = input; preserveAspect; startGroup]
{a,b,c} = IsolateSqrt@Numerator@evecQP[[1]]
:[font = output; output; inactive; preserveAspect; endGroup]
{2*epsilon*p^2*rho*VP0^2 + f*p^2*rho*VP0^2 - 
 
   f*q^2*rho*VP0^2, 1, 
 
  (4*epsilon^2*p^4 + 4*epsilon*f*p^4 + f^2*p^4 + 
 
     4*deltaTilde^2*p^2*q^2 + 8*deltaTilde*f*p^2*q^2 - 
 
     4*epsilon*f*p^2*q^2 + 2*f^2*p^2*q^2 + f^2*q^4)*
 
   rho^2*VP0^4}
;[o]
            2        2      2        2
{2 epsilon p  rho VP0  + f p  rho VP0  - 
 
      2        2               2  4                4
   f q  rho VP0 , 1, (4 epsilon  p  + 4 epsilon f p  + 
 
      2  4               2  2  2
     f  p  + 4 deltaTilde  p  q  + 
 
                     2  2                2  2
     8 deltaTilde f p  q  - 4 epsilon f p  q  + 
 
        2  2  2    2  4     2    4
     2 f  p  q  + f  q ) rho  VP0 }
:[font = text; inactive; preserveAspect]
Write it as tan(gamma) = (A + Sqrt[B])/C
:[font = input; preserveAspect; startGroup]
polC = Denominator@evecQP[[1]]/(rho VP0^2)
:[font = output; output; inactive; preserveAspect; endGroup]
2*(deltaTilde + f)*p*q
;[o]
2 (deltaTilde + f) p q
:[font = input; preserveAspect; startGroup]
polA = {1,f}.
	(CoefficientList[a/(rho VP0^2),f]//Simplify)
:[font = output; output; inactive; preserveAspect; endGroup]
2*epsilon*p^2 + f*(p^2 - q^2)
;[o]
           2       2    2
2 epsilon p  + f (p  - q )
:[font = input; preserveAspect; startGroup]
polB = {1,f,f^2}.
	(CoefficientList[c/(rho^2 VP0^4),f]//Simplify)
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup; endGroup]
f^2*(p^2 + q^2)^2 + 4*p^2*
 
   (epsilon^2*p^2 + deltaTilde^2*q^2) + 
 
  4*f*p^2*(epsilon*p^2 + 2*deltaTilde*q^2 - 
 
     epsilon*q^2)
;[o]
 2   2    2 2      2         2  2             2  2
f  (p  + q )  + 4 p  (epsilon  p  + deltaTilde  q ) + 
 
       2           2                 2            2
  4 f p  (epsilon p  + 2 deltaTilde q  - epsilon q )
:[font = section; inactive; preserveAspect; startGroup]
q[p]
:[font = subsection; inactive; preserveAspect; startGroup]
General Expression
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
q in nice format
:[font = text; inactive; preserveAspect]
Recall that the eigenvalue equation is lambda = rho.
:[font = input; initialization; preserveAspect; startGroup]
*)
evaltmp1 = HalfPower[HalfPower[evalQP,p,psq],q,qsq]
(*
:[font = output; output; inactive; preserveAspect; endGroup]
psq*rho*VP0^2 + epsilon*psq*rho*VP0^2 - 
 
  (f*psq*rho*VP0^2)/2 + qsq*rho*VP0^2 - 
 
  (f*qsq*rho*VP0^2)/2 + 
 
  ((4*epsilon^2*psq^2 + 4*epsilon*f*psq^2 + 
 
        f^2*psq^2 + 8*delta*f*psq*qsq - 
 
        4*epsilon*f*psq*qsq + 2*f^2*psq*qsq + f^2*qsq^2
 
        )*rho^2*VP0^4)^(1/2)/2
;[o]
                                                   2
           2                      2   f psq rho VP0
psq rho VP0  + epsilon psq rho VP0  - -------------- + 
                                            2
 
                              2
             2   f qsq rho VP0
  qsq rho VP0  - -------------- + 
                       2
 
                 2    2                  2    2    2
  Sqrt[(4 epsilon  psq  + 4 epsilon f psq  + f  psq  + 
 
       8 delta f psq qsq - 4 epsilon f psq qsq + 
 
          2            2    2     2    4
       2 f  psq qsq + f  qsq ) rho  VP0 ] / 2
:[font = text; inactive; preserveAspect]
We see that  rho VP0^2/2 is a common factor, taking it out makes the right hand side of the eigenvalue equation equal to 2/VP0^2; define k = 1/VP0^2 temporarily.
:[font = input; initialization; preserveAspect; startGroup]
*)
evaltmp2= evaltmp1/. rho -> 2/VP0^2
(*
:[font = output; output; inactive; preserveAspect; endGroup]
2*psq + 2*epsilon*psq - f*psq + 2*qsq - f*qsq + 
 
  (4*epsilon^2*psq^2 + 4*epsilon*f*psq^2 + f^2*psq^2 + 
 
     8*delta*f*psq*qsq - 4*epsilon*f*psq*qsq + 
 
     2*f^2*psq*qsq + f^2*qsq^2)^(1/2)
;[o]
2 psq + 2 epsilon psq - f psq + 2 qsq - f qsq + 
 
                2    2                  2    2    2
  Sqrt[4 epsilon  psq  + 4 epsilon f psq  + f  psq  + 
 
    8 delta f psq qsq - 4 epsilon f psq qsq + 
 
       2            2    2
    2 f  psq qsq + f  qsq ]
:[font = input; initialization; preserveAspect]
*)
solnRules = Solve[% == 2k,qsq];
(*
:[font = text; inactive; preserveAspect]
Test which is the appropriate root:
:[font = input; preserveAspect; startGroup]
solnRules/. {psq->0, k->1/VP0^2}//UseVS0//
PowerExpand//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{{qsq -> VS0^(-2)}, {qsq -> VP0^(-2)}}
;[o]
            -2              -2
{{qsq -> VS0  }, {qsq -> VP0  }}
:[font = text; inactive; preserveAspect]
Since V(p) = 1/Sqrt[p^2 + q^2], we have V(0) = 1/q, thus the second root is the one we want.
:[font = input; initialization; preserveAspect; startGroup]
*)
Qsq = qsq/.First@Rest@solnRules/.
	{psq->p^2, k->1/VP0^2}//Together//Simplify;
Q = Qsq//Sqrt;
Qsqz = Qsq/. zRule//Simplify
(*
:[font = output; output; inactive; preserveAspect; endGroup]
(-2 + f + 2*z + 2*epsilon*z - 2*f*z - 2*delta*f*z + 
 
    VP0^2*((f^2 + 8*delta*f*z - 4*epsilon*f*z - 
 
          4*delta*f^2*z + 4*epsilon^2*z^2 - 
 
          8*delta*f*z^2 + 8*epsilon*f*z^2 - 
 
          8*delta*epsilon*f*z^2 + 8*delta*f^2*z^2 + 
 
          4*delta^2*f^2*z^2 - 8*epsilon*f^2*z^2)/VP0^4)
 
       ^(1/2))/(2*(-1 + f)*VP0^2)
;[o]
(-2 + f + 2 z + 2 epsilon z - 2 f z - 2 delta f z + 
 
       2        2
    VP0  Sqrt[(f  + 8 delta f z - 4 epsilon f z - 
 
                  2              2  2              2
         4 delta f  z + 4 epsilon  z  - 8 delta f z  + 
 
                      2                      2
         8 epsilon f z  - 8 delta epsilon f z  + 
 
                  2  2          2  2  2
         8 delta f  z  + 4 delta  f  z  - 
 
                    2  2       4                    2
         8 epsilon f  z ) / VP0 ]) / (2 (-1 + f) VP0 )
:[font = input; preserveAspect; startGroup]
{a,b,c} = IsolateSqrt[2(1-f)VP0^2 Qsqz]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{2 - f - 2*z - 2*epsilon*z + 2*f*z + 2*delta*f*z, 
 
  -VP0^2, (f^2 + 8*delta*f*z - 4*epsilon*f*z - 
 
     4*delta*f^2*z + 4*epsilon^2*z^2 - 
 
     8*delta*f*z^2 + 8*epsilon*f*z^2 - 
 
     8*delta*epsilon*f*z^2 + 8*delta*f^2*z^2 + 
 
     4*delta^2*f^2*z^2 - 8*epsilon*f^2*z^2)/VP0^4}
;[o]
{2 - f - 2 z - 2 epsilon z + 2 f z + 2 delta f z, 
 
      2    2
  -VP0 , (f  + 8 delta f z - 4 epsilon f z - 
 
              2              2  2              2
     4 delta f  z + 4 epsilon  z  - 8 delta f z  + 
 
                  2                      2
     8 epsilon f z  - 8 delta epsilon f z  + 
 
              2  2          2  2  2              2  2
     8 delta f  z  + 4 delta  f  z  - 8 epsilon f  z )
 
          4
     / VP0 }
:[font = input; preserveAspect; startGroup]
a//Simplify//Collect[#,z]&
:[font = output; output; inactive; preserveAspect; endGroup]
2 - f + (-2 - 2*epsilon + 2*f + 2*delta*f)*z
;[o]
2 - f + (-2 - 2 epsilon + 2 f + 2 delta f) z
:[font = input; preserveAspect; startGroup]
CoefficientList[%,z]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{2 - f, 2*(-1 - epsilon + f + delta*f)}
;[o]
{2 - f, 2 (-1 - epsilon + f + delta f)}
:[font = input; preserveAspect; startGroup]
B = c VP0^4//Simplify//Collect[#,z]&
:[font = output; output; inactive; preserveAspect; endGroup]
f^2 + (8*delta*f - 4*epsilon*f - 4*delta*f^2)*z + 
 
  (4*epsilon^2 - 8*delta*f + 8*epsilon*f - 
 
     8*delta*epsilon*f + 8*delta*f^2 + 
 
     4*delta^2*f^2 - 8*epsilon*f^2)*z^2
;[o]
 2                                       2
f  + (8 delta f - 4 epsilon f - 4 delta f ) z + 
 
            2
  (4 epsilon  - 8 delta f + 8 epsilon f - 
 
                                  2          2  2
     8 delta epsilon f + 8 delta f  + 4 delta  f  - 
 
                2   2
     8 epsilon f ) z
:[font = input; preserveAspect; startGroup]
CoefficientList[B,z]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{f^2, 4*f*(2*delta - epsilon - delta*f), 
 
  4*(epsilon^2 - 2*delta*f + 2*epsilon*f - 
 
     2*delta*epsilon*f + 2*delta*f^2 + delta^2*f^2 - 
 
     2*epsilon*f^2)}
;[o]
  2
{f , 4 f (2 delta - epsilon - delta f), 
 
            2
  4 (epsilon  - 2 delta f + 2 epsilon f - 
 
                                  2        2  2
     2 delta epsilon f + 2 delta f  + delta  f  - 
 
                2
     2 epsilon f )}
:[font = input; preserveAspect; startGroup]
Coefficient[B,z^2] - 4(epsilon - f delta)^2//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
8*(delta - epsilon)*(-1 + f)*f
;[o]
8 (delta - epsilon) (-1 + f) f
:[font = text; inactive; preserveAspect; rightWrapOffset = 563]
Hence, q^2(p) = (a - sqrt(B))/(2(1-f)VP0^2) = (a - sqrt(B))/(2VS0^2)
where B is the same quantity as appeared in the paper in the formula for v^2(p).
:[font = input; preserveAspect; startGroup]
Sqrt[(a - Sqrt[B])/(2(1-f))]/VP0;
%/.z->0//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
(2 - 2*f)^(1/2)/(2^(1/2)*(1 - f)^(1/2)*VP0)
;[o]
     Sqrt[2 - 2 f]
-----------------------
Sqrt[2] Sqrt[1 - f] VP0
:[font = input; preserveAspect; startGroup]
%//UseVS0
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VP0^(-1)
;[o]
 1
---
VP0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Phase velocity in nice format
:[font = text; inactive; preserveAspect]
Belongs  in the v[p] section, but I need to check the equation in the paper before proceeding.  So here is a direct derivation from q:
:[font = text; inactive; preserveAspect; rightWrapOffset = 552]

q^2(p) = (a - sqrt(B))/(2(1-f)VP0^2)

Thus,
1/v^2(p) = p^2 + q^2(p) = z/VP0^2 + q^2(p)
	= 1/VP0^2 [ (a + 2(1-f)z) - sqrt(B)]/(1-f)

And
v^2(p) = VP0^2 *2(1-f)((a + 2(1-f)z + sqrt(B))/ (a + 2(1-f)z) - B))

So
v^2(p) = (A + Sqrt[B])/(2C), 
where
	A = a + 2(1-f)z ,
	B = as in paper (see above),
	C = (a + 2(1-f)z) - B)/(4(1-f))
:[font = text; inactive; preserveAspect]
First get A:
:[font = input; preserveAspect; startGroup]
a + 2(1-f)z//Simplify//Collect[#,z]&
:[font = output; output; inactive; preserveAspect; endGroup]
2 - f + (-2*epsilon + 2*delta*f)*z
;[o]
2 - f + (-2 epsilon + 2 delta f) z
:[font = input; preserveAspect; startGroup]
CoefficientList[%,z]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{2 - f, 2*(-epsilon + delta*f)}
;[o]
{2 - f, 2 (-epsilon + delta f)}
:[font = text; inactive; preserveAspect]
And then C:
:[font = input; preserveAspect; startGroup]
((a + 2(1-f)z)^2 - B)/(4(1-f))//
Simplify//Collect[#,z]&
:[font = output; output; inactive; preserveAspect; endGroup]
1 - 2*epsilon*z + (2*delta*f - 2*epsilon*f)*z^2
;[o]
                                             2
1 - 2 epsilon z + (2 delta f - 2 epsilon f) z
:[font = input; preserveAspect; startGroup]
CoefficientList[%,z]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{1, -2*epsilon, 2*(delta - epsilon)*f}
;[o]
{1, -2 epsilon, 2 (delta - epsilon) f}
:[font = text; inactive; preserveAspect; endGroup; endGroup]
Both A and C agree with paper and B was verified earlier.
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Weak  TI regime
:[font = input; preserveAspect; startGroup]
weakQ = Q//WeakVTI//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
(1 - p^2*VP0^2)^(1/2)/VP0 + 
 
  ((1 - f)*p^2*VP0*(delta - delta*p^2*VP0^2 + 
 
       epsilon*p^2*VP0^2))/
 
   ((-1 + f)*(1 - p^2*VP0^2)^(1/2))
;[o]
          2    2
Sqrt[1 - p  VP0 ]             2
----------------- + ((1 - f) p  VP0 
       VP0
 
                     2    2            2    2
     (delta - delta p  VP0  + epsilon p  VP0 )) / 
 
                       2    2
   ((-1 + f) Sqrt[1 - p  VP0 ])
:[font = input; preserveAspect; startGroup]
weakQz = weakQ/.zRule//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(1 - z - delta*z + delta*z^2 - epsilon*z^2)/
 
  (VP0*(1 - z)^(1/2))
;[o]
                         2            2
1 - z - delta z + delta z  - epsilon z
---------------------------------------
            VP0 Sqrt[1 - z]
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Moderate  TI regime
:[font = input; preserveAspect]
moderateQ = Q//WeakVTI[#,2]&//PowerExpand//Simplify;
:[font = input; preserveAspect; startGroup]
moderateQz = moderateQ/.zRule//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
(1 - z)^(1/2)/VP0 + (z*
 
     (-delta + delta*z - epsilon*z))/
 
   (VP0*(1 - z)^(1/2)) + 
 
  ((1 - z)^(1/2)*(-((z^2*
 
            (delta - delta*z + epsilon*z)^2)/
 
          (-1 + z)^2) + 
 
       (4*(-delta + epsilon)*z^2*
 
          (delta - delta*z + epsilon*z)*(1 - z + f*z))
 
         /(-f + f*z)))/(2*VP0)
;[o]
Sqrt[1 - z]   z (-delta + delta z - epsilon z)
----------- + -------------------------------- + 
    VP0               VP0 Sqrt[1 - z]
 
  (Sqrt[1 - z] (-(
 
           2                              2
          z  (delta - delta z + epsilon z)
          ---------------------------------) + 
                              2
                      (-1 + z)
 
                              2
       (4 (-delta + epsilon) z  
 
          (delta - delta z + epsilon z) (1 - z + f z))
 
          / (-f + f z))) / (2 VP0)
:[font = input; preserveAspect; startGroup]
moderateQz - weakQz//Together//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
(z^2*(delta - delta*z + epsilon*z)*
 
    (4*delta - 4*epsilon - delta*f - 8*delta*z + 
 
      8*epsilon*z + 5*delta*f*z - 5*epsilon*f*z + 
 
      4*delta*z^2 - 4*epsilon*z^2 - 4*delta*f*z^2 + 
 
      4*epsilon*f*z^2))/(2*f*VP0*(1 - z)^(3/2))
;[o]
  2
(z  (delta - delta z + epsilon z) 
 
    (4 delta - 4 epsilon - delta f - 8 delta z + 
 
      8 epsilon z + 5 delta f z - 5 epsilon f z + 
 
               2              2              2
      4 delta z  - 4 epsilon z  - 4 delta f z  + 
 
                   2                     3/2
      4 epsilon f z )) / (2 f VP0 (1 - z)   )
:[font = input; preserveAspect]
-% (2f VP0 (1-z)^(3/2))/
	(z^2(delta - delta z + epsilon z));
:[font = input; preserveAspect; startGroup]
CoefficientList[%,z]/.epsilon->delta+weaketa//
Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{delta*f + 4*weaketa, (-8 + 5*f)*weaketa, 
 
  4*(1 - f)*weaketa}
;[o]
{delta f + 4 weaketa, (-8 + 5 f) weaketa, 
 
  4 (1 - f) weaketa}
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Small p results
:[font = input; preserveAspect]
qSmallp[p_,n_:1] := Series[Q, {p,0,n}]//
Simplify//PowerExpand//Simplify
:[font = text; inactive; preserveAspect]
Need UseVS0 to dump a lot of Sqrt[1-f]'s that won't cancel
:[font = input; preserveAspect; startGroup]
qSmallp5 = qSmallp[p,5]//UseVS0//PowerExpand//UseF
:[font = output; output; inactive; preserveAspect; endGroup]
SeriesData[p, 0, {VP0^(-1), 0, -((1 + 2*delta)*VP0)/2, 
 
   0, ((16*delta^2 - 16*delta*epsilon - f + 
 
        4*delta*f - 4*delta^2*f - 8*epsilon*f)*VP0^3)/
 
    (8*f)}, 0, 6, 1]
;[o]
                         2
 1    (1 + 2 delta) VP0 p
--- - -------------------- + 
VP0            2
 
            2
  ((16 delta  - 16 delta epsilon - f + 4 delta f - 
 
              2                     3  4
       4 delta  f - 8 epsilon f) VP0  p ) / (8 f) + 
 
      6
  O[p]
:[font = input; preserveAspect; startGroup]
(%//Normal)/.zRule
:[font = output; output; inactive; preserveAspect; endGroup]
VP0^(-1) - ((1 + 2*delta)*z)/(2*VP0) + 
 
  ((16*delta^2 - 16*delta*epsilon - f + 4*delta*f - 
 
       4*delta^2*f - 8*epsilon*f)*z^2)/(8*f*VP0)
;[o]
 1    (1 + 2 delta) z
--- - --------------- + 
VP0        2 VP0
 
            2
  ((16 delta  - 16 delta epsilon - f + 4 delta f - 
 
              2                   2
       4 delta  f - 8 epsilon f) z ) / (8 f VP0)
:[font = input; preserveAspect; startGroup]
Coefficient[8f VP0 %, z, 2]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
16*delta^2 - 16*delta*epsilon - f + 4*delta*f - 
 
  4*delta^2*f - 8*epsilon*f
;[o]
        2
16 delta  - 16 delta epsilon - f + 4 delta f - 
 
         2
  4 delta  f - 8 epsilon f
:[font = section; inactive; preserveAspect; startGroup]
Velocities and Polarizations
:[font = subsection; inactive; preserveAspect; startGroup]
moveout function as function of p
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Weak
:[font = input; preserveAspect; startGroup]
vNMOsqWeak = VNMOsq[weakQ,{p}]//
	WeakVTI//Together//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
(VP0^2*(1 + 2*delta - p^2*VP0^2 - 
 
      12*delta*p^2*VP0^2 + 12*epsilon*p^2*VP0^2 + 
 
      18*delta*p^4*VP0^4 - 18*epsilon*p^4*VP0^4 - 
 
      8*delta*p^6*VP0^6 + 8*epsilon*p^6*VP0^6))/
 
  (-1 + p^2*VP0^2)^2
;[o]
    2                 2    2             2    2
(VP0  (1 + 2 delta - p  VP0  - 12 delta p  VP0  + 
 
                  2    2             4    4
      12 epsilon p  VP0  + 18 delta p  VP0  - 
 
                  4    4            6    6
      18 epsilon p  VP0  - 8 delta p  VP0  + 
 
                 6    6            2    2 2
      8 epsilon p  VP0 )) / (-1 + p  VP0 )
:[font = input; preserveAspect; startGroup]
vNMOsqWeak/.zRule
:[font = output; output; inactive; preserveAspect; endGroup]
(VP0^2*(1 + 2*delta - z - 12*delta*z + 12*epsilon*z + 
 
      18*delta*z^2 - 18*epsilon*z^2 - 8*delta*z^3 + 
 
      8*epsilon*z^3))/(-1 + z)^2
;[o]
    2
(VP0  (1 + 2 delta - z - 12 delta z + 12 epsilon z + 
 
                2               2            3
      18 delta z  - 18 epsilon z  - 8 delta z  + 
 
                 3             2
      8 epsilon z )) / (-1 + z)
:[font = input; preserveAspect; startGroup]
% - VP0^2/(1-z)/.epsilon->delta+weaketa//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
(2*VP0^2*(delta + 6*weaketa*z - 9*weaketa*z^2 + 
 
      4*weaketa*z^3))/(-1 + z)^2
;[o]
      2                                   2
(2 VP0  (delta + 6 weaketa z - 9 weaketa z  + 
 
                 3             2
      4 weaketa z )) / (-1 + z)
:[font = input; preserveAspect; startGroup]
vNMOsqWeak/.ATRules//WeakVTI
:[font = output; output; inactive; preserveAspect; endGroup]
(Vnmo0^2*(1 - y + 12*eta*y - 18*eta*y^2 + 8*eta*y^3))/
 
  (-1 + y)^2
;[o]
     2                             2          3
Vnmo0  (1 - y + 12 eta y - 18 eta y  + 8 eta y )
------------------------------------------------
                           2
                   (-1 + y)
:[font = text; inactive; preserveAspect; rightWrapOffset = 580; endGroup]
This agrees with Cohen (42) (first derived by Alkhalifah and Tsvankin).
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Moderate
:[font = input; preserveAspect]
VNMOsq[moderateQ,{p}]//WeakVTI[#,2]&;
%/.ATRulesVTIQP//WeakVTI[#,2]&
:[font = text; inactive; preserveAspect; endGroup]
This (probably) agrees with Cohen (45) .  (It took a lot of work to massage the result into something readable, cf. nmo.ma notebook).
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Small p
:[font = input; preserveAspect; startGroup]
VNMOsq[qSmallp5,{p}]//
Together//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
SeriesData[p, 0, {(1 + 2*delta)*VP0^2, 0, 
 
   ((-24*delta^2 + 24*delta*epsilon + f - 
 
        8*delta*f + 4*delta^2*f + 12*epsilon*f)*VP0^4
 
      )/f}, 0, 4, 1]
;[o]
                 2              2
(1 + 2 delta) VP0  + ((-24 delta  + 
 
       24 delta epsilon + f - 8 delta f + 
 
              2                      4  2
       4 delta  f + 12 epsilon f) VP0  p ) / f + 
 
      4
  O[p]
:[font = text; inactive; preserveAspect]
This agrees with Cohen (48).
:[font = input; preserveAspect; startGroup]
(%//Normal)/.ATRules
:[font = output; output; inactive; preserveAspect; endGroup]
Vnmo0^2 + ((-24*delta^2 + 
 
       24*delta*(delta + (1 + 2*delta)*eta) + f - 
 
       8*delta*f + 4*delta^2*f + 
 
       12*(delta + (1 + 2*delta)*eta)*f)*Vnmo0^2*y)/
 
   ((1 + 2*delta)^2*f)
;[o]
     2              2
Vnmo0  + ((-24 delta  + 
 
       24 delta (delta + (1 + 2 delta) eta) + f - 
 
                          2
       8 delta f + 4 delta  f + 
 
                                              2
       12 (delta + (1 + 2 delta) eta) f) Vnmo0  y) / 
 
                 2
   ((1 + 2 delta)  f)
:[font = input; preserveAspect; startGroup]
CoefficientList[%/Vnmo0^2, y]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{1, (24*delta*eta + f + 2*delta*f + 12*eta*f)/
 
   (f + 2*delta*f)}
;[o]
    24 delta eta + f + 2 delta f + 12 eta f
{1, ---------------------------------------}
                 f + 2 delta f
:[font = text; inactive; preserveAspect; endGroup; endGroup]
This agrees with Cohen (49).
:[font = subsection; inactive; preserveAspect; startGroup]
phase speed as function of p
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
General form
:[font = input; preserveAspect; rightWrapOffset = 581; startGroup]
vsqQP = Vsq[Q, {p}]/.zRule//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
(2*(-1 + f)*VP0^2)/
 
  (-2 + f + 2*epsilon*z - 2*delta*f*z + 
 
    VP0^2*((f^2 + 8*delta*f*z - 4*epsilon*f*z - 
 
          4*delta*f^2*z + 4*epsilon^2*z^2 - 
 
          8*delta*f*z^2 + 8*epsilon*f*z^2 - 
 
          8*delta*epsilon*f*z^2 + 8*delta*f^2*z^2 + 
 
          4*delta^2*f^2*z^2 - 8*epsilon*f^2*z^2)/VP0^4
 
        )^(1/2))
;[o]
               2
(2 (-1 + f) VP0 ) / 
 
  (-2 + f + 2 epsilon z - 2 delta f z + 
 
       2        2
    VP0  Sqrt[(f  + 8 delta f z - 4 epsilon f z - 
 
                  2              2  2
         4 delta f  z + 4 epsilon  z  - 
 
                    2                2
         8 delta f z  + 8 epsilon f z  - 
 
                            2            2  2
         8 delta epsilon f z  + 8 delta f  z  + 
 
                2  2  2              2  2       4
         4 delta  f  z  - 8 epsilon f  z ) / VP0 ])
:[font = text; inactive; preserveAspect; rightWrapOffset = 535; endGroup]
This is in an awkward form for simplification.  Namely, 
a/(b-sqrt(c)).  Got the general form in the paper in the q[p] section above by direct manipulation of q^2 and no point in repeating that here.  The form here could be used for expansions.
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Weak limit
:[font = text; inactive; preserveAspect]
Can get result from the general expression for the phase velocity
:[font = input; preserveAspect; startGroup]
vsqQP//Sqrt//WeakVTI
:[font = output; output; inactive; preserveAspect; endGroup]
VP0 + (2*2^(1/2)*(-1 + f)^(3/2)*VP0*z*
 
     (delta - delta*z + epsilon*z))/(-2 + 2*f)^(3/2)
;[o]
                         3/2
VP0 + (2 Sqrt[2] (-1 + f)    VP0 z 
 
                                                3/2
     (delta - delta z + epsilon z)) / (-2 + 2 f)
:[font = input; preserveAspect; startGroup]
%//UseVS0//PowerExpand//Collect[#,{VP0,z}]&
:[font = output; output; inactive; preserveAspect; endGroup]
VP0*(1 + delta*z + (-delta + epsilon)*z^2)
;[o]
                                       2
VP0 (1 + delta z + (-delta + epsilon) z )
:[font = text; inactive; preserveAspect]
This agrees with Cohen (20) and is equivalent to Thomsen's 1986 result.  Notice that phase velocity expansions are usually well expressed in the dimensionless variable z = cp^2 p^2. 
:[font = text; inactive; preserveAspect]
Alternatively, use the weak approximation for Q derived above in the Vsq function:
:[font = input; preserveAspect; startGroup]
Vsq[weakQ,{p}]/.zRule//Sqrt//
	WeakVTI//Collect[#,{VP0,z}]&
:[font = output; output; inactive; preserveAspect; endGroup]
VP0*(1 + delta*z + (-delta + epsilon)*z^2)
;[o]
                                       2
VP0 (1 + delta z + (-delta + epsilon) z )
:[font = text; inactive; preserveAspect; endGroup]
Alternatively, use the weak approximation for Q derived above:
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Moderate
:[font = input; preserveAspect; startGroup]
Vsq[moderateQ,{p}]/.zRule//Sqrt//
	WeakVTI[#,2]&//Collect[#,{VP0,z}]&//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
VP0*(1 + delta*z + (-delta + (3*delta^2)/2 + 
 
       epsilon - (2*delta^2)/f + (2*delta*epsilon)/f)*
 
     z^2 + ((-delta + epsilon)*
 
       (-4*delta + 2*epsilon + 5*delta*f)*z^3)/f + 
 
    ((-delta + epsilon)^2*(-4 + 7*f)*z^4)/(2*f))
;[o]
                                    2
                             3 delta
VP0 (1 + delta z + (-delta + -------- + epsilon - 
                                2
 
              2
       2 delta    2 delta epsilon   2
       -------- + ---------------) z  + 
          f              f
 
    ((-delta + epsilon) 
 
                                           3
       (-4 delta + 2 epsilon + 5 delta f) z ) / f + 
 
                      2             4
    (-delta + epsilon)  (-4 + 7 f) z
    ---------------------------------)
                   2 f
:[font = text; inactive; preserveAspect; endGroup]
This agrees with Cohen (22) . 
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Small p
:[font = input; preserveAspect; startGroup]
Vsq[qSmallp5,{p}]/.zRule//Sqrt//PowerExpand//
	Collect[#,{VP0,z}]&//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
VP0*(1 + delta*z + (-delta + (3*delta^2)/2 + 
 
       epsilon - (2*delta^2)/f + (2*delta*epsilon)/f)*
 
     z^2)
;[o]
                                    2
                             3 delta
VP0 (1 + delta z + (-delta + -------- + epsilon - 
                                2
 
              2
       2 delta    2 delta epsilon   2
       -------- + ---------------) z )
          f              f
:[font = subsection; inactive; preserveAspect; startGroup]
group velocity as function of  p
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
General Form
:[font = input; preserveAspect]
gQP = GVel[evalQP, Q, {p, q}];
:[font = input; preserveAspect]
gQP/.zRule//Simplify//PowerExpand;
:[font = text; inactive; preserveAspect]
Too big to print!
:[font = input; preserveAspect; startGroup]
gQP/.p->0//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
{0, 0, ((-2 + 2*f)^(1/2)*VP0)/
 
   (2^(1/2)*(-1 + f)^(1/2))}
;[o]
        Sqrt[-2 + 2 f] VP0
{0, 0, --------------------}
       Sqrt[2] Sqrt[-1 + f]
:[font = input; preserveAspect; startGroup]
%//UseVS0//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{0, 0, VP0}
;[o]
{0, 0, VP0}
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Weak Limit
:[font = input; preserveAspect; startGroup]
weakevalQP = evalQP//WeakVTI
:[font = output; output; inactive; preserveAspect; endGroup]
((p^4 + 2*epsilon*p^4 + 2*p^2*q^2 + 2*delta*p^2*q^2 + 
 
      q^4)*rho*VP0^2)/(p^2 + q^2)
;[o]
   4              4      2  2            2  2    4
((p  + 2 epsilon p  + 2 p  q  + 2 delta p  q  + q ) 
 
           2      2    2
    rho VP0 ) / (p  + q )
:[font = input; preserveAspect; startGroup]
weakgQP = GVel[weakevalQP, Q, {p, q}]//WeakVTI//
	PowerExpand//UseVS0//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
{p*VP0^2*(1 + 2*delta - 4*delta*p^2*VP0^2 + 
 
     4*epsilon*p^2*VP0^2 + 2*delta*p^4*VP0^4 - 
 
     2*epsilon*p^4*VP0^4), 0, 
 
  VP0*(1 - p^2*VP0^2)^(1/2) + 
 
   (VP0^2*(2*2^(1/2)*p^2*VP0*(1 - p^2*VP0^2)^(1/2)*
 
         (-2*delta + 3*delta*p^2*VP0^2 - 
 
           3*epsilon*p^2*VP0^2) + 
 
        (2^(1/2)*p^2*VP0*(3 - 4*p^2*VP0^2)*
 
           (delta - delta*p^2*VP0^2 + 
 
             epsilon*p^2*VP0^2))/(1 - p^2*VP0^2)^(1/2)
 
        ))/2^(1/2)}
;[o]
      2                         2    2
{p VP0  (1 + 2 delta - 4 delta p  VP0  + 
 
                2    2            4    4
     4 epsilon p  VP0  + 2 delta p  VP0  - 
 
                4    4
     2 epsilon p  VP0 ), 0, 
 
                2    2
  VP0 Sqrt[1 - p  VP0 ] + 
 
       2             2               2    2
   (VP0  (2 Sqrt[2] p  VP0 Sqrt[1 - p  VP0 ] 
 
                              2    2
         (-2 delta + 3 delta p  VP0  - 
 
                      2    2
           3 epsilon p  VP0 ) + 
 
                  2             2    2
        (Sqrt[2] p  VP0 (3 - 4 p  VP0 ) 
 
                           2    2            2    2
           (delta - delta p  VP0  + epsilon p  VP0 ))\
 
                       2    2
           / Sqrt[1 - p  VP0 ])) / Sqrt[2]}
:[font = input; preserveAspect; startGroup]
weakgQP/.p->0
:[font = output; output; inactive; preserveAspect; endGroup]
{0, 0, VP0}
;[o]
{0, 0, VP0}
:[font = input; preserveAspect; startGroup]
weakgQPz = weakgQP/.zRule//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{VP0*z^(1/2)*(1 + 2*delta - 4*delta*z + 4*epsilon*z + 
 
     2*delta*z^2 - 2*epsilon*z^2), 0, 
 
  (VP0*(1 - z - delta*z + 3*delta*z^2 - 
 
       3*epsilon*z^2 - 2*delta*z^3 + 2*epsilon*z^3))/
 
   (1 - z)^(1/2)}
;[o]
{VP0 Sqrt[z] (1 + 2 delta - 4 delta z + 4 epsilon z + 
 
              2              2
     2 delta z  - 2 epsilon z ), 0, 
 
                                   2              2
  (VP0 (1 - z - delta z + 3 delta z  - 3 epsilon z  - 
 
                3              3
       2 delta z  + 2 epsilon z )) / Sqrt[1 - z]}
:[font = input; preserveAspect; startGroup]
weakgQPz[[1]]/(VP0 Sqrt[z])//
Collect[#,{z}]&
:[font = output; output; inactive; preserveAspect; endGroup]
1 + 2*delta + (-4*delta + 4*epsilon)*z + 
 
  (2*delta - 2*epsilon)*z^2
;[o]
1 + 2 delta + (-4 delta + 4 epsilon) z + 
 
                         2
  (2 delta - 2 epsilon) z
:[font = input; preserveAspect; startGroup]
weakgQPz[[3]]/(VP0 / Sqrt[1-z])//
Collect[#,{z}]&
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
1 + (-1 - delta)*z + (3*delta - 3*epsilon)*z^2 + 
 
  (-2*delta + 2*epsilon)*z^3
;[o]
                                            2
1 + (-1 - delta) z + (3 delta - 3 epsilon) z  + 
 
                          3
  (-2 delta + 2 epsilon) z
:[font = subsubsection; inactive; preserveAspect]
Moderate -- not done yet
:[font = subsubsection; inactive; preserveAspect; endGroup]
Small p -- not done yet
:[font = subsection; inactive; preserveAspect; startGroup]
polarization vector as function of p
:[font = text; inactive; preserveAspect]
vector =  (tan gamma, 0, 1)
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Weak Limit
:[font = input; preserveAspect; startGroup]
weakTangamma =
	(polA+Sqrt[polB])/polC/.q->weakQ//WeakVTI
:[font = output; output; inactive; preserveAspect; endGroup]
(p*VP0*(delta + f - 3*delta*p^2*VP0^2 + 
 
      2*epsilon*p^2*VP0^2 - f*p^2*VP0^2 + 
 
      delta*f*p^2*VP0^2 + 2*delta*p^4*VP0^4 - 
 
      2*epsilon*p^4*VP0^4 - delta*f*p^4*VP0^4 + 
 
      epsilon*f*p^4*VP0^4))/(f*(1 - p^2*VP0^2)^(3/2))
;[o]
                             2    2
(p VP0 (delta + f - 3 delta p  VP0  + 
 
                 2    2      2    2
      2 epsilon p  VP0  - f p  VP0  + 
 
               2    2            4    4
      delta f p  VP0  + 2 delta p  VP0  - 
 
                 4    4            4    4
      2 epsilon p  VP0  - delta f p  VP0  + 
 
                 4    4              2    2 3/2
      epsilon f p  VP0 )) / (f (1 - p  VP0 )   )
:[font = input; preserveAspect; startGroup]
weakTangammaz = weakTangamma/.zRule
:[font = output; output; inactive; preserveAspect; endGroup]
(z^(1/2)*(delta + f - 3*delta*z + 2*epsilon*z - f*z + 
 
      delta*f*z + 2*delta*z^2 - 2*epsilon*z^2 - 
 
      delta*f*z^2 + epsilon*f*z^2))/(f*(1 - z)^(3/2))
;[o]
(Sqrt[z] (delta + f - 3 delta z + 2 epsilon z - f z + 
 
                           2              2
      delta f z + 2 delta z  - 2 epsilon z  - 
 
               2              2               3/2
      delta f z  + epsilon f z )) / (f (1 - z)   )
:[font = input; preserveAspect; startGroup]
weakTangammaz f(1-z) Sqrt[1-z]/Sqrt[z]//Collect[#,{z}]&
:[font = output; output; inactive; preserveAspect; endGroup]
delta + f + (-3*delta + 2*epsilon - f + delta*f)*z + 
 
  (2*delta - 2*epsilon - delta*f + epsilon*f)*z^2
;[o]
delta + f + (-3 delta + 2 epsilon - f + delta f) z + 
 
                                               2
  (2 delta - 2 epsilon - delta f + epsilon f) z
:[font = input; preserveAspect; startGroup]
Coefficient[%,z,2]//Factor
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(-delta + epsilon)*(-2 + f)
;[o]
(-delta + epsilon) (-2 + f)
:[font = subsubsection; inactive; preserveAspect]
Moderate -- not done yet
:[font = subsubsection; inactive; preserveAspect; endGroup]
Small p -- not done yet
:[font = subsection; inactive; preserveAspect; startGroup]
phase speed as function of theta
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Weak Limit
:[font = input; preserveAspect; startGroup]
lambda[theta_] = weakevalQP/.nRules2D//
Simplify//SineForm//Factor
:[font = output; output; inactive; preserveAspect; endGroup]
rho*VP0^2*(1 + 2*delta*Sin[theta]^2 - 
 
    2*delta*Sin[theta]^4 + 2*epsilon*Sin[theta]^4)
;[o]
       2                        2
rho VP0  (1 + 2 delta Sin[theta]  - 
 
                      4                       4
    2 delta Sin[theta]  + 2 epsilon Sin[theta] )
:[font = input; preserveAspect; startGroup]
vsqAngleQP = lambda[theta]/rho
:[font = output; output; inactive; preserveAspect; endGroup]
VP0^2*(1 + 2*delta*Sin[theta]^2 - 
 
    2*delta*Sin[theta]^4 + 2*epsilon*Sin[theta]^4)
;[o]
   2                        2                     4
VP0  (1 + 2 delta Sin[theta]  - 2 delta Sin[theta]  + 
 
                        4
    2 epsilon Sin[theta] )
:[font = input; preserveAspect; startGroup]
vAngleQP = vsqAngleQP//Sqrt//WeakVTI//Factor
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VP0*(1 + delta*Sin[theta]^2 - delta*Sin[theta]^4 + 
 
    epsilon*Sin[theta]^4)
;[o]
                         2                   4
VP0 (1 + delta Sin[theta]  - delta Sin[theta]  + 
 
                      4
    epsilon Sin[theta] )
:[font = subsubsection; inactive; preserveAspect]
Moderate -- not done yet
:[font = subsubsection; inactive; preserveAspect; endGroup]
Small p -- not done yet
:[font = subsection; inactive; preserveAspect; startGroup]
group velocity as function of theta
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Weak Limit
:[font = input; preserveAspect; startGroup]
weakgAngleQP = weakgQP/. p -> Sin[theta]/vAngleQP//
WeakVTI
:[font = output; output; inactive; preserveAspect; endGroup]
{(VP0*(8 + 5*delta + 7*epsilon + 
 
       8*delta*Cos[2*theta] - 
 
       4*epsilon*Cos[2*theta] + 
 
       3*delta*Cos[4*theta] - 3*epsilon*Cos[4*theta])*
 
     Sin[theta])/8, 0, 
 
  (VP0*Cos[theta]*(8 + 5*delta - 9*epsilon - 
 
       8*delta*Cos[2*theta] + 
 
       12*epsilon*Cos[2*theta] + 
 
       3*delta*Cos[4*theta] - 3*epsilon*Cos[4*theta]))
 
    /8}
;[o]
{(VP0 (8 + 5 delta + 7 epsilon + 
 
       8 delta Cos[2 theta] - 
 
       4 epsilon Cos[2 theta] + 
 
       3 delta Cos[4 theta] - 3 epsilon Cos[4 theta]) 
 
     Sin[theta]) / 8, 0, 
 
  (VP0 Cos[theta] (8 + 5 delta - 9 epsilon - 
 
       8 delta Cos[2 theta] + 
 
       12 epsilon Cos[2 theta] + 
 
       3 delta Cos[4 theta] - 3 epsilon Cos[4 theta]))
 
     / 8}
:[font = input; preserveAspect; startGroup]
weakgAngleQP1 = weakgAngleQP[[1]]/(VP0 Sin[theta])//
SineForm//FactorTrig//Collect[#,{epsilon,delta}]&
:[font = output; output; inactive; preserveAspect; endGroup]
1 + epsilon*(4*Sin[theta]^2 - 3*Sin[theta]^4) + 
 
  delta*(2 - 5*Sin[theta]^2 + 3*Sin[theta]^4)
;[o]
                         2               4
1 + epsilon (4 Sin[theta]  - 3 Sin[theta] ) + 
 
                         2               4
  delta (2 - 5 Sin[theta]  + 3 Sin[theta] )
:[font = input; preserveAspect; startGroup]
weakgAngleQP3 = weakgAngleQP[[3]]/(VP0 Cos[theta])//
SineForm//FactorTrig//Collect[#,{epsilon,delta}]&
:[font = output; output; inactive; preserveAspect; endGroup]
1 - 3*epsilon*Sin[theta]^4 + 
 
  delta*(-Sin[theta]^2 + 3*Sin[theta]^4)
;[o]
                        4
1 - 3 epsilon Sin[theta]  + 
 
                    2               4
  delta (-Sin[theta]  + 3 Sin[theta] )
:[font = input; preserveAspect; startGroup]
Factor[2-5x+3x^2]
:[font = output; output; inactive; preserveAspect; endGroup]
(-1 + x)*(-2 + 3*x)
;[o]
(-1 + x) (-2 + 3 x)
:[font = text; inactive; preserveAspect; rightWrapOffset = 531]
Next result is modulo overall factor of tan(theta) taken out above.
:[font = input; preserveAspect; startGroup]
weaktanpsi = weakgAngleQP1/weakgAngleQP3//
WeakVTI//SineForm
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
1 + 2*delta - 4*delta*Sin[theta]^2 + 
 
  4*epsilon*Sin[theta]^2
;[o]
                                2
1 + 2 delta - 4 delta Sin[theta]  + 
 
                      2
  4 epsilon Sin[theta]
:[font = subsection; inactive; preserveAspect; startGroup]
polarization vector as function of theta
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
General Form
:[font = input; preserveAspect; startGroup]
AnglePolA = polA/.nRules2D//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
-(f*Cos[2*theta]) + 2*epsilon*Sin[theta]^2
;[o]
                                        2
-(f Cos[2 theta]) + 2 epsilon Sin[theta]
:[font = input; preserveAspect; startGroup]
AnglePolB = polB/.nRules2D//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
f^2 + 4*f*(deltaTilde + deltaTilde*Cos[2*theta] - 
 
     epsilon*Cos[2*theta])*Sin[theta]^2 + 
 
  4*Sin[theta]^2*(deltaTilde^2*Cos[theta]^2 + 
 
     epsilon^2*Sin[theta]^2)
;[o]
 2
f  + 4 f (deltaTilde + deltaTilde Cos[2 theta] - 
 
                                     2
     epsilon Cos[2 theta]) Sin[theta]  + 
 
              2            2           2
  4 Sin[theta]  (deltaTilde  Cos[theta]  + 
 
            2           2
     epsilon  Sin[theta] )
:[font = input; preserveAspect; startGroup]
AnglePolC = polC/.nRules2D//Simplify
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(deltaTilde + f)*Sin[2*theta]
;[o]
(deltaTilde + f) Sin[2 theta]
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Weak Limit
:[font = input; preserveAspect; startGroup]
weakAngleTangamma =
	(AnglePolA+Sqrt[AnglePolB])/AnglePolC//
	WeakVTI//SineForm//Factor
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup; endGroup; endGroup]
((delta + f - 2*delta*Sin[theta]^2 + 
 
      2*epsilon*Sin[theta]^2)*Tan[theta])/f
;[o]
                                2
((delta + f - 2 delta Sin[theta]  + 
 
                          2
      2 epsilon Sin[theta] ) Tan[theta]) / f
^*)