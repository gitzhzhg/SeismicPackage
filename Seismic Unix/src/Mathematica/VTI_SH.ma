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
This notebook is an application notebook.  For basic usage of the VTI package, see VTI.ma.  Here, we exercise the 2D q(p) approach to determining moveout, phase and group velocities as functions of ray parameter, p.
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

 Computes the moveout function in a symmetry plane as
   a function of ray parameter, given the vertical
   slowness, Q[p]. 
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
 
       2     2      2                  2     3
    3 p  v[p]  v'[p]  - v[p] v''[p] + p  v[p]  v''[p])
 
                   2     2
    / (v[p] (-1 + p  v[p] ) (v[p] + p v'[p]))
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
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Rules
:[font = input; initialization; preserveAspect; endGroup]
*)
pRules2D = {p1->p,p2->0,p3->q};
nRules2D = {p->Sin[theta],q->Cos[theta]};
zRule = p -> Sqrt[z]/VS0;
ATRules = {
	z -> y/(1+2gamma),
	VS0 -> Vnmo0/Sqrt[1+2 gamma]
};
(*
:[font = subsection; inactive; preserveAspect; startGroup]
EigenAnalysis of VTI Matrix
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
Eigenvalue and vector
:[font = input; preserveAspect]
MVTI =
(C11 p1^2 + C66 p2^2 + C55 p3^2)ii +
(C11 p2^2 + C55 p3^2 + C66 p1^2)jj +
(C33 p3^2 + C55(p1^2 + p2^2))kk +
(C11 - C66)p1 p2 (ij+ji) +
(C13 + C55)p2 p3 (jk+kj) +
(C13 + C55)p3 p1 (ki+ik);
MVTI2D=MVTI/.pRules2D;
{evalVTI, evecVTI} = Eigensystem@MVTI2D;
:[font = text; inactive; preserveAspect]
Check which entry is SH mode:
:[font = input; preserveAspect; startGroup]
evalVTI[[1]]
:[font = output; output; inactive; preserveAspect; endGroup]
C66*p^2 + C55*q^2
;[o]
     2        2
C66 p  + C55 q
:[font = input; preserveAspect; startGroup]
evalVTISH = evalVTI[[1]]//ConvertStandard
:[font = output; output; inactive; preserveAspect; endGroup]
(p^2 + 2*gamma*p^2 + q^2)*rho*VS0^2
;[o]
  2            2    2         2
(p  + 2 gamma p  + q ) rho VS0
:[font = input; preserveAspect; startGroup]
evecVTISH = evecVTI[[1]]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{0, 1, 0}
;[o]
{0, 1, 0}
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
q[p]
:[font = text; inactive; preserveAspect]
Avoid superfluous negative square roots of q^2:
:[font = input; preserveAspect; startGroup]
HalfPower[evalVTISH,p,psq];
HalfPower[%,q,qsq];
solnRules = Solve[% == rho,qsq]
:[font = output; output; inactive; preserveAspect; endGroup]
{{qsq -> -((-1 + psq*VS0^2 + 2*gamma*psq*VS0^2)/
 
       VS0^2)}}
;[o]
                       2                  2
           -1 + psq VS0  + 2 gamma psq VS0
{{qsq -> -(--------------------------------)}}
                            2
                         VS0
:[font = input; preserveAspect; startGroup]
QVTISH=qsq/.First@First@solnRules/.{psq->p^2}//
Sqrt//Expand//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
(1 - p^2*VS0^2 - 2*gamma*p^2*VS0^2)^(1/2)/VS0
;[o]
          2    2            2    2
Sqrt[1 - p  VS0  - 2 gamma p  VS0 ]
-----------------------------------
                VS0
:[font = input; preserveAspect; startGroup]
QzVTISH = QVTISH/.zRule
:[font = output; output; inactive; preserveAspect; endGroup]
(1 - z - 2*gamma*z)^(1/2)/VS0
;[o]
Sqrt[1 - z - 2 gamma z]
-----------------------
          VS0
:[font = input; preserveAspect; startGroup]
QzVTISH//WeakVTI
:[font = output; output; inactive; preserveAspect; endGroup]
(1 - z - gamma*z)/(VS0*(1 - z)^(1/2))
;[o]
1 - z - gamma z
---------------
VS0 Sqrt[1 - z]
:[font = input; preserveAspect; startGroup]
QzVTISH//WeakVTI[#,2]&
:[font = output; output; inactive; preserveAspect; endGroup]
(2 - 4*z - 2*gamma*z + 2*z^2 + 2*gamma*z^2 - 
 
    gamma^2*z^2)/(2*VS0*(1 - z)^(3/2))
;[o]
                         2            2        2  2
2 - 4 z - 2 gamma z + 2 z  + 2 gamma z  - gamma  z
---------------------------------------------------
                              3/2
                 2 VS0 (1 - z)
:[font = input; preserveAspect; startGroup]
CoefficientList[2 VS0 (1-z)^(3/2) %,gamma]//
Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
{2*(-1 + z)^2, 2*(-1 + z)*z, -z^2}
;[o]
           2                  2
{2 (-1 + z) , 2 (-1 + z) z, -z }
:[font = input; preserveAspect; startGroup]
QzVTISH + O[z]^3
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup; endGroup]
SeriesData[z, 0, {VS0^(-1), (-1 - 2*gamma)/(2*VS0), 
 
   -(-1 - 2*gamma)^2/(8*VS0)}, 0, 3, 1]
;[o]
                                       2  2
 1    (-1 - 2 gamma) z   (-1 - 2 gamma)  z        3
--- + ---------------- - ------------------ + O[z]
VS0        2 VS0               8 VS0
:[font = section; inactive; preserveAspect; startGroup]
Velocities and Polarizations
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
moveout function as function of p
:[font = input; preserveAspect; startGroup]
vnmosqVTISH = VNMOsq[QVTISH, {p}]
:[font = output; output; inactive; preserveAspect; endGroup]
((1 + 2*gamma)*VS0^2)/
 
  (1 - p^2*VS0^2 - 2*gamma*p^2*VS0^2)
;[o]
                      2
     (1 + 2 gamma) VS0
-----------------------------
     2    2            2    2
1 - p  VS0  - 2 gamma p  VS0
:[font = input; preserveAspect; startGroup]
vnmo0IsoP = vnmosqVTISH/.p->0//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
(1 + 2*gamma)^(1/2)*VS0
;[o]
Sqrt[1 + 2 gamma] VS0
:[font = input; preserveAspect; startGroup]
vnmosqVTISH/.ATRules//Together//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
Vnmo0^2/(1 - y)
;[o]
     2
Vnmo0
------
1 - y
:[font = input; preserveAspect; startGroup]
vnmoVTISH = %//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
Vnmo0/(1 - y)^(1/2)
;[o]
   Vnmo0
-----------
Sqrt[1 - y]
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
phase speed as function of p
:[font = input; preserveAspect; startGroup]
vsqVTISH = Vsq[QVTISH, {p}]
:[font = output; output; inactive; preserveAspect; endGroup]
(-2*gamma*p^2 + VS0^(-2))^(-1)
;[o]
         1
-------------------
          2      -2
-2 gamma p  + VS0
:[font = input; preserveAspect; startGroup]
%/.zRule//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
VS0^2/(1 - 2*gamma*z)
;[o]
       2
    VS0
-------------
1 - 2 gamma z
:[font = input; preserveAspect; startGroup]
vVTISH = %//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VS0/(1 - 2*gamma*z)^(1/2)
;[o]
        VS0
-------------------
Sqrt[1 - 2 gamma z]
:[font = subsubsection; inactive; preserveAspect; startGroup]
group velocity as function of  p
:[font = input; preserveAspect; startGroup]
gVTISH = GVel[evalVTISH, QVTISH, {p, q}]
:[font = output; output; inactive; preserveAspect; endGroup]
{(1 + 2*gamma)*p*VS0^2, 0, 
 
  (-p^2 - 2*gamma*p^2 + VS0^(-2))^(1/2)*VS0^2}
;[o]
                    2
{(1 + 2 gamma) p VS0 , 0, 
 
         2            2      -2     2
  Sqrt[-p  - 2 gamma p  + VS0  ] VS0 }
:[font = input; preserveAspect; startGroup]
gVTISH/.zRule//Simplify//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
{(1 + 2*gamma)*VS0*z^(1/2), 0, 
 
  VS0*(1 - z - 2*gamma*z)^(1/2)}
;[o]
{(1 + 2 gamma) VS0 Sqrt[z], 0, 
 
  VS0 Sqrt[1 - z - 2 gamma z]}
:[font = text; inactive; preserveAspect; rightWrapOffset = 544; endGroup]
That is,  g[p] = VS0 {(1+2gamma) Sqrt[z], 0, Sqrt[1-(1+2gamma)z]}
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
polarization vector as function of p
:[font = input; preserveAspect; startGroup]
polVTISH = evecVTISH/.q->QVTISH
:[font = output; output; inactive; preserveAspect; endGroup]
{0, 1, 0}
;[o]
{0, 1, 0}
:[font = input; preserveAspect; startGroup]
%/. zRule
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{0, 1, 0}
;[o]
{0, 1, 0}
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
phase speed as function of theta
:[font = input; preserveAspect; startGroup]
lambda[theta_] = evalVTISH/.nRules2D//
Simplify//SineForm//Factor
:[font = output; output; inactive; preserveAspect; endGroup]
rho*VS0^2*(1 + 2*gamma*Sin[theta]^2)
;[o]
       2                        2
rho VS0  (1 + 2 gamma Sin[theta] )
:[font = input; preserveAspect; startGroup]
vsqAngleVTISH = lambda[theta]/rho
:[font = output; output; inactive; preserveAspect; endGroup]
VS0^2*(1 + 2*gamma*Sin[theta]^2)
;[o]
   2                        2
VS0  (1 + 2 gamma Sin[theta] )
:[font = input; preserveAspect; startGroup]
vAngleVTISH = vsqAngleVTISH//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VS0*(1 + 2*gamma*Sin[theta]^2)^(1/2)
;[o]
                               2
VS0 Sqrt[1 + 2 gamma Sin[theta] ]
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
group velocity as function of theta
:[font = input; preserveAspect; startGroup]
gVTISH/. p -> Sin[theta]/vAngleVTISH//
Together//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
{((1 + 2*gamma)*VS0*Sin[theta])/
 
   (1 + 2*gamma*Sin[theta]^2)^(1/2), 0, 
 
  (VS0*(1 - Sin[theta]^2)^(1/2))/
 
   (1 + 2*gamma*Sin[theta]^2)^(1/2)}
;[o]
 (1 + 2 gamma) VS0 Sin[theta]
{-----------------------------, 0, 
                            2
 Sqrt[1 + 2 gamma Sin[theta] ]
 
                           2
    VS0 Sqrt[1 - Sin[theta] ]
  -----------------------------}
                             2
  Sqrt[1 + 2 gamma Sin[theta] ]
:[font = text; inactive; preserveAspect; rightWrapOffset = 651; endGroup]
That is,
 g(theta) = (VS0/Sqrt[1+2gamma Sin^2[theta]] )
 		{(1+2gamma) Sin[theta], 0,Cos[theta]}
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
polarization as a function of theta
:[font = input; preserveAspect; startGroup]
polVTISH/. p -> Sin[theta]/vAngleVTISH
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup; endGroup]
{0, 1, 0}
;[o]
{0, 1, 0}
^*)