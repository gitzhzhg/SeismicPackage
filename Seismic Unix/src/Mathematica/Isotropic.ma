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
Isotropic Case (2D)
:[font = subtitle; inactive; preserveAspect; fontSize = 24]
Using the q(p) method
:[font = subsubtitle; inactive; preserveAspect; fontSize = 18]
Copyright, Jack K. Cohen, 1996, All Rights Reserved
:[font = input; initialization; preserveAspect]
*)
<<ATools.m
Off[General::spell, General::spell1]
(*
:[font = text; inactive; preserveAspect]
This notebook is an application notebook.  For basic usage of the ATools package, see ATools.ma.  Here, we exercise the 2D q(p) approach to determining moveout, phase and group velocities as functions of ray parameter, p.  Possibly, there will eventually be a similar notebook featuring the 3D q(p1,p2), although it isn't really needed here.
:[font = section; inactive; preserveAspect; startGroup]
The Theory
:[font = subsection; inactive; Cclosed; preserveAspect; rightWrapOffset = 554; startGroup]
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
:[font = subsection; inactive; Cclosed; preserveAspect; rightWrapOffset = 536; startGroup]
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
:[font = subsection; inactive; Cclosed; preserveAspect; rightWrapOffset = 537; startGroup]
Group velocity as a function of ray parameter
:[font = text; inactive; preserveAspect; endGroup]
Group velocity is given in terms of the eigenvalue lambda[pvec] (discussed above) as:
	g = 1/2rho * Gradp lambda
	   = 1/2rho * (d lambda/dp, d lambda/dq)
	where q is evaluated at q = Q[p].
:[font = subsection; inactive; Cclosed; preserveAspect; rightWrapOffset = 576; startGroup]
Polarization as a function of ray parameter
:[font = text; inactive; preserveAspect; endGroup]
The polarization is the eigenvector corresponding to lambda.  The eigenvectors can be taken as an orthonormal basis of R3.
:[font = subsection; inactive; Cclosed; preserveAspect; rightWrapOffset = 537; startGroup]
Phase angle representations
:[font = text; inactive; preserveAspect; endGroup; endGroup]
The phase speed is simply v(theta) = Sqrt[lambda[nvec]/rho] where in lambda, the slowness vector is replaced by a unit vector.   The group velocity and moveout velocity are obtained in terms of phase angle via the relation: p = sin(theta)/v(theta).
:[font = section; inactive; initialization; preserveAspect; startGroup]
 Initialization
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
Rules
:[font = input; initialization; preserveAspect; endGroup]
*)
velocityRules = {C33->rho VP0^2, C55->rho VS0^2};

pRules2D = {p1->p,p2->0,p3->q};
nRules2D = {p->Sin[theta],q->Cos[theta]};
zRule[V_] = p -> Sqrt[z]/V;
ATRules[V_] = {V->Vnmo0, p -> Sqrt[y]/Vnmo0};
(*
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
EigenAnalysis of Isotropic Matrix
:[font = input; initialization; preserveAspect]
*)
MIso =
(C33 p1^2 + C55(p2^2 + p3^2))ii +
(C33 p2^2 + C55(p3^2 + p1^2))jj +
(C33 p3^2 + C55(p1^2 + p2^2))kk +
(C33 - C55)p1 p2 (ij+ji) +
(C33 - C55)p2 p3 (jk+kj) +
(C33 - C55)p3 p1 (ki+ik);
MIso2D=MIso/.pRules2D;
(*
:[font = input; initialization; preserveAspect; startGroup]
*)
{evalIso, evecIso} = Eigensystem@MIso2D/.
velocityRules//Simplify
(*
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
{{(p^2 + q^2)*rho*VP0^2, (p^2 + q^2)*rho*VS0^2, 
 
   (p^2 + q^2)*rho*VS0^2}, 
 
  {{p/q, 0, 1}, {-(q/p), 0, 1}, {0, 1, 0}}}
;[o]
    2    2         2    2    2         2
{{(p  + q ) rho VP0 , (p  + q ) rho VS0 , 
 
     2    2         2
   (p  + q ) rho VS0 }, 
 
    p            q
  {{-, 0, 1}, {-(-), 0, 1}, {0, 1, 0}}}
    q            p
:[font = section; inactive; preserveAspect; startGroup]
Velocities and Polarizations
:[font = subsection; inactive; preserveAspect; startGroup]
P wave
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
mode
:[font = input; preserveAspect; startGroup]
evalIsoP = evalIso[[1]]
:[font = output; output; inactive; preserveAspect; endGroup]
(p^2 + q^2)*rho*VP0^2
;[o]
  2    2         2
(p  + q ) rho VP0
:[font = input; preserveAspect; startGroup]
q evecIso[[1]]
:[font = output; output; inactive; preserveAspect; endGroup]
{p, 0, q}
;[o]
{p, 0, q}
:[font = input; preserveAspect; startGroup]
evecIsoP = %/Norm@%
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{p/(p^2 + q^2)^(1/2), 0, q/(p^2 + q^2)^(1/2)}
;[o]
       p                 q
{-------------, 0, -------------}
       2    2            2    2
 Sqrt[p  + q ]     Sqrt[p  + q ]
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
q(p)
:[font = input; preserveAspect; startGroup]
solnRules = Solve[evalIsoP == rho,q]
:[font = output; output; inactive; preserveAspect; endGroup]
{{q -> -((1 - p^2*VP0^2)^(1/2)/VP0)}, 
 
  {q -> (1 - p^2*VP0^2)^(1/2)/VP0}}
;[o]
                   2    2
         Sqrt[1 - p  VP0 ]
{{q -> -(-----------------)}, 
                VP0
 
                  2    2
        Sqrt[1 - p  VP0 ]
  {q -> -----------------}}
               VP0
:[font = input; preserveAspect; startGroup]
QIsoP = q/.First@Rest@solnRules
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(1 - p^2*VP0^2)^(1/2)/VP0
;[o]
          2    2
Sqrt[1 - p  VP0 ]
-----------------
       VP0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
moveout function as function of p
:[font = input; preserveAspect; startGroup]
vnmosqIsoP = VNMOsq[QIsoP, {p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VP0^2/(1 - p^2*VP0^2)
;[o]
      2
   VP0
-----------
     2    2
1 - p  VP0
:[font = input; preserveAspect; startGroup]
vnmo0IsoP = vnmosqIsoP/.p->0//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
VP0
;[o]
VP0
:[font = input; preserveAspect; startGroup]
vnmosqIsoP/.ATRules[vnmo0IsoP]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
Vnmo0^2/(1 - y)
;[o]
     2
Vnmo0
------
1 - y
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
phase speed as function of p
:[font = input; preserveAspect; startGroup]
vsqIsoP = Vsq[QIsoP, {p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VP0^2
;[o]
   2
VP0
:[font = input; preserveAspect; startGroup]
vIsoP = vsqIsoP//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VP0
;[o]
VP0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
group velocity as function of p
:[font = input; preserveAspect; startGroup]
gIsoP = GVel[evalIsoP, QIsoP, {p, q}]
:[font = output; output; inactive; preserveAspect; endGroup]
{p*VP0^2, 0, VP0*(1 - p^2*VP0^2)^(1/2)}
;[o]
      2                   2    2
{p VP0 , 0, VP0 Sqrt[1 - p  VP0 ]}
:[font = input; preserveAspect; startGroup]
%/.zRule[vIsoP]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{VP0*z^(1/2), 0, VP0*(1 - z)^(1/2)}
;[o]
{VP0 Sqrt[z], 0, VP0 Sqrt[1 - z]}
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
polarization vector as function of p
:[font = input; preserveAspect; startGroup]
polIsoP = evecIsoP/.q->QIsoP//Simplify//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
{p*VP0, 0, (1 - p^2*VP0^2)^(1/2)}
;[o]
                     2    2
{p VP0, 0, Sqrt[1 - p  VP0 ]}
:[font = input; preserveAspect; startGroup]
%/. zRule[vIsoP]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{z^(1/2), 0, (1 - z)^(1/2)}
;[o]
{Sqrt[z], 0, Sqrt[1 - z]}
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
phase speed as function of theta
:[font = input; preserveAspect; startGroup]
lambda[theta_] = evalIsoP/.nRules2D//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
rho*VP0^2
;[o]
       2
rho VP0
:[font = input; preserveAspect; startGroup]
vsqAngleIsoP = lambda[theta]/rho
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VP0^2
;[o]
   2
VP0
:[font = subsubsection; inactive; Cclosed; preserveAspect; startGroup]
group velocity as function of theta
:[font = input; preserveAspect; startGroup]
gIsoP/. p -> Sin[theta]/Sqrt@vsqAngleIsoP//
CosineForm//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{VP0*Sin[theta], 0, VP0*Cos[theta]}
;[o]
{VP0 Sin[theta], 0, VP0 Cos[theta]}
:[font = subsubsection; inactive; preserveAspect; startGroup]
polarization as a function of theta
:[font = input; preserveAspect; startGroup]
polIsoP/. p -> Sin[theta]/Sqrt@vsqAngleIsoP//
CosineForm//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
{Sin[theta], 0, Cos[theta]}
;[o]
{Sin[theta], 0, Cos[theta]}
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
SV wave
:[font = subsubsection; inactive; preserveAspect; startGroup]
mode
:[font = input; preserveAspect; startGroup]
evalIsoSV = evalIso[[2]]//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
(p^2 + q^2)*rho*VS0^2
;[o]
  2    2         2
(p  + q ) rho VS0
:[font = input; preserveAspect; startGroup]
p evecIso[[2]]
:[font = output; output; inactive; preserveAspect; endGroup]
{-q, 0, p}
;[o]
{-q, 0, p}
:[font = input; preserveAspect; startGroup]
evecIsoSV = %/Norm@%
:[font = output; output; inactive; preserveAspect; endGroup]
{-(q/(p^2 + q^2)^(1/2)), 0, p/(p^2 + q^2)^(1/2)}
;[o]
         q                  p
{-(-------------), 0, -------------}
         2    2             2    2
   Sqrt[p  + q ]      Sqrt[p  + q ]
:[font = text; inactive; preserveAspect; endGroup]
Hence all the results that depend on the eigenvalue only differ from the P mode via VP0->VS0.
:[font = subsubsection; inactive; preserveAspect; startGroup]
q(p)
:[font = input; preserveAspect; startGroup]
solnRules = Solve[evalIsoSV == rho,q]
:[font = output; output; inactive; preserveAspect; endGroup]
{{q -> -((1 - p^2*VS0^2)^(1/2)/VS0)}, 
 
  {q -> (1 - p^2*VS0^2)^(1/2)/VS0}}
;[o]
                   2    2
         Sqrt[1 - p  VS0 ]
{{q -> -(-----------------)}, 
                VS0
 
                  2    2
        Sqrt[1 - p  VS0 ]
  {q -> -----------------}}
               VS0
:[font = input; preserveAspect; startGroup]
QIsoSV = q/.First@Rest@solnRules
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(1 - p^2*VS0^2)^(1/2)/VS0
;[o]
          2    2
Sqrt[1 - p  VS0 ]
-----------------
       VS0
:[font = subsubsection; inactive; preserveAspect; startGroup]
moveout function as function of p
:[font = input; preserveAspect; startGroup]
vnmosqIsoSV = VNMOsq[QIsoSV, {p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VS0^2/(1 - p^2*VS0^2)
;[o]
      2
   VS0
-----------
     2    2
1 - p  VS0
:[font = input; preserveAspect; startGroup]
vnmo0IsoSV = vnmosqIsoSV/.p->0//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
VS0
;[o]
VS0
:[font = input; preserveAspect; startGroup]
vnmosqIsoSV/.ATRules[vnmo0IsoSV]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
Vnmo0^2/(1 - y)
;[o]
     2
Vnmo0
------
1 - y
:[font = subsubsection; inactive; preserveAspect; startGroup]
phase speed as function of p
:[font = input; preserveAspect; startGroup]
vsqIsoSV = Vsq[QIsoSV, {p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VS0^2
;[o]
   2
VS0
:[font = input; preserveAspect; startGroup]
vIsoSV = vsqIsoSV//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VS0
;[o]
VS0
:[font = subsubsection; inactive; preserveAspect; startGroup]
group velocity
:[font = input; preserveAspect; startGroup]
gIsoSV = GVel[evalIsoSV, QIsoSV, {p, q}]
:[font = output; output; inactive; preserveAspect; endGroup]
{p*VS0^2, 0, VS0*(1 - p^2*VS0^2)^(1/2)}
;[o]
      2                   2    2
{p VS0 , 0, VS0 Sqrt[1 - p  VS0 ]}
:[font = input; preserveAspect; startGroup]
%/.zRule[vIsoSV]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{VS0*z^(1/2), 0, VS0*(1 - z)^(1/2)}
;[o]
{VS0 Sqrt[z], 0, VS0 Sqrt[1 - z]}
:[font = subsubsection; inactive; preserveAspect; startGroup]
polarization vector as function of p
:[font = input; preserveAspect; startGroup]
polIsoSV = evecIsoSV/.q->QIsoSV//
Simplify//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
{-(1 - p^2*VS0^2)^(1/2), 0, p*VS0}
;[o]
            2    2
{-Sqrt[1 - p  VS0 ], 0, p VS0}
:[font = input; preserveAspect; startGroup]
%/. zRule[vIsoSV]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{-(1 - z)^(1/2), 0, z^(1/2)}
;[o]
{-Sqrt[1 - z], 0, Sqrt[z]}
:[font = subsubsection; inactive; preserveAspect; startGroup]
phase speed as function of theta
:[font = input; preserveAspect; startGroup]
lambda[theta_] = evalIsoSV/.nRules2D//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
rho*VS0^2
;[o]
       2
rho VS0
:[font = input; preserveAspect; startGroup]
vsqAngleIsoSV = lambda[theta]/rho
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VS0^2
;[o]
   2
VS0
:[font = subsubsection; inactive; preserveAspect; startGroup]
group velocity as function of theta
:[font = input; preserveAspect; startGroup]
gIsoSV/. p -> Sin[theta]/Sqrt@vsqAngleIsoSV//
CosineForm//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{VS0*Sin[theta], 0, QIsoSV*VS0^2}
;[o]
                              2
{VS0 Sin[theta], 0, QIsoSV VS0 }
:[font = subsubsection; inactive; preserveAspect; startGroup]
polarization as a function of theta
:[font = input; preserveAspect; startGroup]
polIsoSV/. p -> Sin[theta]/Sqrt@vsqAngleIsoSV//
CosineForm//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
{-Cos[theta], 0, Sin[theta]}
;[o]
{-Cos[theta], 0, Sin[theta]}
:[font = subsection; inactive; Cclosed; preserveAspect; startGroup]
SH wave
:[font = subsubsection; inactive; preserveAspect; startGroup]
mode
:[font = input; preserveAspect; startGroup]
evalIsoSH = evalIso[[3]]
:[font = output; output; inactive; preserveAspect; endGroup]
(p^2 + q^2)*rho*VS0^2
;[o]
  2    2         2
(p  + q ) rho VS0
:[font = input; preserveAspect; startGroup]
evecIso[[3]]
:[font = output; output; inactive; preserveAspect; endGroup]
{0, 1, 0}
;[o]
{0, 1, 0}
:[font = input; preserveAspect]
evecIsoSH = %;
:[font = text; inactive; preserveAspect; endGroup]
Hence only the polarization is different from the SV mode
:[font = subsubsection; inactive; preserveAspect; startGroup]
q(p)
:[font = input; preserveAspect; startGroup]
solnRules = Solve[evalIsoSH == rho,q]
:[font = output; output; inactive; preserveAspect; endGroup]
{{q -> -((1 - p^2*VS0^2)^(1/2)/VS0)}, 
 
  {q -> (1 - p^2*VS0^2)^(1/2)/VS0}}
;[o]
                   2    2
         Sqrt[1 - p  VS0 ]
{{q -> -(-----------------)}, 
                VS0
 
                  2    2
        Sqrt[1 - p  VS0 ]
  {q -> -----------------}}
               VS0
:[font = input; preserveAspect; startGroup]
QIsoSH = q/.First@Rest@solnRules
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
(1 - p^2*VS0^2)^(1/2)/VS0
;[o]
          2    2
Sqrt[1 - p  VS0 ]
-----------------
       VS0
:[font = subsubsection; inactive; preserveAspect; startGroup]
moveout function as function of p
:[font = input; preserveAspect; startGroup]
vnmosqIsoSH = VNMOsq[QIsoSH, {p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VS0^2/(1 - p^2*VS0^2)
;[o]
      2
   VS0
-----------
     2    2
1 - p  VS0
:[font = input; preserveAspect; startGroup]
vnmo0IsoSH = vnmosqIsoSH/.p->0//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
VS0
;[o]
VS0
:[font = input; preserveAspect; startGroup]
vnmosqIsoSH/.ATRules[vnmo0IsoSH]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
Vnmo0^2/(1 - y)
;[o]
     2
Vnmo0
------
1 - y
:[font = subsubsection; inactive; preserveAspect; startGroup]
phase speed as function of p
:[font = input; preserveAspect; startGroup]
vsqIsoSH = Vsq[QIsoSH, {p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VS0^2
;[o]
   2
VS0
:[font = input; preserveAspect; startGroup]
vIsoSH = vsqIsoSH//Sqrt//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VS0
;[o]
VS0
:[font = subsubsection; inactive; preserveAspect; startGroup]
group velocity
:[font = input; preserveAspect; startGroup]
gIsoSH = GVel[evalIsoSH, QIsoSH, {p, q}]
:[font = output; output; inactive; preserveAspect; endGroup]
{p*VS0^2, 0, VS0*(1 - p^2*VS0^2)^(1/2)}
;[o]
      2                   2    2
{p VS0 , 0, VS0 Sqrt[1 - p  VS0 ]}
:[font = input; preserveAspect; startGroup]
%/.zRule[vIsoSH]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{VS0*z^(1/2), 0, VS0*(1 - z)^(1/2)}
;[o]
{VS0 Sqrt[z], 0, VS0 Sqrt[1 - z]}
:[font = subsubsection; inactive; preserveAspect; startGroup]
polarization vector as function of p
:[font = input; preserveAspect; startGroup]
polIsoSH = evecIsoSH/.q->QIsoSH//
Simplify//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{0, 1, 0}
;[o]
{0, 1, 0}
:[font = subsubsection; inactive; preserveAspect; startGroup]
phase speed as function of theta
:[font = input; preserveAspect; startGroup]
lambda[theta_] = evalIsoSH/.nRules2D//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
rho*VS0^2
;[o]
       2
rho VS0
:[font = input; preserveAspect; startGroup]
vsqAngleIsoSH = lambda[theta]/rho
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
VS0^2
;[o]
   2
VS0
:[font = subsubsection; inactive; preserveAspect; startGroup]
group velocity as function of theta
:[font = input; preserveAspect; startGroup]
gIsoSH/. p -> Sin[theta]/Sqrt@vsqAngleIsoSH//
CosineForm//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{VS0*Sin[theta], 0, VS0*Cos[theta]}
;[o]
{VS0 Sin[theta], 0, VS0 Cos[theta]}
:[font = subsubsection; inactive; preserveAspect; startGroup]
polarization as a function of theta
:[font = input; preserveAspect; startGroup]
polIsoSH/. p -> Sin[theta]/Sqrt@vsqAngleIsoSH
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup; endGroup; endGroup]
{0, 1, 0}
;[o]
{0, 1, 0}
^*)