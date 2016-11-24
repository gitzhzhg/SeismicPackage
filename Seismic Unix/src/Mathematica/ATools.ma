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
:[font = title; inactive; preserveAspect; rightWrapOffset = 488; startGroup]
ATools Package Examples
:[font = subtitle; inactive; preserveAspect]
Jack K. Cohen, 1996, All Rights Reserved
;[s]
1:0,0;40,-1;
1:1,0,0 ,times,2,24,0,0,0;
:[font = input; preserveAspect]
<<ATools.m
Off[General::spell, General::spell1]
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
General Remarks
:[font = text; inactive; preserveAspect; endGroup]
The following variables are reserved as notation for dyadics and cannot be assigned to:

ii, ij, ik, ji, jj, jk, ki, kj, kk

Also, rho is reserved for density.
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
VNMOsq, Vsq, and GVel
:[font = text; inactive; preserveAspect]
In terms of the eigenvalues, lambda, of the matrix,
	M = Cijkl pj pl,
where pi denotes the ith component of the slowness vector,
the eikonal equation is lambda = rho.  For propagation in
a symmetry plane, the {p1,p2,p3} vector becomes {p,0,q}
and the eikonal equation is solved to get q = q(p).  Then, the Vnmo normal moveout function is determined as
	Vnmo^2(p) = q''/(pq' - q),
where the prime denotes d/dp.
:[font = input; Cclosed; preserveAspect; startGroup]
?VNMOsq
:[font = print; inactive; preserveAspect; endGroup]
VNMOsq[Q[p], {p}]

 Computes the square of the moveout function in a
   symmetry plane as a function of ray parameter,
   given the vertical slowness, Q[p]. 
:[font = input; Cclosed; preserveAspect; startGroup]
Q[p_] := Sqrt[1 - p^2 VP0^2]/VP0
vnmosq = VNMOsq[Q[p],{p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VP0^2/(1 - p^2*VP0^2)
;[o]
      2
   VP0
-----------
     2    2
1 - p  VP0
:[font = input; Cclosed; preserveAspect; startGroup]
vnmo = Sqrt@vnmosq//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
VP0/(1 - p^2*VP0^2)^(1/2)
;[o]
       VP0
-----------------
          2    2
Sqrt[1 - p  VP0 ]
:[font = text; inactive; preserveAspect]
Alternatively, if we just use a simple variable for q, use the following syntax:
:[font = input; Cclosed; preserveAspect; startGroup]
Qp = Sqrt[1 - p^2 VP0^2]/VP0;
vnmosq = VNMOsq[Qp,{p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VP0^2/(1 - p^2*VP0^2)
;[o]
      2
   VP0
-----------
     2    2
1 - p  VP0
:[font = input; Cclosed; preserveAspect; startGroup]
?Vsq
:[font = print; inactive; preserveAspect; endGroup]
Vsq[Q[p], {p}]

 Computes the squared phase speed in a symmetry
   plane as a function of ray parameter, given the
   vertical slowness, Q[p]. 
:[font = input; Cclosed; preserveAspect; startGroup]
vsq = Vsq[Q[p],{p}]
:[font = output; output; inactive; preserveAspect; endGroup]
VP0^2
;[o]
   2
VP0
:[font = input; Cclosed; preserveAspect; startGroup]
v = Sqrt@vsq//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
VP0
;[o]
VP0
:[font = input; Cclosed; preserveAspect; startGroup]
?GVel
:[font = print; inactive; preserveAspect; endGroup]
GVel[Eval[p,q], Q[p], {p, q}]

 Computes the group velocity in a symmetry plane as
   a function of ray parameter.  Here, Eval[p,q] is
   the eigenvalue of the Christoffel matrix in
   slowness, and Q[p] is the vertical slowness. 
:[font = input; Cclosed; preserveAspect; startGroup]
lambda[p_,q_] := rho VP0^2 (p^2 + q^2);
GVel[lambda[p,q], Q[p], {p, q}]
:[font = output; output; inactive; preserveAspect; endGroup]
{p*VP0^2, 0, VP0*(1 - p^2*VP0^2)^(1/2)}
;[o]
      2                   2    2
{p VP0 , 0, VP0 Sqrt[1 - p  VP0 ]}
:[font = input; Cclosed; preserveAspect; startGroup]
lam = rho VP0^2 (p^2 + q^2);
GVel[lam, Q[p], {p, q}]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{p*VP0^2, 0, VP0*(1 - p^2*VP0^2)^(1/2)}
;[o]
      2                   2    2
{p VP0 , 0, VP0 Sqrt[1 - p  VP0 ]}
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
UniformSeries
:[font = text; inactive; preserveAspect]
This function provides a way to expand functions simultaneously  in several variables all to be treated as equally small.  In this next example, epsilon and delta are the small variables:
:[font = input; Cclosed; preserveAspect; startGroup]
?UniformSeries
:[font = print; inactive; preserveAspect; endGroup]
UniformSeries[expression, {variables}, (order)]

 General Utility to transform the expression in a
   series where the list of variables in the second
   argument are treated as small, all of the same
   size order.  The order parameter gives the order
   to keep in the small variables and defaults to 1.
:[font = input; Cclosed; preserveAspect; startGroup]
expr = Sqrt[1 + epsilon + delta];
UniformSeries[expr,{epsilon, delta}]
:[font = output; output; inactive; preserveAspect; endGroup]
(2 + delta + epsilon)/2
;[o]
2 + delta + epsilon
-------------------
         2
:[font = text; inactive; preserveAspect]
UniformSeries can be easily specialized to avoid repeatedly mentioning the variables:
:[font = input; preserveAspect]
WeakTI[expression_, n_:1] :=
	UniformSeries[expression, {epsilon, delta}, n]
:[font = input; Cclosed; preserveAspect; startGroup]
WeakTI[expr]
:[font = output; output; inactive; preserveAspect; endGroup]
(2 + delta + epsilon)/2
;[o]
2 + delta + epsilon
-------------------
         2
:[font = input; Cclosed; preserveAspect; startGroup]
WeakTI[expr,2]
:[font = output; output; inactive; preserveAspect; endGroup]
1 + (delta + epsilon)/2 - (delta + epsilon)^2/8
;[o]
                                       2
    delta + epsilon   (delta + epsilon)
1 + --------------- - ------------------
           2                  8
:[font = text; inactive; preserveAspect]
However, a more general form of WeakTI is available in the VTI.m package.  As a more typical example of using UniformSeries in standalone mode, consider a  study of variable anisotropy
:[font = input; Cclosed; preserveAspect; startGroup]
delta = delta0 + delta1 z;
epsilon = epsilon0 + epsilon1 z;

WeakLimit[expression_, n_:1] :=
	UniformSeries[expression,
		{epsilon0, delta0, epsilon1, delta1}, n]
		
Sqrt[1 + epsilon + delta]//WeakLimit
:[font = output; output; inactive; preserveAspect; endGroup]
(2 + delta0 + epsilon0 + delta1*z + epsilon1*z)/2
;[o]
2 + delta0 + epsilon0 + delta1 z + epsilon1 z
---------------------------------------------
                      2
:[font = text; inactive; preserveAspect; endGroup]
Caution:  If you have also loaded the VTI package, epsilon and delta are reserved variables and cannot be assigned to.  Thus, you'd have to do something like:
  Delta = delta0 + delta1 z;
etc.
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
Matrix Elements via Dyadics
:[font = text; inactive; preserveAspect]
The predefined dyadics, ii, ij, ik, ji, jj, jk, ki, kj, kk, give a convenient way to set up 3x3 matrices
:[font = input; Cclosed; preserveAspect; startGroup]
?ij
:[font = print; inactive; preserveAspect; endGroup]
Dyadic representing the (1,2) matrix element
:[font = input; Cclosed; preserveAspect; startGroup]
MIso =
(C33 p1^2 + C55(p2^2 + p3^2))ii +
(C33 p2^2 + C55(p3^2 + p1^2))jj +
(C33 p3^2 + C55(p1^2 + p2^2))kk +
(C33 - C55)p1 p2 (ij+ji) +
(C33 - C55)p2 p3 (jk+kj) +
(C33 - C55)p3 p1 (ki+ik);
Eigenvalues@MIso
:[font = output; output; inactive; preserveAspect; endGroup]
{C33*(p1^2 + p2^2 + p3^2), 
 
  C55*(p1^2 + p2^2 + p3^2), 
 
  C55*(p1^2 + p2^2 + p3^2)}
;[o]
        2     2     2          2     2     2
{C33 (p1  + p2  + p3 ), C55 (p1  + p2  + p3 ), 
 
         2     2     2
  C55 (p1  + p2  + p3 )}
:[font = input; Cclosed; preserveAspect; startGroup]
?DyadicForm
:[font = print; inactive; preserveAspect; endGroup]
DyadicForm[matrix]

 Write a 3x3 matrix in the dyadic form: a11 ii + a12
   ij + .... 
:[font = input; Cclosed; preserveAspect; startGroup]
M = {{2n1^2, n1 n2, 0}, {n1 n2, 0, 0}, {0, 0, 0}};
Mdyad = M//DyadicForm
:[font = output; output; inactive; preserveAspect; endGroup]
2*n1^2*HoldForm[ii] + n1*n2*HoldForm[ij] + 
 
  n1*n2*HoldForm[ji]
;[o]
    2
2 n1  ii + n1 n2 ij + n1 n2 ji
:[font = text; inactive; preserveAspect; rightWrapOffset = 539]
DyadicForm is only a display mechanism, you have to use the  built-in function ReleaseHold to operate on the output as a matrix again:
:[font = input; Cclosed; preserveAspect; startGroup]
Eigenvalues@Mdyad
:[font = output; output; inactive; preserveAspect; endGroup]
Eigenvalues[2*n1^2*HoldForm[ii] + 
 
   n1*n2*HoldForm[ij] + n1*n2*HoldForm[ji]]
;[o]
                2
Eigenvalues[2 n1  ii + n1 n2 ij + n1 n2 ji]
:[font = input; Cclosed; preserveAspect; startGroup]
Eigenvalues@ReleaseHold@Mdyad//Simplify//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{0, n1^2 - n1*(n1^2 + n2^2)^(1/2), 
 
  n1^2 + n1*(n1^2 + n2^2)^(1/2)}
;[o]
      2             2     2
{0, n1  - n1 Sqrt[n1  + n2 ], 
 
    2             2     2
  n1  + n1 Sqrt[n1  + n2 ]}
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
SineForm and CosineForm
:[font = text; inactive; preserveAspect; rightWrapOffset = 553]
Mathematica's canonical form for Trig expressions often make sense:
:[font = input; Cclosed; preserveAspect; startGroup]
expr = (Cos[theta]^2 - Sin[theta]^2)//Simplify
:[font = output; output; inactive; preserveAspect; endGroup]
Cos[2*theta]
;[o]
Cos[2 theta]
:[font = text; inactive; preserveAspect; rightWrapOffset = 547]
But if we want to see the small theta behavior, then SineForm is better:
:[font = input; Cclosed; preserveAspect; startGroup]
?SineForm
:[font = print; inactive; preserveAspect; endGroup]
SineForm[expression]

 Write trigonometric expression in powers of sine
   eliminating even powers of cosines and all
   multiple angles. 
:[font = input; Cclosed; preserveAspect; startGroup]
expr//SineForm
:[font = output; output; inactive; preserveAspect; endGroup]
1 - 2*Sin[theta]^2
;[o]
                2
1 - 2 Sin[theta]
:[font = text; inactive; preserveAspect]
Similarly, sometimes we prefer to emphasize cosines:
:[font = input; Cclosed; preserveAspect; startGroup]
?CosineForm
:[font = print; inactive; preserveAspect; endGroup]
CosineForm[expression]

 Write trigonometric expression in powers of cosine
   eliminating even powers of sines and all multiple
   angles. 
:[font = input; Cclosed; preserveAspect; startGroup]
expr//CosineForm
:[font = output; output; inactive; preserveAspect; endGroup]
-1 + 2*Cos[theta]^2
;[o]
                 2
-1 + 2 Cos[theta]
:[font = text; inactive; preserveAspect; endGroup]
Often the human must negotiate between the three forms to obtain a fourth form most suited to the purpose 
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
FactorTrig
:[font = text; inactive; preserveAspect]
The built-in Factor utility goes too far with trig expressions, producing linear factors that we'd rather not see:
:[font = input; Cclosed; preserveAspect; startGroup]
exp = Sin[theta]^2 Sin[phi]^2 - Sin[theta]^4 Sin[phi]^2;
Factor[exp]
:[font = output; output; inactive; preserveAspect; endGroup]
Sin[phi]^2*(1 - Sin[theta])*Sin[theta]^2*
 
  (1 + Sin[theta])
;[o]
        2                            2
Sin[phi]  (1 - Sin[theta]) Sin[theta]  (1 + Sin[theta])
:[font = text; inactive; preserveAspect]
FactorTrig reassembles the linear factors appropriately:
:[font = input; Cclosed; preserveAspect; startGroup]
?FactorTrig
:[font = print; inactive; preserveAspect; endGroup]
FactorTrig[expression]

 Factor trigonometric expression eliminating the
   factors (1-Cos[A])(1+Cos[A]) and
   (1-Sin[A])(1+Sin[A]) in favor of Sin[A]^2 and
   Cos[A]^2 respectively. 
:[font = input; Cclosed; preserveAspect; startGroup]
FactorTrig[exp]
:[font = output; output; inactive; preserveAspect; endGroup]
Cos[theta]^2*Sin[phi]^2*Sin[theta]^2
;[o]
          2         2           2
Cos[theta]  Sin[phi]  Sin[theta]
:[font = input; Cclosed; preserveAspect; startGroup]
cosexp = exp//CosineForm
:[font = output; output; inactive; preserveAspect; endGroup]
Cos[theta]^2 - Cos[phi]^2*Cos[theta]^2 - 
 
  Cos[theta]^4 + Cos[phi]^2*Cos[theta]^4
;[o]
          2           2           2             4
Cos[theta]  - Cos[phi]  Cos[theta]  - Cos[theta]  + 
 
          2           4
  Cos[phi]  Cos[theta]
:[font = input; Cclosed; preserveAspect; startGroup]
cosexp//Factor
:[font = output; output; inactive; preserveAspect; endGroup]
(-1 + Cos[phi])*(1 + Cos[phi])*(-1 + Cos[theta])*
 
  Cos[theta]^2*(1 + Cos[theta])
;[o]
(-1 + Cos[phi]) (1 + Cos[phi]) (-1 + Cos[theta]) 
 
            2
  Cos[theta]  (1 + Cos[theta])
:[font = input; Cclosed; preserveAspect; startGroup]
cosexp//FactorTrig
:[font = output; output; inactive; preserveAspect; endGroup]
Cos[theta]^2*Sin[phi]^2*Sin[theta]^2
;[o]
          2         2           2
Cos[theta]  Sin[phi]  Sin[theta]
:[font = input; Cclosed; preserveAspect; startGroup]
sqexp = (1 - Sin[x]^2)^2
:[font = output; output; inactive; preserveAspect; endGroup]
(1 - Sin[x]^2)^2
;[o]
           2 2
(1 - Sin[x] )
:[font = input; Cclosed; preserveAspect; startGroup]
sqexp//FactorTrig
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
Cos[x]^4
;[o]
      4
Cos[x]
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
Norm
:[font = text; inactive; preserveAspect]
Norm gives the norm of  a vector:
:[font = input; Cclosed; preserveAspect; startGroup]
?Norm
:[font = print; inactive; preserveAspect; endGroup]
Norm[vector]

 Compute the Euclidean norm of a vector (i.e.,
   list). 
:[font = input; Cclosed; preserveAspect; startGroup]
v = {p,0,q};
Norm[v]
:[font = output; output; inactive; preserveAspect; endGroup]
(p^2 + q^2)^(1/2)
;[o]
      2    2
Sqrt[p  + q ]
:[font = input; Cclosed; preserveAspect; startGroup]
Norm[{1,1,1,1}]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
2
;[o]
2
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
HalfPower
:[font = text; inactive; preserveAspect]
In solving equations involving squares. it is often convenient to suppress the square powers, thus effectively lowering the order of the equation to be solved.
:[font = input; Cclosed; preserveAspect; startGroup]
?HalfPower
:[font = print; inactive; preserveAspect; endGroup]
HalfPower[expression, x, xsquare]

 Write rational expression in x in powers of xsquare
   = x^2, thus eliminating even powers of x. 
:[font = input; Cclosed; preserveAspect; startGroup]
HalfPower[p^4 + 3 q^2 p^5, p, psq]
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
psq^2 + 3*p*psq^2*q^2
;[o]
   2          2  2
psq  + 3 p psq  q
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
IsolateSqrt
:[font = input; Cclosed; preserveAspect; startGroup]
?IsolateSqrt
:[font = print; inactive; preserveAspect; endGroup]
IsolateSqrt[expression]

 For an expression of the form A + B Sqrt[C], return
   the list {A, B, C}.  Useful for subsequent
   simplification of A, B and especially C.  Caveat:
   An expression of the form A + B Sqrt[C] + D
   Sqrt[E] with C different from E is not of the
   required form. 
:[font = text; inactive; preserveAspect]
In the following expression, we can see that rho VP0^2 is a common factor, but Simplify and PowerExpand can't get at it directly.   In such cases IsolateSqrt may be of help.
:[font = input; preserveAspect]
phase =
	rho VP0^2 stuff1 + 
		stuff2 Sqrt[rho^2 VP0^4 stuff3 +
			rho^4 VP0^6 stuff4];
:[font = input; Cclosed; preserveAspect; startGroup]
Simplify[phase]//PowerExpand
:[font = output; output; inactive; preserveAspect; endGroup]
rho*stuff1*VP0^2 + stuff2*
 
   (rho^2*stuff3*VP0^4 + rho^4*stuff4*VP0^6)^(1/2)
;[o]
              2
rho stuff1 VP0  + stuff2 
 
           2           4      4           6
   Sqrt[rho  stuff3 VP0  + rho  stuff4 VP0 ]
:[font = input; Cclosed; preserveAspect; startGroup]
{a, b, c} = IsolateSqrt[phase]
:[font = output; output; inactive; preserveAspect; endGroup]
{rho*stuff1*VP0^2, stuff2, 
 
  rho^2*stuff3*VP0^4 + rho^4*stuff4*VP0^6}
;[o]
               2
{rho stuff1 VP0 , stuff2, 
 
     2           4      4           6
  rho  stuff3 VP0  + rho  stuff4 VP0 }
:[font = text; inactive; preserveAspect]
Now it is easy to isolate (say) stuff3 and proceed to simplify it if it is a complicated quantity:
:[font = input; Cclosed; preserveAspect; startGroup]
c1 = c/(rho^2 VP0^4)//Apart

:[font = output; output; inactive; preserveAspect; endGroup]
stuff3 + rho^2*stuff4*VP0^2
;[o]
            2           2
stuff3 + rho  stuff4 VP0
:[font = text; inactive; preserveAspect]
Some other examples:
:[font = input; Cclosed; preserveAspect; startGroup]
Clear[a,b,c,d,e,z]
a + b + c + d Sqrt[z] + e Sqrt[z]//IsolateSqrt
:[font = output; output; inactive; preserveAspect; endGroup]
{a + b + c, d + e, z}
;[o]
{a + b + c, d + e, z}
:[font = input; Cclosed; preserveAspect; startGroup]
3(a + 2 b Sqrt[c^2 + d^2])//IsolateSqrt
:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
{3*a, 6*b, c^2 + d^2}
;[o]
            2    2
{3 a, 6 b, c  + d }
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
GrayLevels
:[font = text; inactive; preserveAspect]
GrayLevels is a convenience option to plot several curves in successively lighter shades 
:[font = input; Cclosed; preserveAspect; startGroup]
?GrayLevels
:[font = print; inactive; preserveAspect; endGroup]
A Plot option: PlotStyle -> GrayLevels[n]

 Makes n equally spaced gray levels. 
:[font = input; Cclosed; preserveAspect; startGroup]
Plot[{Sin[x]^2, Cos[x]^2, Cos[2x]}, {x, 0, Pi},
	PlotStyle->GrayLevels[3]]
:[font = postscript; PostScript; formatAsPostScript; output; inactive; preserveAspect; pictureLeft = 34; pictureWidth = 281.75; pictureHeight = 174.125]
%!
%%Creator: Mathematica
%%AspectRatio: .61803 
MathPictureStart
%% Graphics
/Courier findfont 10  scalefont  setfont
% Scaling calculations
0.0238095 0.303152 0.309017 0.294302 [
[(0.5)] .17539 .30902 0 2 Msboxa
[(1)] .32696 .30902 0 2 Msboxa
[(1.5)] .47854 .30902 0 2 Msboxa
[(2)] .63011 .30902 0 2 Msboxa
[(2.5)] .78169 .30902 0 2 Msboxa
[(3)] .93327 .30902 0 2 Msboxa
[(-1)] .01131 .01472 1 0 Msboxa
[(-0.5)] .01131 .16187 1 0 Msboxa
[(0.5)] .01131 .45617 1 0 Msboxa
[(1)] .01131 .60332 1 0 Msboxa
[ -0.001 -0.001 0 0 ]
[ 1.001 .61903 0 0 ]
] MathScale
% Start of Graphics
1 setlinecap
1 setlinejoin
newpath
[ ] 0 setdash
0 g
p
p
.002 w
.17539 .30902 m
.17539 .31527 L
s
P
[(0.5)] .17539 .30902 0 2 Mshowa
p
.002 w
.32696 .30902 m
.32696 .31527 L
s
P
[(1)] .32696 .30902 0 2 Mshowa
p
.002 w
.47854 .30902 m
.47854 .31527 L
s
P
[(1.5)] .47854 .30902 0 2 Mshowa
p
.002 w
.63011 .30902 m
.63011 .31527 L
s
P
[(2)] .63011 .30902 0 2 Mshowa
p
.002 w
.78169 .30902 m
.78169 .31527 L
s
P
[(2.5)] .78169 .30902 0 2 Mshowa
p
.002 w
.93327 .30902 m
.93327 .31527 L
s
P
[(3)] .93327 .30902 0 2 Mshowa
p
.001 w
.05412 .30902 m
.05412 .31277 L
s
P
p
.001 w
.08444 .30902 m
.08444 .31277 L
s
P
p
.001 w
.11476 .30902 m
.11476 .31277 L
s
P
p
.001 w
.14507 .30902 m
.14507 .31277 L
s
P
p
.001 w
.2057 .30902 m
.2057 .31277 L
s
P
p
.001 w
.23602 .30902 m
.23602 .31277 L
s
P
p
.001 w
.26633 .30902 m
.26633 .31277 L
s
P
p
.001 w
.29665 .30902 m
.29665 .31277 L
s
P
p
.001 w
.35728 .30902 m
.35728 .31277 L
s
P
p
.001 w
.38759 .30902 m
.38759 .31277 L
s
P
p
.001 w
.41791 .30902 m
.41791 .31277 L
s
P
p
.001 w
.44822 .30902 m
.44822 .31277 L
s
P
p
.001 w
.50885 .30902 m
.50885 .31277 L
s
P
p
.001 w
.53917 .30902 m
.53917 .31277 L
s
P
p
.001 w
.56948 .30902 m
.56948 .31277 L
s
P
p
.001 w
.5998 .30902 m
.5998 .31277 L
s
P
p
.001 w
.66043 .30902 m
.66043 .31277 L
s
P
p
.001 w
.69074 .30902 m
.69074 .31277 L
s
P
p
.001 w
.72106 .30902 m
.72106 .31277 L
s
P
p
.001 w
.75137 .30902 m
.75137 .31277 L
s
P
p
.001 w
.81201 .30902 m
.81201 .31277 L
s
P
p
.001 w
.84232 .30902 m
.84232 .31277 L
s
P
p
.001 w
.87264 .30902 m
.87264 .31277 L
s
P
p
.001 w
.90295 .30902 m
.90295 .31277 L
s
P
p
.001 w
.96358 .30902 m
.96358 .31277 L
s
P
p
.001 w
.9939 .30902 m
.9939 .31277 L
s
P
p
.002 w
0 .30902 m
1 .30902 L
s
P
p
.002 w
.02381 .01472 m
.03006 .01472 L
s
P
[(-1)] .01131 .01472 1 0 Mshowa
p
.002 w
.02381 .16187 m
.03006 .16187 L
s
P
[(-0.5)] .01131 .16187 1 0 Mshowa
p
.002 w
.02381 .45617 m
.03006 .45617 L
s
P
[(0.5)] .01131 .45617 1 0 Mshowa
p
.002 w
.02381 .60332 m
.03006 .60332 L
s
P
[(1)] .01131 .60332 1 0 Mshowa
p
.001 w
.02381 .04415 m
.02756 .04415 L
s
P
p
.001 w
.02381 .07358 m
.02756 .07358 L
s
P
p
.001 w
.02381 .10301 m
.02756 .10301 L
s
P
p
.001 w
.02381 .13244 m
.02756 .13244 L
s
P
p
.001 w
.02381 .1913 m
.02756 .1913 L
s
P
p
.001 w
.02381 .22073 m
.02756 .22073 L
s
P
p
.001 w
.02381 .25016 m
.02756 .25016 L
s
P
p
.001 w
.02381 .27959 m
.02756 .27959 L
s
P
p
.001 w
.02381 .33845 m
.02756 .33845 L
s
P
p
.001 w
.02381 .36788 m
.02756 .36788 L
s
P
p
.001 w
.02381 .39731 m
.02756 .39731 L
s
P
p
.001 w
.02381 .42674 m
.02756 .42674 L
s
P
p
.001 w
.02381 .4856 m
.02756 .4856 L
s
P
p
.001 w
.02381 .51503 m
.02756 .51503 L
s
P
p
.001 w
.02381 .54446 m
.02756 .54446 L
s
P
p
.001 w
.02381 .57389 m
.02756 .57389 L
s
P
p
.002 w
.02381 0 m
.02381 .61803 L
s
P
P
0 0 m
1 0 L
1 .61803 L
0 .61803 L
closepath
clip
newpath
p
p
p
.004 w
.02381 .30902 m
.02505 .30902 L
.02629 .30904 L
.02753 .30906 L
.02877 .3091 L
.03001 .30914 L
.03125 .30919 L
.03373 .30933 L
.03621 .30951 L
.03869 .30973 L
.04365 .31028 L
.04861 .31098 L
.05357 .31184 L
.06349 .31403 L
.08333 .32022 L
.10317 .32873 L
.14286 .35212 L
.18254 .38259 L
.22222 .41808 L
.2619 .45617 L
.30159 .49425 L
.34127 .52974 L
.38095 .56022 L
.40079 .57291 L
.42063 .5836 L
.44048 .59212 L
.4504 .59551 L
.46032 .5983 L
.47024 .60049 L
.4752 .60135 L
.48016 .60206 L
.48512 .60261 L
.4876 .60283 L
.49008 .603 L
.49256 .60314 L
.4938 .6032 L
.49504 .60324 L
.49628 .60327 L
.49752 .6033 L
.49876 .60331 L
.5 .60332 L
.50124 .60331 L
.50248 .6033 L
.50372 .60327 L
.50496 .60324 L
.5062 .6032 L
.50744 .60314 L
.50992 .603 L
.5124 .60283 L
.51488 .60261 L
Mistroke
.51984 .60206 L
.5248 .60135 L
.52976 .60049 L
.53968 .5983 L
.55952 .59212 L
.57937 .5836 L
.61905 .56022 L
.65873 .52974 L
.69841 .49425 L
.7381 .45617 L
.77778 .41808 L
.81746 .38259 L
.85714 .35212 L
.87698 .33943 L
.89683 .32873 L
.91667 .32022 L
.93651 .31403 L
.94643 .31184 L
.95139 .31098 L
.95635 .31028 L
.96131 .30973 L
.96379 .30951 L
.96627 .30933 L
.96875 .30919 L
.96999 .30914 L
.97123 .3091 L
.97247 .30906 L
.97371 .30904 L
.97495 .30902 L
.97619 .30902 L
Mfstroke
P
P
p
.333 g
p
.004 w
.02381 .60332 m
.02505 .60331 L
.02629 .6033 L
.02753 .60327 L
.02877 .60324 L
.03001 .6032 L
.03125 .60314 L
.03373 .603 L
.03621 .60283 L
.03869 .60261 L
.04365 .60206 L
.04861 .60135 L
.05357 .60049 L
.06349 .5983 L
.08333 .59212 L
.10317 .5836 L
.14286 .56022 L
.18254 .52974 L
.22222 .49425 L
.2619 .45617 L
.30159 .41808 L
.34127 .38259 L
.38095 .35212 L
.40079 .33943 L
.42063 .32873 L
.44048 .32022 L
.4504 .31683 L
.46032 .31403 L
.47024 .31184 L
.4752 .31098 L
.48016 .31028 L
.48512 .30973 L
.4876 .30951 L
.49008 .30933 L
.49256 .30919 L
.4938 .30914 L
.49504 .3091 L
.49628 .30906 L
.49752 .30904 L
.49876 .30902 L
.5 .30902 L
.50124 .30902 L
.50248 .30904 L
.50372 .30906 L
.50496 .3091 L
.5062 .30914 L
.50744 .30919 L
.50992 .30933 L
.5124 .30951 L
.51488 .30973 L
Mistroke
.51984 .31028 L
.5248 .31098 L
.52976 .31184 L
.53968 .31403 L
.55952 .32022 L
.57937 .32873 L
.61905 .35212 L
.65873 .38259 L
.69841 .41808 L
.7381 .45617 L
.77778 .49425 L
.81746 .52974 L
.85714 .56022 L
.87698 .57291 L
.89683 .5836 L
.91667 .59212 L
.93651 .5983 L
.94643 .60049 L
.95139 .60135 L
.95635 .60206 L
.96131 .60261 L
.96379 .60283 L
.96627 .603 L
.96875 .60314 L
.96999 .6032 L
.97123 .60324 L
.97247 .60327 L
.97371 .6033 L
.97495 .60331 L
.97619 .60332 L
Mfstroke
P
P
p
.667 g
p
.004 w
.02381 .60332 m
.02505 .60331 L
.02629 .60328 L
.02753 .60323 L
.02877 .60316 L
.03001 .60307 L
.03125 .60296 L
.03373 .60269 L
.03621 .60233 L
.03869 .6019 L
.04365 .6008 L
.04861 .59939 L
.05357 .59766 L
.06349 .59329 L
.08333 .58092 L
.10317 .56389 L
.14286 .51712 L
.18254 .45617 L
.22222 .38519 L
.2619 .30902 L
.30159 .23285 L
.34127 .16187 L
.38095 .10091 L
.40079 .07553 L
.42063 .05414 L
.44048 .03712 L
.4504 .03033 L
.46032 .02474 L
.47024 .02037 L
.4752 .01865 L
.48016 .01723 L
.48512 .01613 L
.4876 .0157 L
.49008 .01535 L
.49256 .01507 L
.4938 .01496 L
.49504 .01487 L
.49628 .0148 L
.49752 .01475 L
.49876 .01472 L
.5 .01472 L
.50124 .01472 L
.50248 .01475 L
.50372 .0148 L
.50496 .01487 L
.5062 .01496 L
.50744 .01507 L
.50992 .01535 L
.5124 .0157 L
.51488 .01613 L
Mistroke
.51984 .01723 L
.5248 .01865 L
.52976 .02037 L
.53968 .02474 L
.55952 .03712 L
.57937 .05414 L
.61905 .10091 L
.65873 .16187 L
.69841 .23285 L
.7381 .30902 L
.77778 .38519 L
.81746 .45617 L
.85714 .51712 L
.87698 .5425 L
.89683 .56389 L
.91667 .58092 L
.93651 .59329 L
.94643 .59766 L
.95139 .59939 L
.95635 .6008 L
.96131 .6019 L
.96379 .60233 L
.96627 .60269 L
.96875 .60296 L
.96999 .60307 L
.97123 .60316 L
.97247 .60323 L
.97371 .60328 L
.97495 .60331 L
.97619 .60332 L
Mfstroke
P
P
P
% End of Graphics
MathPictureEnd

:[font = output; output; inactive; preserveAspect; endGroup; endGroup]
Graphics["<<>>"]
;[o]
-Graphics-
:[font = section; inactive; Cclosed; preserveAspect; startGroup]
SeriesOrder
:[font = text; inactive; preserveAspect]
SeriesOrder computes the order of a series and is mostly useful inside other Mathematica 
:[font = input; Cclosed; preserveAspect; startGroup]
?SeriesOrder
:[font = print; inactive; preserveAspect; endGroup]
SeriesOrder[series_expression]

 Compute the order of a series expression. 
:[font = input; Cclosed; preserveAspect; startGroup]
SeriesOrder[Series[Sin[x], {x, 0, 3}]]
:[font = output; output; inactive; preserveAspect; endGroup]
4
;[o]
4
:[font = input; Cclosed; preserveAspect; startGroup]
SeriesOrder[Sin[x] + O[x]^4]
:[font = output; output; inactive; preserveAspect; endGroup]
4
;[o]
4
:[font = input; Cclosed; preserveAspect; startGroup]
expr = Cos[x] + O[x]^8
:[font = output; output; inactive; preserveAspect; endGroup]
SeriesData[x, 0, {1, 0, -1/2, 0, 1/24, 0, -1/720}, 
 
  0, 8, 1]
;[o]
     2    4    6
    x    x    x         8
1 - -- + -- - --- + O[x]
    2    24   720
:[font = input; Cclosed; preserveAspect; startGroup]
HalfPower[Normal[expr],x,y] + O[y]^(SeriesOrder[expr]/2)
:[font = output; output; inactive; preserveAspect; endGroup; endGroup; endGroup]
SeriesData[y, 0, {1, -1/2, 1/24, -1/720}, 0, 4, 1]
;[o]
         2    3
    y   y    y         4
1 - - + -- - --- + O[y]
    2   24   720
^*)