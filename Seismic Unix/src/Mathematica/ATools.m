(* 
ATools.m:
This Package provides a module for expanding expressions in several
small parameters, all of the same order of magnitude.  It also
includes a few other convenience functions.
*)
  
(*
Copyright: Jack K. Cohen, Colorado School of Mines, 1996.  All rights reserved.
*)


BeginPackage["ATools`", "Algebra`Trigonometry`"];

(* Declaration of global variables in the package *)
rho::usage = "rho is the default symbol for density"

ii::usage = "Dyadic representing the (1,1) matrix element"
ij::usage = "Dyadic representing the (1,2) matrix element"
ik::usage = "Dyadic representing the (1,3) matrix element"
ji::usage = "Dyadic representing the (2,1) matrix element"
jj::usage = "Dyadic representing the (2,2) matrix element"
jk::usage = "Dyadic representing the (2,3) matrix element"
ki::usage = "Dyadic representing the (3,1) matrix element"
kj::usage = "Dyadic representing the (3,2) matrix element"
kk::usage = "Dyadic representing the (3,3) matrix element"

(* Declaration of public function names in the package *)
VNMOsq::usage = "VNMOsq[Q[p], {p}]\n\n
Computes the square of the moveout function in a symmetry plane
as a function of ray parameter, given the vertical slowness,
Q[p].
"

VNMOsq3D::usage = "VNMOsq3D[Q[p], {p1, p2}, alpha]\n\n
Computes the squared 3D moveout function as a function
of the horizontal ray parameters, p1 and p2, given the
vertical slowness, Q[p], and the angle, alpha,
between the x1-axis and the symmetry axis.
(Not yet implemented).
"

Vsq::usage = "Vsq[Q[p], {p}]\n\n
Computes the squared phase speed in a symmetry plane as a
function of ray parameter, given the vertical slowness,
Q[p].
"

Vsq3D::usage = "Vsq3D[Q[p], {p1, p2}, alpha]\n\n
Computes the squared 3D phase speed as a function of the
horizontal ray parameters, p1 and p2, given the
vertical slowness, Q[p], and the angle, alpha,
between the x1-axis and the symmetry axis.
(Not yet implemented).
"


GVel::usage = "GVel[Eval[p,q], Q[p], {p, q}]\n\n
Computes the group velocity in a symmetry plane as a
function of ray parameter.  Here, Eval[p,q] is the
eigenvalue of the Christoffel matrix in slowness, and
Q[p] is the vertical slowness.
"

UniformSeries::usage = "UniformSeries[expression, {variables}, (order)]\n\n
General Utility to transform the expression in a series where the
list of variables in the second argument are treated as small, all of
the same size order.  The order parameter gives the order to
keep in the small variables and defaults to 1.
"

DyadicForm::usage="DyadicForm[matrix]\n\n
Write a 3x3 matrix in the dyadic form: a11 ii + a12 ij + ....
"

HalfPower::usage="HalfPower[expression, x, xsquare]\n\n
Write rational expression in x in powers of xsquare = x^2,
thus eliminating even powers of x.
"

IsolateSqrt::usage="IsolateSqrt[expression]\n\n
For an expression of the form A + B Sqrt[C], return
the list {A, B, C}.  Useful for subsequent simplification
of A, B and especially C.  Caveat: An expression of the
form A + B Sqrt[C] + D Sqrt[E] with C different from E
is not of the required form.
"

SineForm::usage="SineForm[expression]\n\n
Write trigonometric expression in powers of sine
eliminating even powers of cosines and all multiple angles.
"

CosineForm::usage="CosineForm[expression]\n\n
Write trigonometric expression in powers of cosine
eliminating even powers of sines and all multiple angles.
"

FactorTrig::usage="FactorTrig[expression]\n\n
Factor trigonometric expression eliminating the factors
(1-Cos[A])(1+Cos[A]) and (1-Sin[A])(1+Sin[A]) in favor of
Sin[A]^2 and Cos[A]^2 respectively.
"

Norm::usage="Norm[vector]\n\n
Compute the Euclidean norm of a vector (i.e., list).
"

SeriesOrder::usage="SeriesOrder[series_expression]\n\n
Compute the order of a series expression.
"

GrayLevels::usage="A Plot option: PlotStyle -> GrayLevels[n]\n\n
Makes n equally spaced gray levels.
"

Begin["`private`"];

(* underlying rules and variables left private *)
SineToCosine =
	Sin[m_. x_]^n_Integer?Positive -> 
		(1 - Cos[m x]^2)^Quotient[n,2] Sin[m x]^Mod[n,2]
CosineToSine =
	Cos[m_. x_]^n_Integer?Positive -> 
		(1 - Sin[m x]^2)^Quotient[n,2] Cos[m x]^Mod[n,2]
FactorTrigRules = {
	(1-Cos[x_])^n_.(1+Cos[x_])^n_.->Sin[x]^(2n),
	(-1+Cos[x_])^n_.(1+Cos[x_])^n_.->(-1)^n Sin[x]^(2n),
	(1-Sin[x_])^n_.(1+Sin[x_])^n_.->Cos[x]^(2n),
	(-1+Sin[x_])^n_.(1+Sin[x_])^n_.->(-1)^n Cos[x]^(2n)
}
XToXsquare[x_,xsquare_] =
       x^n_Integer?Positive -> xsquare^Quotient[n,2] x^Mod[n,2]

sqrtRule = x_. + y_. Sqrt[z_] -> {x, y, z};

e1 = {1,0,0}
e2 = {0,1,0}
e3 = {0,0,1}

(* public function implementations *)
VNMOsq[Q_, {p_}] := VNMOsq[Function[p, Q], p] (* Convert to internal form *)
VNMOsq[Q_, p_] := Q''[p]/(p Q'[p] - Q[p])//Simplify

VNMOsq3D[Q_,p1_,p2_,alpha_] := Print["Not yet implemented"];

Vsq[Q_, {p_}] := Vsq[Function[p, Q], p] (* Convert to internal form *)
Vsq[Q_, p_] := 1/(p^2 + Q[p]^2)//Simplify

GVel[lambda_, Q_, {p_, q_},rho_:rho] :=
	GVel[Function[{p, q}, lambda], Function[p, Q], p, q, rho]
GVel[lambda_, Q_, p_, q_, rho_:rho] :=
	1/(2rho){D[lambda[p,q],p],0,D[lambda[p,q],q]}/.q->Q[p]//Simplify

UniformSeries[expression_, L_List, n_:1] :=
Module[{x, tmp},
	tmp = expression/. Thread[L -> L x];
    Normal@Series[tmp, {x,0,n}] /. x->1//
    Simplify//PowerExpand//Simplify
]

ii = Outer[Times, e1, e1]
ij = Outer[Times, e1, e2]
ik = Outer[Times, e1, e3]
ji = Outer[Times, e2, e1]
jj = Outer[Times, e2, e2]
jk = Outer[Times, e2, e3]
ki = Outer[Times, e3, e1]
kj = Outer[Times, e3, e2]
kk = Outer[Times, e3, e3]

DyadicForm[M_?MatrixQ] :=
M[[1,1]]HoldForm[ii] + M[[1,2]]HoldForm[ij] + M[[1,3]]HoldForm[ik] +
M[[2,1]]HoldForm[ji] + M[[2,2]]HoldForm[jj] + M[[2,3]]HoldForm[jk] +
M[[3,1]]HoldForm[ki] + M[[3,2]]HoldForm[kj] + M[[3,3]]HoldForm[kk]
HalfPower[expression_,x_,xsquare_] := expression/.XToXsquare[x,xsquare]
IsolateSqrt[expression_] :=
Module[{a, tmp, b, c},
	a = expression/.x_. + y_. Power[z_, Rational[1,2]]/;
		FreeQ[x,Rational[1,2]] && FreeQ[y,Rational[1,2]] -> x;
	tmp = expression - a//Simplify;
	b = tmp/.y_. Power[z_, Rational[1,2]]/;
		FreeQ[y,Rational[1,2]] -> y;
	tmp /= b//Simplify;
	c = tmp /.Power[z_, Rational[1,2]] -> z;
	{a,b,c}
]
SineForm[expression_]   := TrigReduce[expression]/.CosineToSine//Expand
CosineForm[expression_] := TrigReduce[expression]/.SineToCosine//Expand
FactorTrig[expression_] := Factor[expression]//.FactorTrigRules
Norm[v_List] := Sqrt[v . v]
GrayLevels[n_] := Array[GrayLevel[(#-1)/n]&, {n}]
SeriesOrder[expression_SeriesData] := expression[[5]]/expression[[6]]

End[];
(*
Protect[
	VNMOsq,
	VNMOsq3D,
	UniformSeries,
	rho,
	ii, ij, ik, ji, jj, jk, ki, kj, kk,
	DyadicForm,
	Norm,
	HalfPower,
	SineForm,
	CosineForm,
	FactorTrig,
	GrayLevels,
	SeriesOrder
]
*)
EndPackage[];
