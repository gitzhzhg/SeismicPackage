(* 
Thomsen.m:
Conversions between tensor and Love notations for tranversely isotropic
media and Thomsen notation.
*)
  
(*
Copyright: Jack K. Cohen, Colorado School of Mines, 1995.  All rights reserved.
*)


BeginPackage["Thomsen`", "Algebra`Trigonometry`"];

(* Declaration of global variables in Thomsen package *)
rho
cp
cs
gamma
delta
deltaTilde
epsilon
eta
f
C11
C33
C44
C66
C13
A
C
F
L
N
C1
C2
C3
C4
C5
C6
C1111
C2222
C3333
C2233
C3322
C1133
C3311
C1122
C2211
C1212
C2121
C2112
C1221
C1313
C3131
C3113
C1331
C2323
C3232
C3223
C2332

(* Declaration of public function names in Thomsen package *)
EliminateCs::usage = "EliminateCs[expression]\n\n
Replace cs in expression by cp and f.
"

EliminateF::usage = "EliminateF[expression]\n\n
Replace f in expression by cp and cs.
"

EliminateDeltaTilde::usage = "EliminateDeltaTilde[expression]\n\n
Replace deltaTilde in expression by delta and f.
"

EliminateDelta::usage = "EliminateDelta[expression]\n\n
Replace delta in expression by deltaTilde and f.
"

EliminateEta::usage = "EliminateEta[expression]\n\n
Replace eta in expression by delta and epsilon.
"

EliminateEpsilon::usage = "EliminateEpsilon[expression]\n\n
Replace epsilon in expression by eta and delta.
"

EliminateLove::usage = "EliminateLove[expression]\n\n
Replace Love parameters in expression by Cij parameters.
"

EliminateKuprazde::usage = "EliminateKuprazde[expression]\n\n
Replace Kuprazde parameters in expression by Cij parameters.
"

EliminateHooke::usage = "EliminateHooke[expression]\n\n
Replace Hookean Cijkl parameters in expression by Cij parameters.
"

ConvertCij::usage = "ConvertCij[expression]\n\n
Convert expression from Voight Cij notation to Thomsen notation
in terms of cp, f, gamma, epsilon and deltaTilde.
"

ConvertWeak::usage = "ConvertWeak[expression]\n\n
Convert expressions with Voight Cij and/or Thomsen notation to
the weak TI limit in terms of cp, f, gamma, epsilon and delta.
"

ConvertThomsen::usage = "ConvertThomsen[expression]\n\n
Convert expression from Thomsen notation to Voight Cij notation.
"

SineForm::usage="Write trigonometric expression in powers of sine
"

CosineForm::usage="Write trigonometric expression in powers of cosine
"

Begin["`private`"];

(* actual conversion rules left private *)
CsToFRule =
	cs^n_Integer?Positive -> 
		(cp^2(1 - f))^Quotient[n,2] cs^Mod[n,2]

FToCsRule = f -> 1 - cs^2/cp^2

DeltaRule = deltaTilde -> f(Sqrt[1+2delta/f] - 1)

DeltaTildeRule = delta -> deltaTilde (1 + deltaTilde/2f)

EtaRule = epsilon -> delta + eta + 2 delta eta

EpsilonRule = eta -> (epsilon - delta)/(1 + 2 delta)

CijRules = 
{
	C11 -> rho cp^2 (1 + 2 epsilon),
	C33 -> rho cp^2,
	C13 -> -rho cs^2 + rho cp^2 (f + deltaTilde),
	C44 -> rho cs^2,
	C66 -> rho cs^2 (1 + 2 gamma)
}

ThomsenRules =
{
	cp -> Sqrt[C/rho],
	cs -> Sqrt[L/rho],
	gamma -> (N - L)/(2L),
	epsilon -> (A - C)/(2C),
	delta -> (F + 2L - C)(F + C)/(2C(C - L)),
	deltaTilde -> (F + 2L - C)/C,
	eta -> (1/2) ((A-C)(C-L) - (F + 2L - C)(F + C))/
			(C(C-L) + (F + 2L - C)(F + C)),
	f -> (C - L)/C
}

LoveToCij = 
{
	A -> C11,
	C -> C33,
	F -> C13,
	L -> C44,
	N -> C66
}

KuprazdeToCij =
{
	C1 -> C11,
	C2 -> C11 - C66,
	C3 -> C13,
	C4 -> C33,
	C5 -> C44
}

HookeToCij =
{
	C1111 -> C33,
	C2222 -> C11,
	C3333 -> C11,
	C2233 -> C11 - 2 C66,
	C3322 -> C11 - 2 C66,
	C1133 -> C13,
	C3311 -> C13,
	C1122 -> C13,
	C2211 -> C13,
	C1212 -> C44,
	C2121 -> C44,
	C2112 -> C44,
	C1221 -> C44,
	C1313 -> C44,
	C3131 -> C44,
	C3113 -> C44,
	C1331 -> C44,
	C2323 -> C66,
	C3232 -> C66,
	C3223 -> C66,
	C2332 -> C66
}

SineToCosine =
	Sin[m_. x_]^n_Integer?Positive -> 
		(1 - Cos[m x]^2)^Quotient[n,2] Sin[m x]^Mod[n,2]
CosineToSine =
	Cos[m_. x_]^n_Integer?Positive -> 
		(1 - Sin[m x]^2)^Quotient[n,2] Cos[m x]^Mod[n,2]		

(* public function implementations *)
EliminateCs[expression_] := expression /. CsToFRule//Simplify
EliminateF[expression_] := expression /. FToCsRule//Simplify
EliminateDeltaTilde[expression_] := expression /. DeltaRule//Simplify
EliminateDelta[expression_] := expression /. DeltaTildeRule//Simplify
EliminateEta[expression_] := expression /. EpsilonRule//Simplify
EliminateEpsilon[expression_] := expression /. EtaRule//Simplify
EliminateLove[expression_] := expression /. LoveToCij//Simplify
EliminateKuprazde[expression_] := expression /. KuprazdeToCij//Simplify
EliminateHooke[expression_] := expression /. HookeToCij//Simplify

ConvertCij[expression_] := expression/. CijRules/. CsToFRule//Simplify

ConvertWeak[expression_] :=
Module[{x, tmp},
	tmp = ConvertCij[expression];
	tmp = tmp /. EpsilonRule /. DeltaTildeRule /.
	    {
		    epsilon -> epsilon x,
		    deltaTilde -> delta x,
		    gamma -> gamma x
	    };
	Normal@Series[tmp, {x,0,1}] /. x->1 /. CsToFRule//
	Simplify//PowerExpand
]

ConvertThomsen[expression_] := expression/. ThomsenRules//Simplify

SineForm[expression_]   := TrigReduce[expression]/.CosineToSine//Expand
CosineForm[expression_] := TrigReduce[expression]/.SineToCosine//Expand

End[];

Protect[
	EliminateCs,
	EliminateF,
	EliminateDeltaTilde,
	EliminateDelta,
	EliminateEta,
	EliminateEpsilon,
	EliminateLove,
	EliminateKuprazde, 
	EliminateHooke, 
	ConvertCij,
	ConvertWeak,
	ConvertThomsen,
	SineForm,
	CosineForm
]

EndPackage[];
