(* 
VTI.m:
This Package has two main purposes:
1. Provide conversions between Voigt/Love and Thomsen notation for
homogeneous vertically tranverse isotropic media.
2. Provide tools for expanding expressions in the anisotropy
parameters (to arbitrary order).

It also provides some conveniences such as converting expressions
in Love notation and recognizing the parameter eta used in time
domain seismic processing for VTI media.  The use of the package
is illustrated in the accompanying notebook, VTI.ma.

Note: The plus sign is silently used in computing C13 from Thomsen's delta.

This is a revision of the Thomsen.m package described in CWP-185,
Project Review, 1995.
*)
  
(*
Copyright: Jack K. Cohen, Colorado School of Mines, 1996.  All rights reserved.
*)


BeginPackage["VTI`", "ATools`"];

(* Declaration of global variables in the package *)
VP0 VS0
gamma delta epsilon
eta f deltaTilde
C11 C12 C13 C14 C15 C16
C21 C22 C23 C24 C25 C26
C31 C32 C33 C34 C35 C36
C41 C42 C43 C44 C45 C46
C51 C52 C53 C54 C55 C56
C61 C62 C63 C64 C65 C66
A C F L N

(* Declaration of public function names in the package *)
VoigtVTI::usage = "VoigtVTI[expression]\n\n
Reduce expressions in Cij to the canonical VTI set,
C11, C13, C33, C55, C66.  Note: C44 = C55.
"

UseF::usage = "UseF[expression]\n\n
Replace VS0^2 in expression by VP0^2 and f.  An
odd power of VS0 will result in having a
single VS0 factor in the result.
"

UseVS0::usage = "UseVS0[expression]\n\n
Replace f in expression by VP0 and VS0.
"

UseDeltaTilde::usage = "UseDeltaTilde[expression]\n\n
Replace delta in expression by deltaTilde and f.
"

UseDelta::usage = "UseDelta[expression]\n\n
Replace deltaTilde in expression by delta and f.
"

UseEta::usage = "UseEta[expression]\n\n
Replace epsilon in expression by eta and delta.
"

UseEpsilon::usage = "UseEpsilon[expression]\n\n
Replace eta in expression by delta and epsilon.
"

LoveToVoigt::usage = "LoveToVoigt[expression]\n\n
Replace Love parameters in expression by Cij parameters.
"

VoigtToLove::usage = "VoigtToLove[expression]\n\n
Replace Cij parameters in expression by Love parameters.
"

ConvertCanonical::usage = "ConvertCanonical[expression]\n\n
Convert expression from Voigt Cij notation to Thomsen notation
in terms of the canonical variables, VP0, f, gamma, epsilon, and
deltaTilde.
"

ConvertStandard::usage = "ConvertStandard[expression]\n\n
Convert expression from Voigt Cij notation to Thomsen notation
in terms of the standard variables, VP0, VS0, gamma, epsilon, and
delta.
"
WeakVTI::usage = "WeakVTI[expression, (order)]\n\n
Convert expressions in Thomsen notation (either standard or
canonical) to the weak TI limit in terms of VP0, f, gamma,
epsilon, and delta.  The order parameter gives the order to
keep in gamma, epsilon, delta and defaults to 1 (the literal
weak VTI limit).
"

ConvertToVoigt::usage = "ConvertToVoigt[expression]\n\n
Convert expression from Thomsen notation to Voigt Cij notation.
"

Begin["`private`"];

(* actual conversion rules left private *)
SymmetryRules =
{
	C21 -> C12,
	C31 -> C13, C32 -> C23,
	C41 -> 0, C42 -> 0, C43 -> 0,
	C51 -> 0, C52 -> 0, C53 -> 0, C54 -> 0,
	C61 -> 0, C62 -> 0, C63 -> 0, C64 -> 0, C65 -> 0,
	C14 -> 0, C15 -> 0, C16 -> 0,
	C24 -> 0, C25 -> 0, C26 -> 0,
	C34 -> 0, C35 -> 0, C36 -> 0,
	C45 -> 0, C46 -> 0,
	C56 -> 0
}

VoigtVTIRules =
{
	C22 -> C11,
	C44 -> C55,
	C23 -> C13,
	C12 -> C11 - 2C66
}

VS0ToFRule =
	VS0^n_Integer?Positive -> 
		(VP0^2(1 - f))^Quotient[n,2] VS0^Mod[n,2]

FToVS0Rule = f -> 1 - VS0^2/VP0^2

DeltaRule = deltaTilde -> f(Sqrt[1+2delta/f] - 1)

DeltaTildeRule = delta -> deltaTilde (1 + deltaTilde/(2f))

EtaRule = epsilon -> delta + eta + 2 delta eta

EpsilonRule = eta -> (epsilon - delta)/(1 + 2 delta)

CijRules = 
{
	C11 -> rho VP0^2 (1 + 2 epsilon),
	C33 -> rho VP0^2,
	C13 -> rho VP0^2 (2f + deltaTilde - 1),
	C55 -> rho VP0^2 (1 - f),
	C66 -> rho VP0^2 (1 - f)(1 + 2 gamma)
}

ThomsenRules =
{
	VP0 -> Sqrt[C33/rho],
	VS0 -> Sqrt[C55/rho],
	gamma -> (C66 - C55)/(2C55),
	epsilon -> (C11 - C33)/(2C33),
	delta -> (C13 + 2C55 - C33)(C13 + C33)/(2C33(C33 - C55)),
	deltaTilde -> (C13 + 2C55 - C33)/C33,
	eta -> (1/2) ((C11-C33)(C33-C55) - (C13 + 2C55 - C33)(C13 + C33))/
			(C33(C33-C55) + (C13 + 2C55 - C33)(C13 + C33)),
	f -> (C33 - C55)/C33
}

VoigtToLoveRules = 
{
	C11 -> A,
	C33 -> C,
	C13 -> F,
	C55 -> L,
	C66 -> N
}

LoveToVoigtRules = 
{
	A -> C11,
	C -> C33,
	F -> C13,
	L -> C55,
	N -> C66
}


(* public function implementations *)
VoigtVTI[expression_] := expression/. SymmetryRules/. VoigtVTIRules//Simplify
UseF[expression_] := expression/. VS0ToFRule//Simplify
UseVS0[expression_] := expression/. FToVS0Rule//Simplify
UseDeltaTilde[expression_] :=
	expression /. DeltaTildeRule//Simplify//PowerExpand//Simplify
UseDelta[expression_] := expression/. DeltaRule//Simplify
UseEpsilon[expression_] := expression/. EpsilonRule//Simplify
UseEta[expression_] := expression/. EtaRule//Simplify
LoveToVoigt[expression_] := expression/. LoveToVoigtRules//Simplify
VoigtToLove[expression_] := expression/.
	SymmetryRules/. VoigtVTIRules/. VoigtToLoveRules//Simplify

ConvertCanonical[expression_] := expression/.
	SymmetryRules/. VoigtVTIRules/. CijRules//Simplify

ConvertStandard[expression_] :=
	ConvertCanonical[expression]/. DeltaRule/. FToVS0Rule //Simplify

ConvertToVoigt[expression_] := expression/. ThomsenRules//Simplify

(* This Uniform Series is private and probably should be eliminated *)
UniformSeries[expression_, L_List, n_:1] :=
Module[{x, tmp},
	tmp = expression/. Thread[L -> L x];
    Normal@Series[tmp, {x,0,n}] /. x->1//
    Simplify//PowerExpand//Simplify
]

WeakVTI[expression_, n_:1] :=
UniformSeries[expression/.DeltaRule,{gamma, delta, epsilon, eta}, n]//Simplify

End[];
(*
Protect[
	rho, VP0, VS0, gamma, delta, deltaTilde, epsilon, eta, f,
	C11, C12, C13, C14, C15, C16,
	C21, C22, C23, C24, C25, C26,
	C31, C32, C33, C34, C35, C36,
	C41, C42, C43, C44, C45, C46,
	C51, C52, C53, C54, C55, C56,
	C61, C62, C63, C64, C65, C66,
	A, C, F, L, N,
	VoigtVTI,
	UseVS0,
	UseF,
	UseDeltaTilde,
	UseDelta,
	UseEta,
	UseEpsilon,
	LoveToVoigt,
	VoigtToLove,
	ConvertCanonical,
	ConvertStandard,
	ConvertToVoigt,
	WeakVTI
]
*)

EndPackage[];
