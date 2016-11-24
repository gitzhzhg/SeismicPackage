(* 
Ortho.m:
This Package has two main purposes:
1. Provide conversions between Voigt and Thomsen notation for
homogeneous orthorhombic media.
2. Provide tools for expanding expressions in the anisotropy
parameters (to arbitrary order).

It also provides some conveniences such as recognizing the parameter
eta used in time domain seismic processing.  The use
of the package is illustrated in the accompanying notebook, Ortho.ma.

Note: The plus signs are silently used in computing C13, C23, and, C12
from the deltas.
*)
  
(*
Copyright: Jack K. Cohen, Colorado School of Mines, 1996.  All rights reserved.
*)


BeginPackage["Ortho`", "ATools`"];

(* Declaration of global variables in the package *)
VP0 VS0 VS1
gamma1 delta1 epsilon1 eta1
gamma2 delta2 epsilon2 eta2 delta3
gammaT gammaS
f1 f2 delta1Tilde delta2Tilde delta3Tilde
C11 C12 C13 C14 C15 C16
C21 C22 C23 C24 C25 C26
C31 C32 C33 C34 C35 C36
C41 C42 C43 C44 C45 C46
C51 C52 C53 C54 C55 C56
C61 C62 C63 C64 C65 C66

(* Declaration of public function names in the package *)
VoigtOrtho::usage = "VoigtOrtho[expression]\n\n
Reduce expressions in Cij to the canonical Ortho set,
C11, C22, C33, C44, C55, C66, C13, C23, C12.
"

UseF1::usage = "UseF1[expression]\n\n
Replace VS1^2 in expression by VP0^2 and f1 and replace VS0 by
these and gammaT.  An odd power of VS1 will result in having a
single VS1 factor in the result.
"

UseVS1::usage = "UseVS1[expression]\n\n
Replace f1 in expression by VP0 and VS1 and
replace VS0 by these and gammaT.
"

UseVS0::usage = "UseVS0[expression]\n\n
Replace VS1 in expression by VS0 and gammaS and
replace f1 by these and VP0.
"

UseF2::usage = "UseF2[expression]\n\n
Replace f1 by f2 and gammaS and eliminate gamma1
and gammaT in favor of gammaS.
"

UseDeltaTildes::usage = "UseDeltaTildes[expression]\n\n
Replace delta1, delta2, delta3 in expression by deltaTildes,
gammas, and f1.
"

UseDeltas::usage = "UseDeltas[expression]\n\n
Replace delta1Tilde, delta2Tilde, delta3Tilde in expression by
deltas, gammas, and f1.
"

UseEtas::usage = "UseEtas[expression]\n\n
Replace epsilons in expression by etas and deltas.
"

UseEpsilons::usage = "UseEpsilons[expression]\n\n
Replace etas in expression by deltas and epsilons.
"

UseGammaS::usage = "UseGammaS[expression]\n\n
Replace gammaT and gamma1 in expression by gammaS and gamma2.
"

UseGammaT::usage = "UseGammaT[expression]\n\n
Replace gammaS and gamma1 in expression by gammaT and gamma2.
"

UseGamma1::usage = "UseGamma1[expression]\n\n
Replace gammaS and gammaT in expression by gamma1 and gamma2.
"

ConvertCanonical::usage = "ConvertCanonical[expression]\n\n
Convert expression from Voigt Cij notation to Thomsen-Tsvankin notation
in terms of the canonical variables: VP0, f1, gamma2, gammaT,
epsilon1, epsilon2, delta1Tilde, delta2Tilde, and delta3Tilde.
"

ConvertStandard::usage = "ConvertStandard[expression]\n\n
Convert expression from Voigt Cij notation to Thomsen-Tsvankin notation
in terms of the standard variables: VP0, VS0, gamma1, gamma2,
epsilon1, epsilon2, delta1, delta2, and delta3.
"

WeakOrtho::usage = "WeakOrtho[expression, (order)]\n\n
Convert expressions in Thomsen-Tsvankin notation (either standard or
canonical) to the weak Ortho limit in terms of VP0, f1, gamma1, gamma2,
epsilon1, epsilon2, delta1, delta2, and delta3.  The order parameter
gives the order to keep in the gammas, epsilons, and deltas.  It
defaults to 1 (the literal weak Ortho limit).
"

ConvertToVoigt::usage = "ConvertToVoigt[expression]\n\n
Convert expression from Thomsen-Tsvankin notation to Voigt Cij notation.
"

VoigtVTI::usage = "VoigtVTI[expression]\n\n
Reduce expressions in Cij to the canonical VTI set,
C11, C33, C55, C66, C13.
"

VTILimit::usage = "VTILimit[expression]\n\n
Convert expression in Thomsen-Tsvankin parameters to
Thomsen parameters assuming VTI symmetry.
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

VS1ToF1Rule =
	VS1^n_Integer?Positive -> 
		(VP0^2(1 - f1))^Quotient[n,2] VS1^Mod[n,2]

F1ToVS1Rule = f1 -> 1 - VS1^2/VP0^2

F1ToF2Rule = f1->1-(1-f2)(1 + 2 gammaS)
F2ToF1Rule = f2->1-(1-f1)(1 + 2 gammaT)

VS1ToVS0Rule = VS1 -> VS0 Sqrt[1 + 2 gammaS]
VS0ToVS1Rule = VS0 -> VS1 Sqrt[1 + 2 gammaT]

(* F2 and F3 are private variables used to simplify delta2,3 *)
F2 = 1 - (1-f1)(1+2gammaT)  (* Perhaps (1+2gamma2)/(1+2gamma1) and public *)
F3 = 1 - (1-f1)(1+2gamma2)/(1+2epsilon2)
DeltaRules = {
	delta1Tilde -> f1(Sqrt[1+2delta1/f1] - 1),
	delta2Tilde -> F2(Sqrt[1+2delta2/F2] - 1),
	delta3Tilde -> F3(Sqrt[1+2delta3/F3] - 1)
}
DeltaTildeRules = {
	delta1 -> delta1Tilde (1 + delta1Tilde/(2f1)),
	delta2 -> delta2Tilde (1 + delta2Tilde/(2F2)),
	delta3 -> delta3Tilde (1 + delta3Tilde/(2F3))
}

EtaRules = {
	epsilon1 -> delta1 + eta1 + 2 delta1 eta1,
	epsilon2 -> delta2 + eta2 + 2 delta2 eta2
}

EpsilonRules = {
	eta1 -> (epsilon1 - delta1)/(1 + 2 delta1),
	eta2 -> (epsilon2 - delta2)/(1 + 2 delta2)
}


GammaSRules = {
	gammaT -> -gammaS/(1 + 2 gammaS),
	gamma1 -> gamma2 + gammaS + 2 gamma2 gammaS
}

GammaTRules = {
	gammaS -> -gammaT/(1 + 2 gammaT),
	gamma1 -> (gamma2 - gammaT)/(1 + 2 gammaT)
}

Gamma1Rules = {
	gammaT -> (gamma2 - gamma1)/(1 + 2 gamma1),
	gammaS -> (gamma1 - gamma2)/(1 + 2 gamma2)
}

CijRules = 
{
	C11 -> rho VP0^2 (1 + 2 epsilon2),
	C22 -> rho VP0^2 (1 + 2 epsilon1),
	C33 -> rho VP0^2,
	C44 -> rho VP0^2 (1 - f1),
	C55 -> rho VP0^2 (1 - f1) (1 + 2 gammaT),
	C66 -> rho VP0^2 (1 - f1) (1 + 2 gamma2),
	C13 -> rho VP0^2 (delta2Tilde + 2 F2 - 1),
	C23 -> rho VP0^2 (delta1Tilde + 2 f1 - 1),
	C12 -> rho VP0^2 (1 + 2 epsilon2) (delta3Tilde + 2 F3 - 1)
}

VTIRules =
{
	gamma1 -> gamma2,
	delta1 -> delta2,
	epsilon1 -> epsilon2,
	delta1Tilde -> delta2Tilde,
	f1 -> f2,
	VS1 -> VS0,
	delta3 -> 0,
	gammaT -> 0,
	gammaS -> 0,
	delta3Tilde -> 0
}

TsvankinRules =
{
	VP0 -> Sqrt[C33/rho],
	VS1 -> Sqrt[C44/rho],
	VS0 -> Sqrt[C55/rho],
	gamma2 -> (C66 - C44)/(2C44),
	gamma1 -> (C66 - C55)/(2C55),
	gammaT -> (C55 - C44)/(2C44),
	gammaS -> (C44 - C55)/(2C55),
	epsilon2 -> (C11 - C33)/(2C33),
	epsilon1 -> (C22 - C33)/(2C33),
	delta2 -> (C13 + 2C55 - C33)(C13 + C33)/(2C33(C33 - C55)),
	delta1 -> (C23 + 2C44 - C33)(C23 + C33)/(2C33(C33 - C44)),
	delta3 -> (C12 + 2C66 - C11)(C12 + C11)/(2C11(C11 - C66)),
	delta2Tilde -> (C13 + 2C55 - C33)/C33,
	delta1Tilde -> (C23 + 2C44 - C33)/C33,
	delta3Tilde -> (C12 + 2C66 - C11)/C11,
	eta2 -> (1/2) ((C11-C33)(C33-C55) - (C13 + 2C55 - C33)(C13 + C33))/
			(C33(C33-C55) + (C13 + 2C55 - C33)(C13 +C33)),
	eta1 -> (1/2) ((C22-C33)(C33-C44) - (C23 + 2C44 - C33)(C23 + C33))/
			(C33(C33-C44) + (C23 + 2C44 - C33)(C23 +C33)),
	f1 -> (C33 - C44)/C33,
	f2 -> (C33 - C55)/C33
}

(* public function implementations *)
VoigtOrtho[expression_] := expression/. SymmetryRules//Simplify
VoigtVTI[expression_] := expression/. SymmetryRules/. VoigtVTIRules//Simplify
UseF1[expression_]  := expression/.
	F2ToF1Rule/.VS0ToVS1Rule/. VS1ToF1Rule/. GammaTRules//Simplify
UseVS1[expression_] := expression/.
	F2ToF1Rule/.F1ToVS1Rule/. VS0ToVS1Rule/. GammaTRules//Simplify
UseVS0[expression_] := expression/.
	F2ToF1Rule/.F1ToVS1Rule/. VS1ToVS0Rule/. GammaSRules//Simplify
UseF2[expression_] := expression/.
	VS0ToVS1Rule/. VS1ToF1Rule/. F1ToF2Rule/. GammaSRules//Simplify
UseGammaS[expression_] := expression/. GammaSRules//Simplify
UseGammaT[expression_] := expression/. GammaTRules//Simplify
UseGamma1[expression_] := expression/. Gamma1Rules//Simplify
UseDeltaTildes[expression_] :=
	expression /. DeltaTildeRules//Simplify//PowerExpand//Simplify
UseDeltas[expression_] := expression/. DeltaRules//Simplify
UseEpsilons[expression_] := expression/. EpsilonRules//Simplify
UseEtas[expression_] := expression/. EtaRules//Simplify

ConvertCanonical[expression_] :=
	expression/. SymmetryRules/. CijRules//Simplify

ConvertStandard[expression_] :=
	ConvertCanonical[expression]/. DeltaRules/.F1ToVS1Rule/.
		VS1ToVS0Rule/. Gamma1Rules//Simplify

ConvertToVoigt[expression_] := expression/. TsvankinRules//Simplify

VTILimit[expression_] := expression/. VTIRules//Simplify


(* This Uniform Series is private and probably should be eliminated *)
UniformSeries[expression_, L_List, n_:1] :=
Module[{x, tmp},
	tmp = expression/. Thread[L -> L x];
    Normal@Series[tmp, {x,0,n}] /. x->1//
    Simplify//PowerExpand//Simplify
]

WeakOrtho[expression_, n_:1] :=
UniformSeries[expression/.DeltaRules/.Gamma1Rules,
	{gamma2, delta2, epsilon2, delta1, epsilon1, gamma1,
	 delta3, eta1, eta2}, n]//Simplify

End[];
(*
Protect[
	rho, VP0, VS0, VS1,
	gamma2, delta1, epsilon1, eta1,
	gammaT, delta2, epsilon2, eta2,
	gammaS, delta3, gamma1, f1, f2,
	C11, C12, C13, C14, C15, C16,
	C21, C22, C23, C24, C25, C26,
	C31, C32, C33, C34, C35, C36,
	C41, C42, C43, C44, C45, C46,
	C51, C52, C53, C54, C55, C56,
	C61, C62, C63, C64, C65, C66,
	A, C, F, L, N,
	C1, C2, C3, C4, C5, C6,
	VoigtOrtho,
	UseF1,
	UseVS1,
	UseVS0,
	UseF,
	UseDeltaTildes,
	UseDeltas,
	UseEtas,
	UseEpsilons,
	UseGammaS,
	UseGammaT,
	UseGamma1,
	ConvertCanonical,
	ConvertStandard,
	ConvertToVoigt,
	WeakOrtho,
	VoigtVTI,
	VTILimit
]
*)
EndPackage[];
