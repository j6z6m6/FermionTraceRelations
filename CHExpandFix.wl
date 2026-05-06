(* Compatibility expansion for CH trace relations under newer Wolfram versions.
   It canonicalizes expressions as coefficient + ordered NonCommutativeMultiply
   factors, expands Tr linearly, and unwraps repeated noncommutative products
   represented as Power or GeneralizedPower. *)

ClearAll[
  CHExpand, CHTerms, CHMul, CHEmit, CHDotPowers, CHDotRules,
  CHFieldNumber, CHTraceWord, CHTraceNumberList
];

CHDotRules = {
  HoldPattern[GeneralizedPower[Dot, x_, n_Integer?Positive]] :>
    Apply[Dot, ConstantArray[x, n]],
  HoldPattern[GeneralizedPower[NonCommutativeMultiply, x_, n_Integer?Positive]] :>
    Apply[NonCommutativeMultiply, ConstantArray[x, n]],
  HoldPattern[Power[x_NonCommutativeMultiply, n_Integer?Positive]] :>
    Apply[NonCommutativeMultiply, ConstantArray[x, n]],
  HoldPattern[Verbatim[Dot][a___, x_Plus, b___]] :>
    Total[Dot[a, #, b] & /@ (List @@ x)],
  HoldPattern[Verbatim[Dot][a___, Times[c_?NumericQ, x_], b___]] :>
    c Dot[a, x, b]
};

CHDotPowers[expr_] := FixedPoint[Expand[# /. CHDotRules] &, expr];

CHFieldNumber[1] := Sequence[];
CHFieldNumber[\[Psi]m] := 1;
CHFieldNumber[d1\[Psi]m] := 2;
CHFieldNumber[d2\[Psi]m] := 3;
CHFieldNumber[d3\[Psi]m] := 4;
CHFieldNumber[d4\[Psi]m] := 5;
CHFieldNumber[d5\[Psi]m] := 6;
CHFieldNumber[d6\[Psi]m] := 7;
CHFieldNumber[d7\[Psi]m] := 8;
CHFieldNumber[d8\[Psi]m] := 9;
CHFieldNumber[d9\[Psi]m] := 10;
CHFieldNumber[d10\[Psi]m] := 11;

CHTraceWord[x_Dot] := Flatten[CHTraceWord /@ (List @@ x)];
CHTraceWord[GeneralizedPower[Dot, x_, n_Integer?Positive]] :=
  Flatten[ConstantArray[CHTraceWord[x], n]];
CHTraceWord[x_] := {CHFieldNumber[x]};

CHTraceNumberList[x_NonCommutativeMultiply] :=
  Join @@ (CHTraceNumberList /@ (List @@ x));
CHTraceNumberList[GeneralizedPower[NonCommutativeMultiply, x_, n_Integer?Positive]] :=
  Flatten[ConstantArray[CHTraceNumberList[x], n], 1];
CHTraceNumberList[Power[x_NonCommutativeMultiply, n_Integer?Positive]] :=
  Flatten[ConstantArray[CHTraceNumberList[x], n], 1];
CHTraceNumberList[Power[x_Tr, n_Integer?Positive]] :=
  Flatten[ConstantArray[CHTraceNumberList[x], n], 1];
CHTraceNumberList[x_Times] :=
  Join @@ (CHTraceNumberList /@ Select[List @@ x, Not[NumericQ[#]] &]);
CHTraceNumberList[Tr[x_]] := {CHTraceWord[CHDotPowers[x]]};

CHMul[a_, b_] := Flatten[
  Outer[{#1[[1]] #2[[1]], Join[#1[[2]], #2[[2]]]} &, a, b, 1],
  1
];

CHTerms[x_?NumericQ] := {{x, {}}};

CHTerms[x_Plus] := Join @@ (CHTerms /@ (List @@ x));

CHTerms[x_Times] :=
  Fold[CHMul, {{1, {}}}, CHTerms /@ (List @@ x)];

CHTerms[x_NonCommutativeMultiply] :=
  Fold[CHMul, {{1, {}}}, CHTerms /@ (List @@ x)];

CHTerms[Power[x_Tr, n_Integer?Positive]] :=
  Fold[CHMul, {{1, {}}}, ConstantArray[CHTerms[x], n]];

CHTerms[Power[x_NonCommutativeMultiply, n_Integer?Positive]] :=
  Fold[CHMul, {{1, {}}}, ConstantArray[CHTerms[x], n]];

CHTerms[GeneralizedPower[NonCommutativeMultiply, x_, n_Integer?Positive]] :=
  Fold[CHMul, {{1, {}}}, ConstantArray[CHTerms[x], n]];

CHTerms[Tr[x_]] := Module[{y = Expand[CHDotPowers[x]], args, nums, rest},
  Which[
    Head[y] === Plus,
      Join @@ (CHTerms /@ (Tr /@ (List @@ y))),

    Head[y] === Times && AnyTrue[List @@ y, NumericQ],
      args = List @@ y;
      nums = Select[args, NumericQ];
      rest = Select[args, Not[NumericQ[#]] &];
      ({(Times @@ nums) #[[1]], #[[2]]} & /@
        CHTerms[Tr[Times @@ rest]]),

    True,
      {{1, {Tr[y]}}}
  ]
];

CHTerms[x_] := {{1, {x}}};

CHEmit[{c_, {}}] := c;
CHEmit[{c_, f_}] /; Length[f] == 1 := c First[f];
CHEmit[{1, f_}] := NonCommutativeMultiply @@ f;
CHEmit[{-1, f_}] := -NonCommutativeMultiply @@ f;
CHEmit[{c_, f_}] /; NumericQ[c] && TrueQ[c < 0] :=
  -NonCommutativeMultiply[-c, Sequence @@ f];
CHEmit[{c_, f_}] := NonCommutativeMultiply[c, Sequence @@ f];

CHExpand[expr_] := Total[CHEmit /@ CHTerms[Expand[CHDotPowers[expr]]]];
