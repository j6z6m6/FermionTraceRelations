# FermionTraceRelations

Data and notebooks for fermionic trace relations.

Authors: Giorgos Eleftheriou, Ziming Ji, Sameer Murthy

arXiv: 2605.08085

## Overview

This repository contains Mathematica notebooks and generated data for trace relations of fermionic matrices. The computations use a top-down construction of polarized Cayley-Hamilton relations, expressed in an enumerated multi-trace basis and row-reduced to extract independent relations.

## Contents

- `FermionMultiTrace@InfinityN.nb`: notebook for constructing the infinite-`N` single-trace and multi-trace bases.
- `singletracelist.m`, `multitracelist.m`: generated basis data.
- `CheckingFTR.nb`: notebook for checking fermionic trace relations.
- `CHExpandFix.wl`: Wolfram Language helper for expanding and canonicalizing trace expressions.
- `N=2Charge6/`, `N=2Charge7/`, `N=2Charge8/`: `N=2` trace relation generating notebooks and generated relation data.
- `N=3Charge8/`, `N=3Charge9/`, `N=3Charge10/`: `N=3` trace relation generating notebooks and generated relation data.
- `tables.tex`, `tables.pdf`: summary tables for the patterns in trace-relation counting.

Each charge folder contains the relevant Mathematica notebook and generated `.m` data files or archives. `multitracelist.m` and `CHExpandFix.wl` are needed for individual trace relation generating notebooks.

## Requirements

- Wolfram Mathematica / Wolfram Language 12+
- LaTeX, only needed to rebuild `tables.pdf` from `tables.tex`

## Notes

Generated `.m`, `.pdf`, and `.zip` files are included for reproducibility. Recreate them from the notebooks when updating the underlying computations.
