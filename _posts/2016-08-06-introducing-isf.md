---
layout: post
title: Introducing ISF project 
---

The ISF project is a database of Integer Sequence Formulas and some applications derived from it. The Integer Sequence Formulas are parsed from the [OEIS](http://getpoole.com).

### Current ISF parts

The ISF is currently composed of:

* A parser for math formulas found in the OEIS.
* A program that find relations between OEIS sequences.
* A MathWebSearch instance for OEIS documents.

[Head to this thesis](https://github.com/poole/lanyon#readme) to learn more.

### Some stats 

The current parser can parse about 90% of the formula lines (%F) from the OEIS documents. Out of the parsed formulas, 89% are parsed semantic correctly.
The program that generates relations between OEIS sequences uses three methods. In total, the number of relations is about 20 million (look at the thesis for the results).

### Download

ISF is developed on and hosted with GitHub. Head to the <a href="https://github.com/eluzhnica/OEIS">GitHub repository</a> for downloads, bug reports, and requests.

Thanks!
