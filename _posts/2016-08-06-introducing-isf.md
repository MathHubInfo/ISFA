---
layout: post
title: Introducing ISFA project 
---

The ISFA project is a database of Integer Sequence Formulas and Applications derived from it. The integer sequence formulas are parsed from the [OEIS](http://oeis.org).

### Current ISFA parts

The ISFA is currently composed of:

* A parser for math formulas found in the OEIS.
* A program that find relations between OEIS sequences (relations available <a href="https://github.com/MathHubInfo/OEIS/tree/master/results/relations">here</a>).
* A MathWebSearch instance for OEIS documents (http://oeissearch.mathweb.org)

[Head to this thesis](https://github.com/MathHubInfo/OEIS/blob/master/docs/Enxhell_Luzhnica_BSC.pdf) to learn more.

### Some stats and results

The current parser can parse about 90% of the formula lines (%F) from the OEIS documents. Out of the parsed formulas, 89% are parsed semantic correctly.
The program that generates relations between OEIS sequences uses three methods. In total, the number of generated relations is about 20 million (look at the thesis for the exact results).
Since we were able to parse the formulas, we know the current relations that are present in the OEIS. We made a graph where each line represents a relation between the two sequences it connects. Here is the current graph of relations between the sequences that we selected for generation
(again, read the thesis for information about the sequences we have selected):

<figure>
  <img src="{{site.baseurl}}/images/current-graph.png" alt="Current relations" align="middle">
  <figcaption>Fig 1. The current OEIS graph of relations.</figcaption>
</figure>

This is what happens to the graph after we run the second method of direct relations:

<figure>
  <img src="{{site.baseurl}}/images/after2.png" alt="Current relations" align="middle">
  <figcaption>Fig 2. Graph of direct relations.</figcaption>
</figure>

<figure>
  <img src="{{site.baseurl}}/images/after.png" alt="Current relations" align="middle">
  <figcaption>Fig 3. Graph of relations of the third method.</figcaption>
</figure>

These graphs are available in interactive format but it takes a lot of time to load them since there are millions of relations. The code for interactive ones can be found <a href="https://github.com/MathHubInfo/OEIS/graphs">here</a>.


### Download

ISF is developed on and hosted with GitHub. Head to the <a href="https://github.com/MathHubInfo/OEIS">GitHub repository</a> for downloads, bug reports, and requests.

Thanks!
