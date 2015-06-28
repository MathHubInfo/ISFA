# OEIS
A parser for OEIS formulas.

TODOS:
    1. Allow phi() func();
    5. Create a dictionary of external words and add them to the "exceptions" rule of grammar when parsing the grammmar
    n = p_a * p_b * ... * p_h * p_i * p_j * p_k
    B_4(-1) = 2*log(3) - (2/3)*Catalan + 2*Ti_2(3-2*sqrt(2)) - sqrt(8) * arctan( 1/sqrt(8) )
    postProcess fix
     a(n+1) is the determinant of the n X n symmetric Pascal matrix M_(i, j) = binomial(i+j+1, i). - _Benoit Cloitre_, Aug 19 2003 - post process this
     
    7. Add mod support, note different forms, 1 mod 3, x = 4 (mod 3), different meanings.
    8. sin an! should be parsed as sin(an!) (change the _argument_ rule). (  ) if not bla bla


    - Sum{k in Z}
    - fix the fucking phi(), phi(,)
    -         Euler transform of period 8 sequence [ 1, -1, 1, 0, 1, -1, 1, -2, ...]. - _Michael Somos_, Apr 28 2003 */
