# mmath

[<img src="http://quickcheck-ci.com/p/licenser/mmath.png" alt="Build Status" width="160px">](http://quickcheck-ci.com/p/licenser/mmath)

A library for performaing math on number 'arrays' in binaries.

Most functions include  a _r version to be used after a number array was 'ralized' (as in missing values were filled)

## Floating decimals

This branch is work in progress to add decimal floating point support.

Remaining issues

 * We need to agree on final number encoding, potentialy using type part for exponent
 * Add reading and writing decimal to/from a bitstring
 * Add quick test coverage for decimal lists
 * Ensure proper decimal algebra be c level unit tests
   * I am still experiencing some broken result from operation on 2 decimals with different exponent
 * Check performance and maybe add some optimisations
