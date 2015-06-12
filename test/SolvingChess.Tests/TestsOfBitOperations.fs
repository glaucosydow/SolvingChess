module TestOfBitsOperations

open NUnit.Framework
open BitOperations
open BoardUnits

[<Test>]
let ``lsb returns the index of the least significant bit that is set``() =
    Assert.AreEqual(lsb(1UL <<< 31), 31)
    Assert.AreEqual((1UL <<< 31).lsb(), 31)

[<Test>]
let ``msb returns the index of the most significant bit that is set``() =
    Assert.AreEqual(msb (sq 'h' 8), 63)





    