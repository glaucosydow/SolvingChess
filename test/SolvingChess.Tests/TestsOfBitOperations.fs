module TestOfBitsOperations

open Xunit
open BitOperations
open BoardUnits

[<Fact>]
let ``lsb returns the index of the least significant bit that is set``() =
    Assert.Equal(lsb(1UL <<< 31), 31)
    Assert.Equal((1UL <<< 31).lsb(), 31)

[<Fact>]
let ``msb returns the index of the most significant bit that is set``() =
    Assert.Equal(msb (sq 'h' 8), 63)