module BitOperations

let private Index64 = [|
        63;  0; 58;  1; 59; 47; 53;  2;
        60; 39; 48; 27; 54; 33; 42;  3;
        61; 51; 37; 40; 49; 18; 28; 20;
        55; 30; 34; 11; 43; 14; 22;  4;
        62; 57; 46; 52; 38; 26; 32; 41;
        50; 36; 17; 19; 29; 10; 13; 21;
        56; 45; 25; 31; 35; 16;  9; 12;
        44; 24; 15;  8; 23;  7;  6;  5
    |]


let lsb value = 
    Index64.[int (((value &&& (~~~value + 1UL)) * 571347909858961602UL) >>> 58)]

let rec msb (value: uint64) =
    match value with
    | v when v > 4294967295UL -> 32 + (msb (value >>> 32))
    | v when v > 65535UL -> 16 + (msb (value >>> 16))
    | v when v > 255UL -> 8  + (msb (value >>> 8))
    | v when v > 15UL -> 4  + (msb (value >>> 4))
    | v when v > 3UL -> 2  + (msb (value >>> 2))
    | v when v > 1UL -> 1
    | _ -> 0
    

let inline except bitsToRemove bitmap =
    bitmap &&& (~~~bitsToRemove)

let inline isSet (bits:uint64) bitmap =
    (bits &&& bitmap) = bits

type System.UInt64 with 
    member x.lsb() = lsb x
    member x.msb() = msb x
  