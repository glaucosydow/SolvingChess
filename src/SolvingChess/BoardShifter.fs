module BoardShifter

let private Vshift = Array.zeroCreate<uint64> 15
let private Hshift = Array.zeroCreate<uint64> 15

for i = 6 downto 0 do
    Vshift.[i] <- Vshift.[i + 1] ||| (0xFFUL <<< ((6 - i)*8))

for i = 0 to 6 do
    Vshift.[i + 8] <- ~~~Vshift.[i]

for i = 6 downto 0 do
    Hshift.[i] <- Hshift.[i + 1] ||| (72340172838076673UL <<< (6 - i));

for i = 0 to 6 do
    Hshift.[i + 8] <- ~~~Hshift.[i]

let chessShift ranks files pos =
    let newpos = pos &&& ~~~(Vshift.[ranks + 7] ||| Hshift.[files + 7])
    let shift = ranks * 8 + files
    if shift > 0 then newpos <<< shift else newpos >>> -shift

type System.UInt64 with
    member x.chessShift ranks files = chessShift ranks files x

    