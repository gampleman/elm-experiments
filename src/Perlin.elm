module Perlin exposing (noise2, octave2)

import Array exposing (Array)
import Bitwise

octave2 : Int ->  Float -> Float -> Float
octave2 octaves x y =
    let 
      go i total frequency amplitude value =
        if i == 0 then 
            value / total 
        else 
            go (i - 1) (total + amplitude) (frequency * 2) (amplitude * 0.5) (value + amplitude * noise2 (x * frequency) (y * frequency))

    in 
    go octaves 0 1 1 0


noise2 : Float -> Float -> Float
noise2 x y =
    let
        xi =
            floor x

        yi =
            floor y

        x_ =
            Bitwise.and xi 255

        y_ =
            Bitwise.and yi 255

        x2 =
            x - toFloat xi

        y2 =
            y - toFloat yi

        u =
            fade x2

        v =
            fade y2

        a =
            (p x_) + y_

        b =
            p (x_ + 1) + y_
    in
    lerp v
        (lerp u (grad2 (p a) x2 y2) (grad2 (p b) (x2 - 1) y2))
        (lerp u (grad2 (p (a + 1)) x2 (y2 - 1)) (grad2 (p (b + 1)) (x2 - 1) (y2 - 1)))


lerp : Float -> Float -> Float -> Float
lerp t a b =
    a + t * (b - a)


fade : Float -> Float
fade t =
    t * t * t * (t * (t * 6 - 15) + 10)


grad2 i x y =
    let
        v =
            if Bitwise.and i 1 == 0 then
                x

            else
                y
    in
    if Bitwise.and i 2 == 0 then
        v

    else
        -v


parr : Array Int
parr =
    Array.fromList [ 151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180 ]


p : Int -> Int
p i =
    (if i < 256 then
        Array.get i parr

     else
        Array.get (i - 256) parr
    )
        |> Maybe.withDefault 0
