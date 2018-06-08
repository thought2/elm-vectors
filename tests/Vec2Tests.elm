module Vec2Tests exposing (..)

--import Types exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Test exposing (..)
import Types exposing (..)
import Vec2 exposing (..)


-- TYPE CONSTRUCTORS


vec2 : Test
vec2 =
    describe
        "vec2"
        [ test "behaves like the real constructor" <|
            \_ ->
                Vec2.vec2 True False
                    |> Expect.equal (Vec2 True False)
        ]



-- FUNCTOR


map : Test
map =
    describe
        "map"
        [ test "maps over all fields" <|
            \_ ->
                Vec2.map not (Vec2 True False)
                    |> Expect.equal (Vec2 False True)
        ]


functorLaws : Test
functorLaws =
    describe
        "Functor laws"
        [ fuzz (Fuzz.map2 Vec2 int int) "Identity" <|
            \fuzzyIntVec ->
                Vec2.map identity fuzzyIntVec
                    |> Expect.equal fuzzyIntVec
        , fuzz (Fuzz.map3 (,,) (Fuzz.map2 Vec2 int int) int int) "Composition" <|
            \( fuzzyIntVec, n1, n2 ) ->
                let
                    f =
                        (+) n1

                    g =
                        (*) n2
                in
                Vec2.map (f << g) fuzzyIntVec
                    |> Expect.equal ((Vec2.map f << Vec2.map g) fuzzyIntVec)
        ]



-- APPLICATIVE


pure : Test
pure =
    describe
        "pure"
        [ test "initializes all fields" <|
            \_ ->
                Vec2.pure True
                    |> Expect.equal (Vec2 True True)
        ]


apply : Test
apply =
    describe "apply"
        [ test "applies functions in the context" <|
            \_ ->
                Vec2.apply (Vec2 not not) (Vec2 True False)
                    |> Expect.equal (Vec2 False True)
        ]


applicativeLaws : Test
applicativeLaws =
    describe
        "Applicative laws"
        [ fuzz (Fuzz.map2 Vec2 int int) "Identity" <|
            \fuzzyIntVec ->
                Vec2.apply (Vec2.pure identity) fuzzyIntVec
                    |> Expect.equal fuzzyIntVec
        , fuzz (Fuzz.map2 (,) int int) "Homomorphism" <|
            \( n1, n2 ) ->
                let
                    f =
                        (+) n1
                in
                Vec2.apply (Vec2.pure f) (Vec2.pure n2)
                    |> Expect.equal (Vec2.pure (f n2))
        , fuzz (Fuzz.map2 (,) int int) "Interchange" <|
            \( n1, n2 ) ->
                let
                    f =
                        Vec2.pure ((+) n1)
                in
                Vec2.apply f (Vec2.pure n2)
                    |> Expect.equal (Vec2.apply (Vec2.pure ((|>) n2)) f)
        , fuzz (Fuzz.map3 (,,) int int int) "Composition" <|
            \( n1, n2, n3 ) ->
                let
                    f1 =
                        Vec2.pure ((+) n1)

                    f2 =
                        Vec2.pure ((-) n2)

                    x =
                        Vec2.pure n3
                in
                Vec2.apply (Vec2.apply (Vec2.apply (Vec2.pure (<<)) f1) f2) x
                    |> Expect.equal (Vec2.apply f1 (Vec2.apply f2 x))
        ]


liftA1 : Test
liftA1 =
    describe "liftA1"
        [ test "lifts and applies unary function" <|
            \_ ->
                Vec2.liftA1 not (Vec2 True False)
                    |> Expect.equal (Vec2 False True)
        ]


liftA2 : Test
liftA2 =
    describe "liftA2"
        [ test "lifts and applies binary function" <|
            \_ ->
                Vec2.liftA2 (+) (Vec2 1 2) (Vec2 10 20)
                    |> Expect.equal (Vec2 11 22)
        ]


liftA3 : Test
liftA3 =
    describe "liftA3"
        [ test "lifts and applies ternary function" <|
            \_ ->
                Vec2.liftA3 clamp (Vec2 0 0) (Vec2 100 200) (Vec2 200 300)
                    |> Expect.equal (Vec2 100 200)
        ]



-- MONAD


bind : Test
bind =
    let
        createVec : Int -> Types.Vec2 (List String)
        createVec n =
            Vec2.vec2 (List.repeat n "x") (List.repeat n "y")

        myVec =
            Vec2.vec2 1 2

        expectedResult =
            Vec2 [ "x" ] [ "y", "y" ]
    in
    describe "bind/andThen"
        [ test "bind" <|
            \_ ->
                Vec2.bind createVec myVec
                    |> Expect.equal expectedResult
        , test "andThen" <|
            \_ ->
                Vec2.andThen createVec myVec
                    |> Expect.equal expectedResult
        ]


return : Test
return =
    describe
        "return"
        [ test "initializes all fields" <|
            \_ ->
                Vec2.return True
                    |> Expect.equal (Vec2 True True)
        ]


monadLaws : Test
monadLaws =
    describe
        "Monad laws"
        [ fuzz (Fuzz.map2 Vec2 int int) "Left identity" <|
            \n ->
                let
                    f x =
                        Vec2 x x
                in
                Vec2.bind f (Vec2.return n)
                    |> Expect.equal (f n)
        , fuzz (Fuzz.map2 (,) int int) "Right identity" <|
            \( n1, n2 ) ->
                let
                    m =
                        Vec2 n1 n2
                in
                Vec2.bind Vec2.return m
                    |> Expect.equal m
        , fuzz (Fuzz.map2 (,) int int) "Associativity" <|
            \( n1, n2 ) ->
                let
                    f x =
                        Vec2.pure (x + n1)

                    g x =
                        Vec2.pure (x - n2)

                    m =
                        Vec2 n1 n2
                in
                Vec2.bind g (Vec2.bind f m)
                    |> Expect.equal (Vec2.bind (\x -> Vec2.bind g (f x)) m)
        ]



-- -- FOLDABLE


foldl : Test
foldl =
    describe "foldl"
        [ fuzz (Fuzz.map2 (,) int int) "conses from left to right" <|
            \( n1, n2 ) ->
                Vec2.foldl (::) [] (Vec2 n1 n2)
                    |> Expect.equal [ n2, n1 ]
        ]


foldl1 : Test
foldl1 =
    describe "foldl1"
        [ fuzz (Fuzz.map2 (,) int int) "subtracts from left to right" <|
            \( n1, n2 ) ->
                Vec2.foldl1 (-) (Vec2 n1 n2)
                    |> Expect.equal (n1 - n2)
        ]


foldr : Test
foldr =
    describe "foldr"
        [ fuzz (Fuzz.map2 (,) int int) "conses from right to left" <|
            \( n1, n2 ) ->
                Vec2.foldl (::) [] (Vec2 n1 n2)
                    |> Expect.equal [ n2, n1 ]
        ]


foldr1 : Test
foldr1 =
    describe "foldr1"
        [ fuzz (Fuzz.map2 (,) int int) "subtracts from right to left" <|
            \( n1, n2 ) ->
                Vec2.foldr1 (-) (Vec2 n1 n2)
                    |> Expect.equal (n2 - n1)
        ]



-- GET / SET / UPDATE


getNth0 : Test
getNth0 =
    describe "getNth0"
        [ fuzz (Fuzz.map2 (,) int int) "takes element at position 0" <|
            \( n0, n1 ) ->
                Vec2.getNth0 (Vec2 n0 n1)
                    |> Expect.equal n0
        ]


getNth1 : Test
getNth1 =
    describe "getNth1"
        [ fuzz (Fuzz.map2 (,) int int) "takes element at position 1" <|
            \( n0, n1 ) ->
                Vec2.getNth1 (Vec2 n0 n1)
                    |> Expect.equal n1
        ]


setNth0 : Test
setNth0 =
    describe "setNth0"
        [ fuzz (Fuzz.map3 (,,) int int int) "sets element at position 0" <|
            \( n0, n1, n2 ) ->
                Vec2.setNth0 n0 (Vec2 n1 n2)
                    |> Expect.equal (Vec2 n0 n2)
        ]


setNth1 : Test
setNth1 =
    describe "setNth1"
        [ fuzz (Fuzz.map3 (,,) int int int) "sets element at position 1" <|
            \( n0, n1, n2 ) ->
                Vec2.setNth1 n0 (Vec2 n1 n2)
                    |> Expect.equal (Vec2 n1 n0)
        ]


mapNth0 : Test
mapNth0 =
    describe "mapNth0"
        [ fuzz (Fuzz.map2 (,) bool bool) "maps over element at position 0" <|
            \( b0, b1 ) ->
                Vec2.mapNth0 not (Vec2 b0 b1)
                    |> Expect.equal (Vec2 (not b0) b1)
        ]


mapNth1 : Test
mapNth1 =
    describe "mapNth1"
        [ fuzz (Fuzz.map2 (,) bool bool) "maps over element at position 0" <|
            \( b0, b1 ) ->
                Vec2.mapNth1 not (Vec2 b0 b1)
                    |> Expect.equal (Vec2 b0 (not b1))
        ]



-- CONVERSIONS


toTuple : Test
toTuple =
    describe "toTuple"
        [ fuzz (Fuzz.map2 (,) int int) "makes a 2-tuple" <|
            \( n0, n1 ) ->
                Vec2.toTuple (Vec2 n0 n1)
                    |> Expect.equal ( n0, n1 )
        ]


fromTuple : Test
fromTuple =
    describe "fromTuple"
        [ fuzz (Fuzz.map2 (,) int int) "makes a Vec2 from a 2-tuple" <|
            \( n0, n1 ) ->
                Vec2.fromTuple ( n0, n1 )
                    |> Expect.equal (Vec2 n0 n1)
        ]


toList : Test
toList =
    describe "toList"
        [ fuzz (Fuzz.map2 (,) int int) "makes a list of length 2" <|
            \( n0, n1 ) ->
                Vec2.toList (Vec2 n0 n1)
                    |> Expect.equal [ n0, n1 ]
        ]


fromList : Test
fromList =
    describe "fromList"
        [ fuzz (Fuzz.map2 (,) int int) "makes a vector from a length 2 list" <|
            \( n0, n1 ) ->
                Vec2.fromList [ n0, n1 ]
                    |> Expect.equal (Just (Vec2 n0 n1))
        , let
            intWithout n =
                Fuzz.conditional
                    { retries = 0, fallback = always 0, condition = (==) n }
                    int
          in
          fuzz (Fuzz.map2 (,) (intWithout 2) int) "makes no vector from lists of length /= 2" <|
            \( n0, n1 ) ->
                Vec2.fromList (List.repeat n0 n1)
                    |> Expect.equal Nothing
        ]



-- OTHER OPERATIONS


reverse : Test
reverse =
    describe "reverse"
        [ fuzz (Fuzz.map2 (,) int int) "reverses the vector elements" <|
            \( n0, n1 ) ->
                Vec2.reverse (Vec2 n0 n1)
                    |> Expect.equal (Vec2 n1 n0)
        ]


asArgs : Test
asArgs =
    describe "asArgs"
        [ fuzz (Fuzz.map2 (,) int int) "use vector elements as function arguments" <|
            \( n0, n1 ) ->
                Vec2.asArgs (+) (Vec2 n0 n1)
                    |> Expect.equal (n0 + n1)
        ]
