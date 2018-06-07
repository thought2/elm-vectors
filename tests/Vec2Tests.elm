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



-- -- GET / UPDATE
-- mapNth0 : Test
-- mapNth0 =
--     describe ""
--         [ test "mapNth0" <|
--             \_ ->
--                 Vec2.mapNth0 1
--                     |> Expect.equal ()
--         ]
-- mapNth1 : Test
-- mapNth1 =
--     describe ""
--         [ test "mapNth1" <|
--             \_ ->
--                 Vec2.mapNth1 1
--                     |> Expect.equal ()
--         ]
-- getNth0 : Test
-- getNth0 =
--     describe ""
--         [ test "getNth0" <|
--             \_ ->
--                 Vec2.getNth0 1
--                     |> Expect.equal ()
--         ]
-- getNth1 : Test
-- getNth1 =
--     describe ""
--         [ test "getNth1" <|
--             \_ ->
--                 Vec2.getNth1 1
--                     |> Expect.equal ()
--         ]
-- -- CONVERSIONS
-- toTuple : Test
-- toTuple =
--     describe ""
--         [ test "toTuple" <|
--             \_ ->
--                 Vec2.toTuple 1
--                     |> Expect.equal ()
--         ]
-- fromTuple : Test
-- fromTuple =
--     describe ""
--         [ test "fromTuple" <|
--             \_ ->
--                 Vec2.fromTuple 1
--                     |> Expect.equal ()
--         ]
-- toList : Test
-- toList =
--     describe ""
--         [ test "toList" <|
--             \_ ->
--                 Vec2.toList 1
--                     |> Expect.equal ()
--         ]
-- fromList : Test
-- fromList =
--     describe ""
--         [ test "fromList" <|
--             \_ ->
--                 Vec2.fromList 1
--                     |> Expect.equal ()
--         ]
-- -- OTHER OPERATIONS
-- reverse : Test
-- reverse =
--     describe ""
--         [ test "reverse" <|
--             \_ ->
--                 Vec2.reverse 1
--                     |> Expect.equal ()
--         ]
-- asArgs : Test
-- asArgs =
--     describe ""
--         [ test "asArgs" <|
--             \_ ->
--                 Vec2.asArgs 1
--                     |> Expect.equal ()
--         ]
