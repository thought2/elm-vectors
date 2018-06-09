module Vec2Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (bool, int)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import Types exposing (..)
import Vec2 exposing (..)


-- TYPE CONSTRUCTORS


testVec2 : Test
testVec2 =
    describe
        "vec2"
        [ test "behaves like the real constructor" <|
            \_ ->
                vec2 True False
                    |> Expect.equal (Vec2 True False)
        ]



-- FUNCTOR


testMap : Test
testMap =
    describe
        "map"
        [ test "maps over all fields" <|
            \_ ->
                map not (Vec2 True False)
                    |> Expect.equal (Vec2 False True)
        ]


functorLawsIdentity : () -> Test
functorLawsIdentity _ =
    fuzz (Fuzz.map2 Vec2 int int) "Identity" <|
        \fuzzyIntVec ->
            map identity fuzzyIntVec
                |> Expect.equal fuzzyIntVec


functorLawsComposition : () -> Test
functorLawsComposition _ =
    fuzz3 (Fuzz.map2 Vec2 int int) int int "Composition" <|
        \fuzzyIntVec n1 n2 ->
            let
                f =
                    (+) n1

                g =
                    (*) n2
            in
            map (f << g) fuzzyIntVec
                |> Expect.equal ((map f << map g) fuzzyIntVec)


functorLaws : Test
functorLaws =
    describe
        "Functor laws"
        [ functorLawsIdentity ()
        , functorLawsComposition ()
        ]



-- APPLICATIVE


testPure : Test
testPure =
    describe
        "pure"
        [ test "initializes all fields" <|
            \_ ->
                pure True
                    |> Expect.equal (Vec2 True True)
        ]


testApply : Test
testApply =
    describe "apply"
        [ test "applies functions in the context" <|
            \_ ->
                apply (Vec2 not not) (Vec2 True False)
                    |> Expect.equal (Vec2 False True)
        ]


applicativeLawsIdentity : () -> Test
applicativeLawsIdentity _ =
    fuzz (Fuzz.map2 Vec2 int int) "Identity" <|
        \fuzzyIntVec ->
            apply (pure identity) fuzzyIntVec
                |> Expect.equal fuzzyIntVec


applicativeLawsHomomorphism : () -> Test
applicativeLawsHomomorphism _ =
    fuzz2 int int "Homomorphism" <|
        \n1 n2 ->
            let
                f =
                    (+) n1
            in
            apply (pure f) (pure n2)
                |> Expect.equal (pure (f n2))


applicativeLawsInterchange : () -> Test
applicativeLawsInterchange _ =
    fuzz2 int int "Interchange" <|
        \n1 n2 ->
            let
                f =
                    pure ((+) n1)
            in
            apply f (pure n2)
                |> Expect.equal (apply (pure ((|>) n2)) f)


applicativeLawsComposition : () -> Test
applicativeLawsComposition _ =
    fuzz3 int int int "Composition" <|
        \n1 n2 n3 ->
            let
                f1 =
                    pure ((+) n1)

                f2 =
                    pure ((-) n2)

                x =
                    pure n3
            in
            apply (apply (apply (pure (<<)) f1) f2) x
                |> Expect.equal (apply f1 (apply f2 x))


applicativeLaws : Test
applicativeLaws =
    describe
        "Applicative laws"
        [ applicativeLawsIdentity ()
        , applicativeLawsHomomorphism ()
        , applicativeLawsInterchange ()
        , applicativeLawsComposition ()
        ]


testAndMap : Test
testAndMap =
    describe "andMap"
        [ fuzz2 int int "behaves like apply" <|
            \n0 n1 ->
                let
                    x =
                        Vec2 n0 n1

                    f =
                        pure (\x -> x + x)
                in
                andMap x f
                    |> Expect.equal (apply f x)
        ]


testMap2 : Test
testMap2 =
    describe
        "map2"
        [ test "maps over all fields of 2 vectors" <|
            \_ ->
                map2 (\x y -> x :: y :: []) (Vec2 1 2) (Vec2 2 1)
                    |> Expect.equal (Vec2 [ 1, 2 ] [ 2, 1 ])
        ]


testMap3 : Test
testMap3 =
    describe
        "map3"
        [ test "maps over all fields of 3 vectors" <|
            \_ ->
                map3 (\x y z -> x :: y :: z :: []) (Vec2 1 3) (Vec2 2 2) (Vec2 3 1)
                    |> Expect.equal (Vec2 [ 1, 2, 3 ] [ 3, 2, 1 ])
        ]



-- MONAD


testBind : Test
testBind =
    let
        createVec : Int -> Types.Vec2 (List String)
        createVec n =
            vec2 (List.repeat n "x") (List.repeat n "y")

        myVec =
            vec2 1 2

        expectedResult =
            Vec2 [ "x" ] [ "y", "y" ]
    in
    describe "bind/andThen"
        [ test "bind" <|
            \_ ->
                bind createVec myVec
                    |> Expect.equal expectedResult
        , test "andThen" <|
            \_ ->
                andThen createVec myVec
                    |> Expect.equal expectedResult
        ]


testReturn : Test
testReturn =
    describe
        "return"
        [ test "initializes all fields" <|
            \_ ->
                return True
                    |> Expect.equal (Vec2 True True)
        ]


monadLawsLeftIdentity : () -> Test
monadLawsLeftIdentity _ =
    fuzz (Fuzz.map2 Vec2 int int) "Left identity" <|
        \n ->
            let
                f x =
                    Vec2 x x
            in
            bind f (return n)
                |> Expect.equal (f n)


monadLawsRightIdentity : () -> Test
monadLawsRightIdentity _ =
    fuzz2 int int "Right identity" <|
        \n1 n2 ->
            let
                m =
                    Vec2 n1 n2
            in
            bind return m
                |> Expect.equal m


monadLawsAssociativity : () -> Test
monadLawsAssociativity _ =
    fuzz2 int int "Associativity" <|
        \n1 n2 ->
            let
                f x =
                    pure (x + n1)

                g x =
                    pure (x - n2)

                m =
                    Vec2 n1 n2
            in
            bind g (bind f m)
                |> Expect.equal (bind (\x -> bind g (f x)) m)


monadLaws : Test
monadLaws =
    describe
        "Monad laws"
        [ monadLawsLeftIdentity ()
        , monadLawsRightIdentity ()
        , monadLawsAssociativity ()
        ]



-- -- FOLDABLE


testFoldl : Test
testFoldl =
    describe "foldl"
        [ fuzz2 int int "conses from left to right" <|
            \n1 n2 ->
                foldl (::) [] (Vec2 n1 n2)
                    |> Expect.equal [ n2, n1 ]
        ]


testFoldl1 : Test
testFoldl1 =
    describe "foldl1"
        [ fuzz2 int int "subtracts from left to right" <|
            \n1 n2 ->
                foldl1 (-) (Vec2 n1 n2)
                    |> Expect.equal (n1 - n2)
        ]


testFoldr : Test
testFoldr =
    describe "foldr"
        [ fuzz2 int int "conses from right to left" <|
            \n1 n2 ->
                foldl (::) [] (Vec2 n1 n2)
                    |> Expect.equal [ n2, n1 ]
        ]


testFoldr1 : Test
testFoldr1 =
    describe "foldr1"
        [ fuzz2 int int "subtracts from right to left" <|
            \n1 n2 ->
                foldr1 (-) (Vec2 n1 n2)
                    |> Expect.equal (n2 - n1)
        ]



-- GET / SET / UPDATE


testGetNth0 : Test
testGetNth0 =
    describe "getNth0"
        [ fuzz2 int int "takes element at position 0" <|
            \n0 n1 ->
                getNth0 (Vec2 n0 n1)
                    |> Expect.equal n0
        ]


testGetNth1 : Test
testGetNth1 =
    describe "getNth1"
        [ fuzz2 int int "takes element at position 1" <|
            \n0 n1 ->
                getNth1 (Vec2 n0 n1)
                    |> Expect.equal n1
        ]


testSetNth0 : Test
testSetNth0 =
    describe "setNth0"
        [ fuzz3 int int int "sets element at position 0" <|
            \n0 n1 n2 ->
                setNth0 n0 (Vec2 n1 n2)
                    |> Expect.equal (Vec2 n0 n2)
        ]


testSetNth1 : Test
testSetNth1 =
    describe "setNth1"
        [ fuzz3 int int int "sets element at position 1" <|
            \n0 n1 n2 ->
                setNth1 n0 (Vec2 n1 n2)
                    |> Expect.equal (Vec2 n1 n0)
        ]


testMapNth0 : Test
testMapNth0 =
    describe "mapNth0"
        [ fuzz2 bool bool "maps over element at position 0" <|
            \b0 b1 ->
                mapNth0 not (Vec2 b0 b1)
                    |> Expect.equal (Vec2 (not b0) b1)
        ]


testMapNth1 : Test
testMapNth1 =
    describe "mapNth1"
        [ fuzz2 bool bool "maps over element at position 0" <|
            \b0 b1 ->
                mapNth1 not (Vec2 b0 b1)
                    |> Expect.equal (Vec2 b0 (not b1))
        ]



-- CONVERSIONS


testToTuple : Test
testToTuple =
    describe "toTuple"
        [ fuzz2 int int "makes a 2-tuple" <|
            \n0 n1 ->
                toTuple (Vec2 n0 n1)
                    |> Expect.equal ( n0, n1 )
        ]


testFromTuple : Test
testFromTuple =
    describe "fromTuple"
        [ fuzz (Fuzz.map2 (,) int int) "makes a Vec2 from a 2-tuple" <|
            \( n0, n1 ) ->
                fromTuple ( n0, n1 )
                    |> Expect.equal (Vec2 n0 n1)
        ]


testToList : Test
testToList =
    describe "toList"
        [ fuzz2 int int "makes a list of length 2" <|
            \n0 n1 ->
                toList (Vec2 n0 n1)
                    |> Expect.equal [ n0, n1 ]
        ]


testFromList : Test
testFromList =
    describe "fromList"
        [ fuzz2 int int "makes a vector from a length 2 list" <|
            \n0 n1 ->
                fromList [ n0, n1 ]
                    |> Expect.equal (Just (Vec2 n0 n1))
        , let
            intWithout n =
                Fuzz.conditional
                    { retries = 0, fallback = always 0, condition = (==) n }
                    int
          in
          fuzz2 (intWithout 2) int "makes no vector from lists of length /= 2" <|
            \n0 n1 ->
                fromList (List.repeat n0 n1)
                    |> Expect.equal Nothing
        ]



-- OTHER OPERATIONS


testReverse : Test
testReverse =
    describe "reverse"
        [ fuzz2 int int "reverses the vector elements" <|
            \n0 n1 ->
                reverse (Vec2 n0 n1)
                    |> Expect.equal (Vec2 n1 n0)
        ]


testAsArgs : Test
testAsArgs =
    describe "asArgs"
        [ fuzz2 int int "use vector elements as function arguments" <|
            \n0 n1 ->
                asArgs (+) (Vec2 n0 n1)
                    |> Expect.equal (n0 + n1)
        ]
