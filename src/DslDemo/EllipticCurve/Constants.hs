-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Constants
  ( a,
    b,
    p,
    n,
    glvA1,
    glvB1,
    glvA2,
    glvB2,
    beta,
  )
where

a :: Integer
a = 0

b :: Integer
b = 7

p :: Integer
p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f

n :: Integer
n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

-- ## GLV.
-- https://github.com/bitcoin-core/secp256k1/blob/master/src/scalar_impl.h
-- https://github.com/bitcoin-core/secp256k1/issues/1798
-- https://github.com/bitcoin-core/secp256k1/blob/master/src/field.h
glvA1 :: Integer
glvA1 = 0x3086d221a7d46bcde86c90e49284eb15

glvB1 :: Integer
glvB1 = -0xe4437ed6010e88286f547fa90abfe4c3

glvA2 :: Integer
glvA2 = 0x114ca50f7a8e2f3f657c1108d9d44cfd8

glvB2 :: Integer
glvB2 = 0x3086d221a7d46bcde86c90e49284eb15

beta :: Integer
beta = 0x7ae96a2b657c07106e64479eac3434e99cf0497512f58995c1396c28719501ee
