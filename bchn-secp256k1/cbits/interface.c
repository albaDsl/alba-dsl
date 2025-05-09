// Copyright (c) 2025 albaDsl

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <secp256k1.h>
#include <secp256k1_schnorr.h>

/* uint8_t hextobin(const char *str, uint8_t *bytes, size_t blen); */

/* int verifySchnorrSig(uint8_t *pubKeyData, uint8_t *sigData, uint8_t *msgData) { */
/*   secp256k1_context *ctx; */
/*   int res; */

/*   ctx = secp256k1_context_create(0x0101); */
/*   res = secp256k1_schnorr_verify(ctx, sigData, msgData, */
/*                                  (secp256k1_pubkey *)pubKeyData); */
/*   secp256k1_context_destroy(ctx); */
/*   return res; */
/* } */

/* int verifyEcdsaSig(uint8_t *pubKeyData, uint8_t *sigData, uint8_t *msgData) { */
/*   secp256k1_context *ctx; */
/*   int res; */

/*   ctx = secp256k1_context_create(0x0101); */
/*   res = secp256k1_ecdsa_verify(ctx, (secp256k1_ecdsa_signature *)sigData, */
/*                                msgData, (secp256k1_pubkey *)pubKeyData); */
/*   secp256k1_context_destroy(ctx); */
/*   return res; */
/* } */
