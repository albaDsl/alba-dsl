-- Copyright (c) 2025 albaDsl

module TestLibauthVectorsExclusions2026 where

import Data.Text qualified as T

excludeStandard :: [T.Text]
excludeStandard =
  [ "sl0m2j", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "ye8js7", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "snxadr", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "wcj4kf", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "hjzty5", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "tqqc7u", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "2td6aq", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "s6e3g4", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "yrv5kg", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "ewyrd8", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "6ls5vh", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "f8ldd4", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "vtusz6", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "986pn3", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "umgppv", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "w95r53", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "xseywy", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "s2a64m", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "33a9nr", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "5jlz6j", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "4s9zm8", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "azq078", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "ltr5yx", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "he9ltd", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "30qpjf", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "yk9g97", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "pwran7", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "ll48n6", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "earn55", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "mxmwrr", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "3hlq32", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "vqmyvn", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "r2usf6", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "zsscpw", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "p9ncaz", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "89dn79", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "af0enq", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "hafrhn", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "dvm3p0", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "8t65vp", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "7nsx5a", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "838k9p", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "pqydxn", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "ug0w55", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "7y55wj", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "27tjfx", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "kx0rh5", -- failed with SeBadOpcode "OP_UNUSED OP_VERIF"
    "dyxfml", -- validation failure VfTxScriptPubKeySize
    "pwuxfd", -- validation failure VfTxScriptPubKeySize
    "dg2e2u", -- validation failure VfTxScriptPubKeySize
    "ntndgy" -- validation failure VfTxScriptPubKeySize
  ]

excludeNonStandardInStandardMode :: [T.Text]
excludeNonStandardInStandardMode =
  [ "u33zrd", -- passed validation.
    "vd72vn", -- passed validation.
    "nsw96e", -- passed validation.
    "deq7jk", -- passed validation.
    "4z3ggm", -- passed validation.
    "0cgg6z", -- passed validation.
    "v2p56a", -- passed validation.
    "fe2jpa", -- passed validation.
    "lzpd5s", -- passed validation.
    "d38r9m", -- passed validation.
    "t8zltz", -- passed validation.
    "pjlgea", -- passed validation.
    "vr3wjd", -- passed validation.
    "agljhp", -- passed validation.
    "2au3wt", -- passed validation.
    "qaz95k", -- passed validation.
    "kna366", -- passed validation.
    "7k09dy", -- passed validation.
    "8u2vtn", -- passed validation.
    "z3q52r", -- passed validation.
    "cxytme", -- passed validation.
    "lfm0mw", -- passed validation.
    "80l4wu", -- passed validation.
    "465xxr", -- passed validation.
    "vkc3tk", -- passed validation.
    "vxc6ku", -- passed validation.
    "dqxeyg", -- passed validation.
    "jkcusz", -- passed validation.
    "svw6qy", -- passed validation.
    "rgwmf8", -- passed validation.
    "673q2s", -- passed validation.
    "6k8e2w", -- passed validation.
    "up5err", -- passed validation.
    "fn6s3t", -- passed validation.
    "tafanh", -- passed validation.
    "nx6mx8", -- passed validation.
    "jnhzst", -- passed validation.
    "jfm62q", -- passed validation.
    "kr62gp", -- passed validation.
    "y2je2y", -- passed validation.
    "8ylwra", -- passed validation.
    "m0aprc", -- passed validation.
    "pnre84", -- passed validation.
    "z3sjs4", -- passed validation.
    "9ga8v8", -- passed validation.
    "28urur", -- passed validation.
    "dl4q97", -- passed validation.
    "jp0ase", -- passed validation.
    "4y85zq", -- passed validation.
    "tag2ez", -- passed validation.
    "qv9a03", -- passed validation.
    "tm0j3u", -- passed validation.
    "236zhk", -- passed validation.
    "hdyexe", -- passed validation.
    "hlrhs5", -- passed validation.
    "urx4u0", -- passed validation.
    "glj96c", -- passed validation.
    "7a3f28", -- passed validation.
    "8jcu48", -- passed validation.
    "qa6uek", -- passed validation.
    "wrc6v7", -- passed validation.
    "ql8nql", -- passed validation.
    "gt2may", -- passed validation.
    "9d7zh2", -- passed validation.
    "x48wxa" -- passed validation.
  ]

excludeInvalid :: [T.Text]
excludeInvalid =
  [ "h4d5nz", -- passed validation.
    "j8u53e", -- passed validation.
    "j07snz", -- passed validation.
    "ev6d70", -- passed validation.
    "euhyv9", -- passed validation.
    "9wpluu", -- passed validation.
    "0wv2x5", -- passed validation.
    "w0x57n", -- passed validation.
    "lgpn0r", -- passed validation.
    "5vxr09", -- passed validation.
    "d6akln", -- passed validation.
    "rwnd46", -- passed validation.
    "jcns45", -- passed validation.
    "9x8hfl", -- passed validation.
    "nsd77p", -- passed validation.
    "gx55hr", -- passed validation.
    "qwq9x7", -- passed validation.
    "95zm4e", -- passed validation.
    "dfum5p", -- passed validation.
    "p7gcms", -- passed validation.
    "lc48r2", -- passed validation.
    "wv9xsg", -- passed validation.
    "d2yg6y", -- passed validation.
    "nfuczn", -- passed validation.
    "yeqv20", -- passed validation.
    "eydg7p", -- passed validation.
    "a62s5a", -- passed validation.
    "3vfppz", -- passed validation.
    "8zsq5v", -- passed validation.
    "dll5z4", -- passed validation.
    "v35hdm", -- passed validation.
    "7ed7yx", -- passed validation.
    "k0l2q6", -- passed validation.
    "hxp23j", -- passed validation.
    "gqzxg5", -- passed validation.
    "6zz804", -- passed validation.
    "fqsjgt", -- passed validation.
    "dx7yvf", -- passed validation.
    "fl0mfm", -- passed validation.
    "pyq7py", -- passed validation.
    "lclhw9", -- passed validation.
    "8jzadl", -- passed validation.
    "lrsehz", -- passed validation.
    "uxghcx", -- passed validation.
    "p2sax0", -- passed validation.
    "j79fxv", -- passed validation.
    "mnkfz2", -- passed validation.
    "86d2rh", -- passed validation.
    "g4ljnx", -- passed validation.
    "dev6zj", -- passed validation.
    "s8zuvq", -- passed validation.
    "l80g62", -- passed validation.
    "skzw0z", -- passed validation.
    "89dq8w", -- passed validation.
    "q3j9jd", -- passed validation.
    "ze7jsp", -- passed validation.
    "csk3ul", -- passed validation.
    "z478f6", -- passed validation.
    "4z3jpd", -- passed validation.
    "hr2xhm", -- passed validation.
    "34am3h", -- passed validation.
    "d7yl06", -- passed validation.
    "c8nf5t", -- passed validation.
    "7dsdpk", -- passed validation.
    "69rvph", -- passed validation.
    "7lydtd", -- passed validation.
    "6tn6n5", -- passed validation.
    "q99r4u", -- passed validation.
    "hhaa78", -- passed validation.
    "aay9ku", -- passed validation.
    "krp5f8", -- passed validation.
    "3hg37t", -- passed validation.
    "dp6gmz", -- passed validation.
    "f5tq95", -- passed validation.
    "dardju", -- passed validation.
    "yw7eak", -- passed validation.
    "7n5cwh", -- passed validation.
    "nw0h0k", -- passed validation.
    "nujg5l", -- passed validation.
    "ca67jj", -- passed validation.
    "ntdz7l", -- passed validation.
    "e3y9nf", -- passed validation.
    "d6rm8p", -- passed validation.
    "eeufs2", -- passed validation.
    "ygxnjm", -- passed validation.
    "7smnd9", -- passed validation.
    "7my5fz", -- passed validation.
    "tla9ma", -- passed validation.
    "8mcm85", -- passed validation.
    "95hhd6", -- passed validation.
    "k06ga7", -- passed validation.
    "srmf6p", -- passed validation.
    "cj3dpd", -- passed validation.
    "3ek72q", -- passed validation.
    "myqddp", -- passed validation.
    "8g422u", -- passed validation.
    "90566r", -- passed validation.
    "6qnne9", -- passed validation.
    "d5yppm", -- passed validation.
    "405uht", -- passed validation.
    "snpzq2", -- passed validation.
    "lz7wrl", -- passed validation.
    "cw9jw5", -- passed validation.
    "c7ert2", -- passed validation.
    "l3qvsk", -- passed validation.
    "y6lg9q", -- passed validation.
    "0f235x", -- passed validation.
    "jemls0", -- passed validation.
    "gcp2dz", -- passed validation.
    "5muly8", -- passed validation.
    "ghga68", -- passed validation.
    "y57nxf", -- passed validation.
    "8m8h75", -- passed validation.
    "aump5c", -- passed validation.
    "595dvh", -- passed validation.
    "2h6dqj", -- passed validation.
    "qwv840", -- passed validation.
    "mqss4v", -- passed validation.
    "mq8fuf", -- passed validation.
    "zccu5d", -- passed validation.
    "4qq55d", -- passed validation.
    "ycl2h4", -- passed validation.
    "4cq8qy", -- passed validation.
    "esckzs", -- passed validation.
    "k793xh", -- passed validation.
    "qdmhep", -- passed validation.
    "rw4kcm", -- passed validation.
    "atfuqy", -- passed validation.
    "mnkcha", -- passed validation.
    "npjeqz", -- passed validation.
    "77qqky", -- passed validation.
    "4pwr9u", -- passed validation.
    "fd8v7f", -- passed validation.
    "ea6l4g", -- passed validation.
    "elhuna", -- passed validation.
    "k8p52s", -- passed validation.
    "0ccq6x", -- passed validation.
    "cmndcd", -- passed validation.
    "06k33j", -- passed validation.
    "6pqa2x", -- passed validation.
    "0yd5ag", -- passed validation.
    "jzx23z", -- passed validation.
    "6ppuv5", -- passed validation.
    "tgys5q", -- passed validation.
    "f8snxd", -- passed validation.
    "3fakll", -- passed validation.
    "py5lnv", -- passed validation.
    "tujdhd", -- passed validation.
    "hwkttf", -- passed validation.
    "hqk7rx", -- passed validation.
    "u572d2", -- passed validation.
    "p59w8m", -- passed validation.
    "urtzp3", -- passed validation.
    "m9tthm", -- passed validation.
    "wmgkmm", -- passed validation.
    "z24uvm", -- passed validation.
    "3wkhmt", -- passed validation.
    "dllkxt", -- passed validation.
    "4z2y8g", -- passed validation.
    "zlgs7t", -- passed validation.
    "r7yfmq", -- passed validation.
    "spm2kn", -- passed validation.
    "x7tjlf", -- passed validation.
    "qywrj9", -- passed validation.
    "du30ak", -- passed validation.
    "ds2u6p", -- passed validation.
    "2gxe0f", -- passed validation.
    "hdvvsp", -- passed validation.
    "2z4c54", -- passed validation.
    "srue73", -- passed validation.
    "uuceal", -- passed validation.
    "phcdds", -- passed validation.
    "kup5gf", -- passed validation.
    "rqzzhz", -- passed validation.
    "pjt5m4", -- passed validation.
    "qj09n7", -- passed validation.
    "wqtz5a", -- passed validation.
    "2clqrf", -- passed validation.
    "sj54sk", -- passed validation.
    "tf3el7", -- passed validation.
    "kks2kv", -- passed validation.
    "7u4sj7", -- passed validation.
    "c8mhmc", -- passed validation.
    "u3plnj", -- passed validation.
    "92rdpe", -- passed validation.
    "up0w2q", -- passed validation.
    "fhluzg", -- passed validation.
    "hx8t7g", -- passed validation.
    "qv32y8", -- passed validation.
    "auydhr", -- passed validation.
    "cynp8h", -- passed validation.
    "xa6zpa", -- passed validation.
    "3pej8l", -- passed validation.
    "wd9gpp", -- passed validation.
    "r8kzw5", -- passed validation.
    "u4gu6e", -- passed validation.
    "8sfl5x", -- passed validation.
    "9tcgmn", -- passed validation.
    "2h0gt6", -- passed validation.
    "hjgnl6", -- passed validation.
    "8vuanf", -- passed validation.
    "hnuent", -- passed validation.
    "gfsyd2", -- passed validation.
    "f5aj5t", -- passed validation.
    "qthfv2", -- passed validation.
    "dypunp", -- passed validation.
    "hzrlss", -- passed validation.
    "ns7tya", -- passed validation.
    "hm0dna", -- passed validation.
    "wkklzu", -- passed validation.
    "ngmn2v", -- passed validation.
    "gyvyym", -- passed validation.
    "2zngl0", -- passed validation.
    "yu87wy", -- passed validation.
    "0vdftj", -- passed validation.
    "s5759z", -- passed validation.
    "s9gyry", -- passed validation.
    "rexmu9", -- passed validation.
    "4a2e0w", -- passed validation.
    "j06tzg", -- passed validation.
    "6x5tsq", -- passed validation.
    "9v3dm8", -- passed validation.
    "xjx557", -- passed validation.
    "f5zax6", -- passed validation.
    "5tdjr8", -- passed validation.
    "p3pnk0", -- passed validation.
    "9yuq53", -- passed validation.
    "yadhuc", -- passed validation.
    "2l6fp9", -- passed validation.
    "umh4zh", -- passed validation.
    "7pt03f", -- passed validation.
    "jgjd7l", -- passed validation.
    "rn5vlr", -- passed validation.
    "f7p7a7", -- passed validation.
    "ey0mwu", -- passed validation.
    "aexfnq", -- passed validation.
    "mxvlq6", -- passed validation.
    "pd8et3", -- passed validation.
    "ycstnl", -- passed validation.
    "r09hvw", -- passed validation.
    "zd4s4d", -- passed validation.
    "pfqcf4", -- passed validation.
    "atr9nq", -- passed validation.
    "jnst4x", -- passed validation.
    "hs9q0g", -- passed validation.
    "7mdnnp", -- passed validation.
    "3mhmuf", -- passed validation.
    "lxhcvj", -- passed validation.
    "ld8z5g", -- passed validation.
    "x96wut", -- passed validation.
    "73h74c", -- passed validation.
    "0fjrvm", -- passed validation.
    "ek6n8x", -- passed validation.
    "2hlfym", -- passed validation.
    "3xahtq", -- passed validation.
    "u2wh4h", -- passed validation.
    "h7f2p5", -- passed validation.
    "p3lda2", -- passed validation.
    "xu9exl", -- passed validation.
    "wkn2vm", -- passed validation.
    "6rc9dk", -- passed validation.
    "wd3tgd", -- passed validation.
    "6hj8wx", -- passed validation.
    "r225jg", -- passed validation.
    "a3jd4z", -- passed validation.
    "gvhdzx", -- passed validation.
    "4rut9j", -- passed validation.
    "4fyk9p", -- passed validation.
    "ved87x", -- passed validation.
    "q6u0zx", -- passed validation.
    "hc5awx", -- passed validation.
    "wck0pj", -- passed validation.
    "9c23mf", -- passed validation.
    "zxu4kq", -- passed validation.
    "pl3dla", -- passed validation.
    "0k87ea", -- passed validation.
    "htu4dj", -- passed validation.
    "tar82w", -- passed validation.
    "fangsj", -- passed validation.
    "rjmtl4", -- passed validation.
    "9qjdkl", -- passed validation.
    "v9ermw", -- passed validation.
    "x8vpqq", -- passed validation.
    "wpey5s", -- passed validation.
    "0asa3s", -- passed validation.
    "ggwu8k", -- passed validation.
    "h2ffvg", -- passed validation.
    "50gxf5", -- passed validation.
    "u99xgt", -- passed validation.
    "rh2vcm", -- passed validation.
    "q2zfpx", -- passed validation.
    "8rawcp", -- passed validation.
    "qzeg0n", -- passed validation.
    "66t0r4", -- passed validation.
    "zlrwkn", -- passed validation.
    "qjz52q", -- passed validation.
    "ejm005", -- passed validation.
    "c8xqks", -- passed validation.
    "ds37ce", -- passed validation.
    "lnkqgk", -- passed validation.
    "xmvka6", -- passed validation.
    "ljm8qs", -- passed validation.
    "k6p2mw", -- passed validation.
    "3c3a78", -- passed validation.
    "vw4l9f", -- passed validation.
    "qmgzru", -- passed validation.
    "qxsa38", -- passed validation.
    "qhvxwq", -- passed validation.
    "6thlj9", -- passed validation.
    "e2wjpx", -- passed validation.
    "7hy96r", -- passed validation.
    "8h2qcq", -- passed validation.
    "xk5x92", -- passed validation.
    "5gwp9p", -- passed validation.
    "vr7wfl", -- passed validation.
    "d4zpnr", -- passed validation.
    "3xsk3c", -- passed validation.
    "u2c28c", -- passed validation.
    "x2tpjr", -- passed validation.
    "eunfcw", -- passed validation.
    "c5ag9h", -- passed validation.
    "xny5c3", -- passed validation.
    "texldg", -- passed validation.
    "fzxruv", -- passed validation.
    "yyt29m", -- passed validation.
    "vhkref", -- passed validation.
    "patu5e", -- passed validation.
    "rgpalt", -- passed validation.
    "u2fvsv", -- passed validation.
    "p6e42f", -- passed validation.
    "fffg2r", -- passed validation.
    "ew8qnf", -- passed validation.
    "ymhef8", -- passed validation.
    "afkkjm", -- passed validation.
    "84w0gu", -- passed validation.
    "e28lj3", -- passed validation.
    "uzy4a8", -- passed validation.
    "5xchcr", -- passed validation.
    "hy3gpk", -- passed validation.
    "k286fj", -- passed validation.
    "aacnah", -- passed validation.
    "xz32lv", -- passed validation.
    "5us8hd", -- passed validation.
    "ecnpkx", -- passed validation.
    "3vhtmk", -- passed validation.
    "5978ek", -- passed validation.
    "4h2rwc", -- passed validation.
    "d4qme9", -- passed validation.
    "j6yhah", -- passed validation.
    "8uez8l", -- passed validation.
    "97z473", -- passed validation.
    "22xq2l", -- passed validation.
    "7akxn9", -- passed validation.
    "l5xstx", -- passed validation.
    "4sth43", -- passed validation.
    "awcrug", -- passed validation.
    "gyywrm", -- passed validation.
    "v4cgfh", -- passed validation.
    "qve3h0", -- passed validation.
    "cwsalg", -- passed validation.
    "grh3py", -- passed validation.
    "gzkg2p", -- passed validation.
    "9lgfc3", -- passed validation.
    "pmfdzt", -- passed validation.
    "hrayw4", -- passed validation.
    "fla645", -- passed validation.
    "3c5v4h", -- passed validation.
    "ykkxm5", -- passed validation.
    "ug2s67", -- passed validation.
    "h8ege5", -- passed validation.
    "9f2a3k", -- passed validation.
    "ucy8vs", -- passed validation.
    "2dw2dn", -- passed validation.
    "wjqpz3", -- passed validation.
    "pa8tu2", -- passed validation.
    "mrwz0t", -- passed validation.
    "vwy68l", -- passed validation.
    "36n40p", -- passed validation.
    "mqy5yn", -- passed validation.
    "lf6d0w", -- passed validation.
    "5v4jdg", -- passed validation.
    "0tenug", -- passed validation.
    "cntgu5", -- passed validation.
    "ngu8tq", -- passed validation.
    "p92u3n", -- passed validation.
    "2jnufh", -- passed validation.
    "zc8sy2", -- passed validation.
    "5aq0az", -- passed validation.
    "kjph8w", -- passed validation.
    "swt5f7", -- passed validation.
    "9smt2s", -- passed validation.
    "vt8sx2", -- passed validation.
    "asty0y", -- passed validation.
    "gyfdph", -- passed validation.
    "rca7m8", -- passed validation.
    "5jkkp3", -- passed validation.
    "7gu4yw", -- passed validation.
    "d83gze", -- passed validation.
    "vkwe83", -- passed validation.
    "xq7ulx", -- passed validation.
    "lar3jw", -- passed validation.
    "70n3yd", -- passed validation.
    "k5czud", -- passed validation.
    "886v0c", -- passed validation.
    "cda6dl", -- passed validation.
    "gk79dd", -- passed validation.
    "g9yacw", -- passed validation.
    "pkesre", -- passed validation.
    "dwwvgx", -- passed validation.
    "3ydry0", -- passed validation.
    "x7t6ut", -- passed validation.
    "vgn2ln", -- passed validation.
    "lmwn3q", -- passed validation.
    "k4ft7z", -- passed validation.
    "tyc8xe", -- passed validation.
    "359a3l", -- passed validation.
    "5rqkaj", -- passed validation.
    "0m55jv", -- passed validation.
    "8w40wm", -- passed validation.
    "w3hsga", -- passed validation.
    "5ed0a5", -- passed validation.
    "60p3t2", -- passed validation.
    "whr82n", -- passed validation.
    "4unqan", -- passed validation.
    "tz7tup", -- passed validation.
    "0eu4ye", -- passed validation.
    "s5mju3", -- passed validation.
    "zm7kry", -- passed validation.
    "s4d8y9", -- passed validation.
    "kz6rth", -- passed validation.
    "2kxfhn", -- passed validation.
    "6fkdv5" -- passed validation.
  ]
