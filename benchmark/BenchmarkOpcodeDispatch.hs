-- Copyright (c) 2025 albaDsl

module BenchmarkOpcodeDispatch where

import Criterion.Main
import Data.Map.Strict qualified as M
import Data.Vector qualified as V
import Prelude hiding (sum)

{- ORMOLU_DISABLE -}
data Op
  = OP1 | OP2 | OP3 | OP4 | OP5 | OP6 | OP7 | OP8 | OP9 | OP10 | OP11 | OP12
  | OP13 | OP14 | OP15 | OP16 | OP17 | OP18 | OP19 | OP20 | OP21 | OP22 | OP23
  | OP24 | OP25 | OP26 | OP27 | OP28 | OP29 | OP30 | OP31 | OP32 | OP33 | OP34
  | OP35 | OP36 | OP37 | OP38 | OP39 | OP40 | OP41 | OP42 | OP43 | OP44 | OP45
  | OP46 | OP47 | OP48 | OP49 | OP50 | OP51 | OP52 | OP53 | OP54 | OP55 | OP56
  | OP57 | OP58 | OP59 | OP60 | OP61 | OP62 | OP63 | OP64 | OP65 | OP66 | OP67
  | OP68 | OP69 | OP70 | OP71 | OP72 | OP73 | OP74 | OP75 | OP76 | OP77 | OP78
  | OP79 | OP80 | OP81 | OP82 | OP83 | OP84 | OP85 | OP86 | OP87 | OP88 | OP89
  | OP90 | OP91 | OP92 | OP93 | OP94 | OP95 | OP96 | OP97 | OP98 | OP99 | OP100
  | OP101 | OP102 | OP103 | OP104 | OP105 | OP106 | OP107 | OP108 | OP109
  | OP110 | OP111 | OP112 | OP113 | OP114 | OP115 | OP116 | OP117 | OP118
  | OP119 | OP120 | OP121 | OP122 | OP123 | OP124 | OP125 | OP126 | OP127
  | OP128 | OP129 | OP130 | OP131 | OP132 | OP133 | OP134 | OP135 | OP136
  | OP137 | OP138 | OP139 | OP140 | OP141 | OP142 | OP143 | OP144 | OP145
  | OP146 | OP147 | OP148 | OP149 | OP150 | OP151 | OP152 | OP153 | OP154
  | OP155 | OP156 | OP157 | OP158 | OP159 | OP160 | OP161 | OP162 | OP163
  | OP164 | OP165 | OP166 | OP167 | OP168 | OP169 | OP170 | OP171 | OP172
  | OP173 | OP174 | OP175 | OP176 | OP177 | OP178 | OP179 | OP180 | OP181
  | OP182 | OP183 | OP184 | OP185 | OP186 | OP187 | OP188 | OP189 | OP190
  | OP191 | OP192 | OP193 | OP194 | OP195 | OP196 | OP197 | OP198 | OP199
  | OP200 | OP201 | OP202 | OP203 | OP204 | OP205 | OP206 | OP207 | OP208
  | OP209 | OP210 | OP211 | OP212 | OP213 | OP214 | OP215 | OP216 | OP217
  | OP218 | OP219 | OP220 | OP221 | OP222 | OP223 | OP224 | OP225 | OP226
  | OP227 | OP228 | OP229 | OP230 | OP231 | OP232 | OP233 | OP234 | OP235
  | OP236 | OP237 | OP238 | OP239 | OP240 | OP241 | OP242 | OP243 | OP244
  | OP245 | OP246 | OP247 | OP248 | OP249 | OP250
  deriving (Eq, Ord, Enum)
{- ORMOLU_ENABLE -}

its :: Int
its = 1_000

opcodeDispatchBench :: Benchmark
opcodeDispatchBench =
  bgroup
    "opcode dispatch"
    [ bench "with case" $ nf (opcodeDispatch caseFun 0) its,
      bench "with map" $ nf (opcodeDispatch (mapFun opMap) 0) its,
      bench "with vector" $ nf (opcodeDispatch (vecFun opVec) 0) its
    ]

opcodeDispatch :: (Op -> Int -> Int) -> Int -> Int -> ()
opcodeDispatch _f !sum 0 =
  if sum == its
    then ()
    else error "Does not add up."
opcodeDispatch f sum n = opcodeDispatch f (f OP250 sum) (pred n)

caseFun :: Op -> Int -> Int
caseFun op x =
  case op of
    OP1 -> x
    OP2 -> x
    OP3 -> x
    OP4 -> x
    OP5 -> x
    OP6 -> x
    OP7 -> x
    OP8 -> x
    OP9 -> x
    OP10 -> x
    OP11 -> x
    OP12 -> x
    OP13 -> x
    OP14 -> x
    OP15 -> x
    OP16 -> x
    OP17 -> x
    OP18 -> x
    OP19 -> x
    OP20 -> x
    OP21 -> x
    OP22 -> x
    OP23 -> x
    OP24 -> x
    OP25 -> x
    OP26 -> x
    OP27 -> x
    OP28 -> x
    OP29 -> x
    OP30 -> x
    OP31 -> x
    OP32 -> x
    OP33 -> x
    OP34 -> x
    OP35 -> x
    OP36 -> x
    OP37 -> x
    OP38 -> x
    OP39 -> x
    OP40 -> x
    OP41 -> x
    OP42 -> x
    OP43 -> x
    OP44 -> x
    OP45 -> x
    OP46 -> x
    OP47 -> x
    OP48 -> x
    OP49 -> x
    OP50 -> x
    OP51 -> x
    OP52 -> x
    OP53 -> x
    OP54 -> x
    OP55 -> x
    OP56 -> x
    OP57 -> x
    OP58 -> x
    OP59 -> x
    OP60 -> x
    OP61 -> x
    OP62 -> x
    OP63 -> x
    OP64 -> x
    OP65 -> x
    OP66 -> x
    OP67 -> x
    OP68 -> x
    OP69 -> x
    OP70 -> x
    OP71 -> x
    OP72 -> x
    OP73 -> x
    OP74 -> x
    OP75 -> x
    OP76 -> x
    OP77 -> x
    OP78 -> x
    OP79 -> x
    OP80 -> x
    OP81 -> x
    OP82 -> x
    OP83 -> x
    OP84 -> x
    OP85 -> x
    OP86 -> x
    OP87 -> x
    OP88 -> x
    OP89 -> x
    OP90 -> x
    OP91 -> x
    OP92 -> x
    OP93 -> x
    OP94 -> x
    OP95 -> x
    OP96 -> x
    OP97 -> x
    OP98 -> x
    OP99 -> x
    OP100 -> x
    OP101 -> x
    OP102 -> x
    OP103 -> x
    OP104 -> x
    OP105 -> x
    OP106 -> x
    OP107 -> x
    OP108 -> x
    OP109 -> x
    OP110 -> x
    OP111 -> x
    OP112 -> x
    OP113 -> x
    OP114 -> x
    OP115 -> x
    OP116 -> x
    OP117 -> x
    OP118 -> x
    OP119 -> x
    OP120 -> x
    OP121 -> x
    OP122 -> x
    OP123 -> x
    OP124 -> x
    OP125 -> x
    OP126 -> x
    OP127 -> x
    OP128 -> x
    OP129 -> x
    OP130 -> x
    OP131 -> x
    OP132 -> x
    OP133 -> x
    OP134 -> x
    OP135 -> x
    OP136 -> x
    OP137 -> x
    OP138 -> x
    OP139 -> x
    OP140 -> x
    OP141 -> x
    OP142 -> x
    OP143 -> x
    OP144 -> x
    OP145 -> x
    OP146 -> x
    OP147 -> x
    OP148 -> x
    OP149 -> x
    OP150 -> x
    OP151 -> x
    OP152 -> x
    OP153 -> x
    OP154 -> x
    OP155 -> x
    OP156 -> x
    OP157 -> x
    OP158 -> x
    OP159 -> x
    OP160 -> x
    OP161 -> x
    OP162 -> x
    OP163 -> x
    OP164 -> x
    OP165 -> x
    OP166 -> x
    OP167 -> x
    OP168 -> x
    OP169 -> x
    OP170 -> x
    OP171 -> x
    OP172 -> x
    OP173 -> x
    OP174 -> x
    OP175 -> x
    OP176 -> x
    OP177 -> x
    OP178 -> x
    OP179 -> x
    OP180 -> x
    OP181 -> x
    OP182 -> x
    OP183 -> x
    OP184 -> x
    OP185 -> x
    OP186 -> x
    OP187 -> x
    OP188 -> x
    OP189 -> x
    OP190 -> x
    OP191 -> x
    OP192 -> x
    OP193 -> x
    OP194 -> x
    OP195 -> x
    OP196 -> x
    OP197 -> x
    OP198 -> x
    OP199 -> x
    OP200 -> x
    OP201 -> x
    OP202 -> x
    OP203 -> x
    OP204 -> x
    OP205 -> x
    OP206 -> x
    OP207 -> x
    OP208 -> x
    OP209 -> x
    OP210 -> x
    OP211 -> x
    OP212 -> x
    OP213 -> x
    OP214 -> x
    OP215 -> x
    OP216 -> x
    OP217 -> x
    OP218 -> x
    OP219 -> x
    OP220 -> x
    OP221 -> x
    OP222 -> x
    OP223 -> x
    OP224 -> x
    OP225 -> x
    OP226 -> x
    OP227 -> x
    OP228 -> x
    OP229 -> x
    OP230 -> x
    OP231 -> x
    OP232 -> x
    OP233 -> x
    OP234 -> x
    OP235 -> x
    OP236 -> x
    OP237 -> x
    OP238 -> x
    OP239 -> x
    OP240 -> x
    OP241 -> x
    OP242 -> x
    OP243 -> x
    OP244 -> x
    OP245 -> x
    OP246 -> x
    OP247 -> x
    OP248 -> x
    OP249 -> x
    OP250 -> succ x

mapFun :: M.Map Op (Int -> Int) -> Op -> Int -> Int
mapFun m op x =
  let f = m M.! op
   in f x

opMap :: M.Map Op (Int -> Int)
opMap = M.adjust (const succ) OP250 (M.fromList $ (,id) <$> [OP1 .. OP250])

vecFun :: V.Vector (Int -> Int) -> Op -> Int -> Int
vecFun v op x =
  let f = v V.! fromEnum op
   in f x

opVec :: V.Vector (Int -> Int)
opVec = V.fromList [id | _ <- [OP1 .. OP250]] V.// [(fromEnum OP250, succ)]
