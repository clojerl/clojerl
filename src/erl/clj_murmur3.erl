-module(clj_murmur3).

-export([ ordered/1
        , unordered/1
        ]).

-define(SEED, 0).
-define(C1, 16#cc9e2d51).
-define(C2, 16#1b873593).
-define(C3, 16#85ebca6b).
-define(C4, 16#c2b2ae35).
-define(C5, 16#e6546b64).

-define(MAX_UINT32, 16#FFFFFFFF). %% 32 bits
-define(UINT32(X), (X band ?MAX_UINT32)). %% Truncate to a 32 bit integer
-define(ROTL(X, R), (?UINT32(X bsl R) bor ?UINT32(X bsr ?UINT32(32 - R)))).

-spec ordered(any()) -> integer().
ordered(Seq) ->
  List = clj_rt:to_list(Seq),
  Hashes = [clj_rt:hash(X) || X <- List],
  murmur_ordered_coll(Hashes).

-spec unordered(any()) -> integer().
unordered(Seq) ->
  List = clj_rt:to_list(Seq),
  Hashes = [clj_rt:hash(X) || X <- List],
  murmur_unordered_coll(Hashes).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

murmur_ordered_coll(Hashes) ->
  Hash = do_murmur_ordered_coll(Hashes, 1),
  murmur_mix_coll_hash(Hash, length(Hashes)).

do_murmur_ordered_coll([], Hash) ->
  Hash;
do_murmur_ordered_coll([H | Hashes], Hash) ->
  do_murmur_ordered_coll(Hashes, ?UINT32(?UINT32(31 * Hash) + H)).

murmur_unordered_coll(Hashes) ->
  Hash = do_murmur_unordered_coll(Hashes, 1) ,
  murmur_mix_coll_hash(Hash, length(Hashes)).

do_murmur_unordered_coll([], Hash) ->
  Hash;
do_murmur_unordered_coll([H | Hashes], Hash) ->
  do_murmur_unordered_coll(Hashes, ?UINT32(Hash + H)).

murmur_mix_coll_hash(Hash, Len) ->
  K1 = mix_k1(Hash),
  H1 = mix_h1(?SEED, K1),
  fmix(H1, Len).

mix_k1(K1_0) ->
  K1_1 = ?UINT32(K1_0 * ?C1),
  K1_2 = ?UINT32(?ROTL(K1_1, 15)),
  ?UINT32(K1_2 * ?C2).

mix_h1(H1_0, K1_0) ->
  H1_1 = ?UINT32(H1_0 bxor K1_0),
  H1_2 = ?UINT32(?ROTL(H1_1, 13)),
  ?UINT32(?UINT32(H1_2 * 5) + ?C5).

fmix(H0, Len) ->
  H1 = ?UINT32(H0 bxor Len),
  H2 = ?UINT32(H1 bxor ?UINT32(H1 bsr 16)),
  H3 = ?UINT32(H2 * ?C3),
  H4 = ?UINT32(H3 bxor ?UINT32(H3 bsr 13)),
  H5 = ?UINT32(H4 * ?C4),
  ?UINT32(H5 bxor ?UINT32(H5 bsr 16)) .
