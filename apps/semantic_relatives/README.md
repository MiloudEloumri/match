semantic_relatives
=====

Semantic Relatives application uses [SERESYE](https://github.com/MiloudEloumri/match/tree/main/apps/seresye) and [Semantic Web ToolKit for Erlang applications](https://github.com/fogfish/semantic) to read and process semantic web data. It also saves the SERESYE KB in Mnesia.  

Build
-----
    $ rebar3 compile
Run
-----
    $ rebar3 shell
    semantic_relatives_mnesia:start().
    seresye_srv:get_kb(semantic_relatives_mnesia).
SERESYE KB Queries Examples
-----
    seresye_srv:query_kb(semantic_relatives_mnesia, {brother, '_', alice}).
    seresye_srv:query_kb(semantic_relatives_mnesia, {brother, '_', fun (X) -> (X == alice) or (X == anna) end}).
    seresye_srv:query_kb(semantic_relatives_mnesia, {grandfather, '_', caesar}).
    seresye_srv:query_kb(semantic_relatives_mnesia, {grandfather, '_', fun (X) -> (X == caesar) or (X == anna) end}).
    seresye_srv:query_kb(semantic_relatives_mnesia, {mother, '_', alice}).
    seresye_srv:query_kb(semantic_relatives_mnesia, {father, '_', alice}).
    seresye_srv:query_kb(semantic_relatives_mnesia, {brother, fun (X) -> (X == bob) or (X == mark) end, '_'}).
Mnesia Queries Examples
-----
    semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, '_', alice}).
    semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {grandfather, '_', caesar}).
    semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {mother, '_', alice}).
    semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {father, '_', alice}).
	semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, '_', fun (X) -> (X == alice) or (X == anna) end}).
	semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {grandfather, '_', fun (X) -> (X == caesar) or (X == anna) end}).
	semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, fun (X) -> (X == bob) or (X == mark) end, '_'}).
Results
-----
    milou@HP122021 MINGW64 ~/match (main)
$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling seresye
===> Compiling match
===> Compiling semantic_relatives
===> Analyzing applications...
===> Compiling extra_apps/seresye/examples
Eshell V11.0  (abort with ^G)
1> ===> Booted match
1> ===> Booted semantic
1> ===> Booted cf
1> ===> Booted erlware_commons
1> ===> Booted seresye
1> ===> Booted semantic_relatives
1> ===> Booted sasl
1> semantic_relatives_mnesia:start().
ok
2> seresye_srv:get_kb(semantic_relatives_mnesia).
[{grandfather,corrado,caesar},
 {grandfather,corrado,anna},
 {father,corrado,bob},
 {parent,corrado,bob},
 {sister,anna,caesar},
 {female,anna},
 {father,corrado,alice},
 {father,corrado,mark},
 {male,corrado},
 {parent,corrado,alice},
 {grandmother,jane,caesar},
 {grandmother,jane,anna},
 {mother,jane,bob},
 {mother,jane,mark},
 {mother,jane,alice},
 {female,jane},
 {mother,sara,anna},
 {parent,sara,anna},
 {brother,mark,bob},
 {brother,bob,mark},
 {sister,alice,mark},
 {brother,mark,alice},
 {parent,jane,mark},
 {mother,sara,caesar},
 {female,sara},
 {parent,sara,caesar},
 {male,mark},
 {brother,...},
 {...}|...]
3> seresye_srv:query_kb(semantic_relatives_mnesia, {brother, '_', alice}).
[{brother,mark,alice},{brother,bob,alice}]
4> seresye_srv:query_kb(semantic_relatives_mnesia, {brother, '_', fun (X) -> (X == alice) or (X == anna) end}).
[{brother,mark,alice},
 {brother,caesar,anna},
 {brother,bob,alice}]
5> seresye_srv:query_kb(semantic_relatives_mnesia, {grandfather, '_', caesar}).
[{grandfather,corrado,caesar}]
6> seresye_srv:query_kb(semantic_relatives_mnesia, {grandfather, '_', fun (X) -> (X == caesar) or (X == anna) end}).
[{grandfather,corrado,caesar},{grandfather,corrado,anna}]
7> seresye_srv:query_kb(semantic_relatives_mnesia, {mother, '_', alice}).
[{mother,jane,alice}]
8> seresye_srv:query_kb(semantic_relatives_mnesia, {father, '_', alice}).
[{father,corrado,alice}]
9> seresye_srv:query_kb(semantic_relatives_mnesia, {brother, fun (X) -> (X == bob) or (X == mark) end, '_'}).
[{brother,mark,bob},
 {brother,bob,mark},
 {brother,mark,alice},
 {brother,bob,alice}]
10> semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, '_', alice}).
[{brother,mark,alice},{brother,bob,alice}]
11> semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {grandfather, '_', caesar}).
[{grandfather,corrado,caesar}]
12> semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {mother, '_', alice}).
[{mother,jane,alice}]
13> semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {father, '_', alice}).
[{father,corrado,alice}]
14> semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, '_', fun (X) -> (X == alice) or (X == anna) end}).
[{brother,mark,alice},
 {brother,caesar,anna},
 {brother,bob,alice}]
15> semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {grandfather, '_', fun (X) -> (X == caesar) or (X == anna) end}).
[{grandfather,corrado,caesar},{grandfather,corrado,anna}]
16> semantic_relatives_mnesia_handler:query_kb(semantic_relatives_kb, {brother, fun (X) -> (X == bob) or (X == mark) end, '_'}).
[{brother,mark,bob},
 {brother,bob,mark},
 {brother,mark,alice},
 {brother,bob,alice}]
17>
