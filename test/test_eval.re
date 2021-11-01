open OUnit2;
open Rresult;
open Lwt.Infix;

open Mesh.Eval;

let value_of_json = (path) =>
  Yojson.Basic.from_file(path)
  |> value_of_yojson;

let standardize = (value) => {
  let rec standardize = (value) =>
    switch (value) {
    | VRecord(fields) => 
      let fields' = List.map(((name, value)) => (name, standardize(value)), fields);
      VRecord(List.sort(((name, _), (name', _)) => String.compare(name, name'), fields'))
    | VList(values) => VList(List.map(standardize, values))
    | VTuple(values) => VTuple(List.map(standardize, values))
    | VOpt(Some(value)) => VOpt(Some(standardize(value)))
    | _ => value
    };
  
  R.map((values) =>
    List.map(standardize, values),
    value
  );  
};
  

let test_cases = [
  // Literals
  ("1;",                                    [VInt(1)]),
  ("\"hello\";",                            [VString("hello")]),

  // Simple identity function
  ("let f = a => a; f(0);",                 [VInt(0)]),

  // Function with let binding
  ("let f = (x) => {
      let y = 0;
      y
    };
    f(\"hello\");",                         [VInt(0)]),

  // Variable overshadowing
  ("let x = true;
    let x = 10;
    x;",                                    [VInt(10)]),

  ("let f = (x) => {
      let x = 0;
      x
    };
    f(\"hello\");",                         [VInt(0)]),

  // Destructuring
  ("let (a, b) = (0, 1); a;",               [VInt(0)]),
  
  // Argument pattern
  ("let first = ((a, b)) => a;
    first((0, 1));
    first((\"hello\", 10));",               [VInt(0), VString("hello")]),


  // Modules
  ("module M = {
      let x = 2;
    };
    M.x;",                                  [VInt(2)]),
  ("module M = {
      let x = 2;
      let y = 5;
      let f = (a, b) => b;
    };
    M.f(M.x, \"hello\");",                  [VString("hello")]),
  ("module M = {
      let x = 2;
      let y = 5;
      let f = (a, b) => b;
    };
    open M;
    f(x, \"hello\");",                      [VString("hello")]),
    
  // External functions
  ("external f = \"list_cons\";
    f(1, [2, 3]);",                         [VList([VInt(1), VInt(2), VInt(3)])]),
  ("external f = \"int_add\";
    f(1, 2);",                              [VInt(3)]),
  ("external f = \"int_sub\";
    f(1, 2);",                              [VInt(-1)]),
  ("external f = \"int_mul\";
    f(1, 2);",                              [VInt(2)]),
  ("external f = \"int_div\";
    f(4, 2);",                              [VInt(2)]),
  ("external f = \"float_add\";
    f(1., 2.);",                            [VFloat(3.)]),
  ("external f = \"float_sub\";
    f(1., 2.);",                            [VFloat(-1.)]),
  ("external f = \"float_mul\";
    f(1., 2.);",                            [VFloat(2.)]),
  ("external f = \"float_div\";
    f(4., 2.);",                            [VFloat(2.)]),

  // Operator binding
  ("external add = \"int_add\";
    let (+) = add;
    1 + 2;",                                [VInt(3)]),
  ("external add = \"int_add\";
    let (.++) = add(1);
    ++2;",                                  [VInt(3)]),

  // Records
  ("{};",                                   [VRecord([])]),
  ("{a: 1, b: 2, c: 3};",                   [VRecord([("c", VInt(3)), ("b", VInt(2)), ("a", VInt(1))])]),
  ("let r = {a: 1, b: 2};
    {...r, c: 3};",                         [VRecord([("c", VInt(3)), ("b", VInt(2)), ("a", VInt(1))])]),
  ("{a: 1, b: 2, a: 3};",                   [VRecord([("a", VInt(3)), ("b", VInt(2))])]),
  ("let r = {a: 1, b: 2};
    {...r, a: 3};",                         [VRecord([("a", VInt(3)), ("b", VInt(2))])]),
  ("let r = {a: 1, b: 2};
    r.a;",                                  [VInt(1)]),
  ("let r = {a: 1, b: 2};
    let r2 = {...r, a: 3};
    r2.a;",                                 [VInt(3)]),

  // Stdlib
  ("1 + 2;",                                [VInt(3)]),
  ("3 - 2;",                                [VInt(1)]),
  ("3 * 2;",                                [VInt(6)]),
  ("4 / 2;",                                [VInt(2)]),

  ("1.0 +. 2.0;",                           [VFloat(3.0)]),
  ("3.0 -. 2.0;",                           [VFloat(1.0)]),
  ("3.0 *. 2.0;",                           [VFloat(6.0)]),
  ("3.0 /. 2.0;",                           [VFloat(1.5)]),

  ("let l = [1, 2];
    [0, ...l];",                            [VList([VInt(0), VInt(1), VInt(2)])]),

  ("let f = (x) => x - 1;
    3 |> f;",                               [VInt(2)]),

  ("let l = [1, 2, 3];
    let f = (x) => x + 1;
    List.map(f, l);",                       [VList([VInt(2), VInt(3), VInt(4)])]),

  ("let l = [1, 2, 3];
    List.map((x) => x + 1, l);",            [VList([VInt(2), VInt(3), VInt(4)])]),

  ("let l = [1, 2, 3];
    let f = (i, x) => x + i;
    List.mapi(f, l);",                      [VList([VInt(1), VInt(3), VInt(5)])]),

  ("let l = [1, 2, 3];
    List.foldl((acc, x) => acc + x, 0, l);",                      
    [VInt(6)]),

  ("let l = [1, 2, 3];
    List.foldr((x, acc) => acc + x, l, 0);",                      
    [VInt(6)]),

  // Options
  ("Option.some(1);", 
    [VOpt(Some(VInt(1)))]),
  
  ("Option.none;", 
    [VOpt(None)]),
  
  ("let maybe_int = Option.some(1);
    Option.get(10, maybe_int);", 
    [VInt(1)]),

  ("let maybe_int = Option.none;
    Option.get(10, maybe_int);", 
    [VInt(10)]),

  // Graphql
  ("Graphql.execute(```graphql(https://countries.trevorblades.com/)
      query {
        country(code: \"BR\") {
          name
        }
      }
    ```);",
    [VRecord([("country", VOpt(Some(VRecord([("name", VString("Brazil"))]))))])]),
  ("Graphql.execute(```graphql(https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v2)
      query {
        pairs(first: 1) {
          id
        }
      }
    ```);",
    [VRecord([("pairs", VList([VRecord([("id", VString("0x00004ee988665cdda9a1080d5792cecd16dc1220"))])]))])]),
  ("Graphql.execute(```graphql(https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v2)
      query {
        pairs(first: 1) {
          token0 {
            symbol
          }
          id
        }
      }
    ```);",
    [VRecord([("pairs", VList([VRecord([
      ("id", VString("0x00004ee988665cdda9a1080d5792cecd16dc1220")),
      ("token0", VRecord([("symbol", VString("SLC"))]))
    ])]))])]),
  ({|Graphql.execute(```graphql(https://api.thegraph.com/subgraphs/name/graphprotocol/graph-network-mainnet)
      query {
        subgraph(id: "0x673b6e9fe607f6ddf4a4f25b386b846c5c82995e-2") {
          id
          displayName
          image
          currentVersion {
            id
            subgraphDeployment {
              id
            }
          }
        }
      }
    ```);|},
    [VRecord([("subgraph", VOpt(Some(VRecord([
      ("id", VString("0x673b6e9fe607f6ddf4a4f25b386b846c5c82995e-2")),
      ("displayName", VOpt(Some(VString("PoolTogether")))),
      ("image", VOpt(Some(VString("https://ipfs.network.thegraph.com/api/v0/cat?arg=QmUKg8NDZKgfF2aB7aHJ4xiz9rzLUZP1RHcAjRKG6rG8oz")))),
      ("currentVersion", VOpt(Some(VRecord([
        ("id", VString("0x673b6e9fe607f6ddf4a4f25b386b846c5c82995e-2-0")),
        ("subgraphDeployment", VRecord([("id", VString("0x666d78959706d7e1ed20befeb1d2c9a4e512a22931d8e22928b1f63ce48fcb40"))]))
      ])))),
    ]))))])]),
  ("Graphql.execute(```graphql(https://api.thegraph.com/subgraphs/name/convex-community/convex-votium)
      query {
        bribes {
          id
          epoch {
            id
          }
          token
          amount
        }
      }
  ```);",
    [value_of_json([%string {|%{Sys.getenv("TEST_DIR")}/subgraph-test-data1.json|}])])
] |> List.map(((mesh_src, expected)) => (mesh_src, R.ok(expected)));

let pp_value = (values) => 
  switch (values) {
  | Error(`Msg(msg)) => msg
  | Ok(values) =>
    List.map(string_of_value, values)   |> (s) =>
    "\n" ++ String.concat(",\n", s);
  };

let make_single_test = ((mesh_src, expected)) =>
  String.escaped(mesh_src) >:: OUnitLwt.lwt_wrapper((_) => 
    Lwt_result.map(fst, Mesh.parse_eval(mesh_src)) >|= (values) => 
    assert_equal(~printer=pp_value, standardize(expected), standardize(values))
  );

let suite = 
  "test_eval" >::: List.map(make_single_test, test_cases);