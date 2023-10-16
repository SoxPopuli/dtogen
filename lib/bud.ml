open Format
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Json = struct
  let get_assoc (x: Yojson.Safe.t) = 
    match x with
    | `Assoc x -> x
    | `Bool _ -> Failure "wrong type: Bool" |> raise
    | `Float _ -> Failure "wrong type: Float" |> raise
    | `Int _ -> Failure "wrong type: Int" |> raise
    | `Intlit _ -> Failure "wrong type: Intlit" |> raise
    | `List _ -> Failure "wrong type: List" |> raise
    | `Null -> Failure "wrong type: Null" |> raise
    | `String _ -> Failure "wrong type: String" |> raise
    | `Tuple _ -> Failure "wrong type: Tuple" |> raise
    | `Variant _ -> Failure "wrong type: Variant" |> raise
end

module Semver = struct
  type t =
    { major : int
    ; minor : int
    ; patch : int
    }

  let t_of_yojson x =
    let x =
      match x with
      | `String (s : string) -> s
      | _ -> raise (Failure "wrong type")
    in
    let parts = String.split_on_char '.' x in
    match parts with
    | [ major; minor; patch ] ->
      let major = int_of_string major in
      let minor = int_of_string minor in
      let patch = int_of_string patch in
      { major; minor; patch }
    | _ -> { major = 0; minor = 0; patch = 0 }
  ;;

  let yojson_of_t x =
    let str = sprintf "%d.%d.%d" x.major x.minor x.patch in
    `String str
  ;;
end

module Endpoint = struct
  type get =
    { tag: string array
    ; summary: int
    } [@@deriving yojson]

  type post

  type t =
    { name : string
    ; get : get option
    ; post : post option
    }

  type endpoints = t list

  module ElementMap = Map.Make(String)

  let parse_get (json: Yojson.Safe.t) =
    let map = ElementMap.empty in

    Failure "todo" |> raise

  let endpoints_of_yojson (json : Yojson.Safe.t) : endpoints =
    let paths =
      Json.get_assoc json
      |> List.map (fun (name, _elem) -> { name; get = None; post = None })
    in
    paths
  ;;

  let yojson_of_endpoints _x = `String "hi"
end

type contact =
  { name : string
  ; url : string
  ; email : string
  }
[@@deriving yojson]

type logo = { url : string } [@@deriving yojson]

type info =
  { version : Semver.t
  ; title : string
  ; contact : contact
  ; logo : logo [@key "x-logo"]
  ; description : string
  }
[@@deriving yojson]

type tag =
  { name : string
  ; display_name : string [@key "x-displayName"]
  ; description : string
  }
[@@deriving yojson]

type readme = { explorer_enabled : bool [@key "explorer-enabled"] } [@@deriving yojson]

type server =
  { url : string
  ; description : string
  }
[@@deriving yojson]

type schema =
  { openapi : Semver.t
  ; info : info
  ; tags : tag array
  ; readme : readme [@key "x-readme"]
  ; servers : server array
  ; paths : Endpoint.endpoints
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]
