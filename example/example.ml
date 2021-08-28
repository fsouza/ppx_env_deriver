type person = { name : string; age : int }

(* TODO: I manually typed those below to validate the types generated in the signature. We need to generate the implementation now! *)

let person_of_env () =
  {
    name = Sys.getenv_opt "NAME" |> Option.value ~default:"";
    age = Sys.getenv_opt "AGE" |> Option.value ~default:"0" |> int_of_string;
  }

let env_of_person { name; age } = [ ("NAME", name); ("AGE", string_of_int age) ]
