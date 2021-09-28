(* Printexc.record_backtrace true *)

exception InconsistentArgs of string


let optimisation_flag = ref false
let in_filename = ref ""
let i_flag = ref false
let f_flag = ref false

let i_fun () =
  if !f_flag
  then raise @@ InconsistentArgs "-i and -f options should not be specified together."
  else i_flag := true

let f_fun () =
  if !i_flag
  then raise @@ InconsistentArgs "-i and -f options should not be specified together."
  else f_flag := true

let anon_fun s =
  match !in_filename with
  | "" ->
    in_filename := s;
  | _  -> raise @@ InconsistentArgs "More than one input files provided."

let speclist =
  [("-O", Arg.Set optimisation_flag, "Enable optimisation");
   ("-i", Arg.Unit i_fun, "Program in standard input, intermediate code in standard output.");
   ("-f", Arg.Unit f_fun, "Program in standard input, final code in standard output.")]


let main () =
  begin
    try
      Arg.parse speclist anon_fun "pcl {<file> | -i | -f} [-O]";
      (* Check for mutual exclusion of args *)
      if !in_filename != "" && (!i_flag || !f_flag)
      then raise @@ InconsistentArgs
        "Cannot specify either of -i, -f options when providing an input file because \
        with either of these option the program reads from standard input"
    with InconsistentArgs e ->
      Printf.eprintf "Worng usage: %s" e;
      exit 1
  end;
  let cin =
    if !i_flag || !f_flag
    then stdin
    else open_in !in_filename
  in
  let lexbuf = Lexing.from_channel cin in
  try
    let ast = Parser.program Lexer.lexer lexbuf in
    let sem_ast = SemAnalysis.typecheck_ast ast in
    let the_module = Codegen.codegen sem_ast in
    let verification = Llvm_analysis.verify_module the_module in
    if Option.is_some verification
    then
      begin
        print_endline "Llvm error: Verification of module failed:";
        print_string @@ Option.get verification;
        exit 1
      end;
    if !i_flag
    then print_endline @@ Llvm.string_of_llmodule the_module
    else
      let target_machine =
        (* Llvm_X86.initialize ();  This doesn't work for some reason *)
        Llvm_all_backends.initialize ();
        let default_triple = Llvm_target.Target.default_triple () in
        let target = Llvm_target.Target.by_triple default_triple in
        Llvm_target.TargetMachine.create ~triple:default_triple target
      in
      let asm_filetype = Llvm_target.CodeGenFileType.AssemblyFile in
      if !optimisation_flag
      then
        begin
          let pm = Llvm.PassManager.create () in
          Llvm_scalar_opts.add_memcpy_opt pm;
          Llvm_scalar_opts.add_memory_to_register_promotion pm;
          Llvm_scalar_opts.add_constant_propagation pm;
          Llvm_scalar_opts.add_instruction_combination pm;
          Llvm_scalar_opts.add_cfg_simplification pm;
          Llvm_ipo.add_function_inlining pm;
          ignore @@ Llvm.PassManager.run_module the_module pm
        end;
      if !f_flag
      then
        let memory_buffer =
          Llvm_target.TargetMachine.emit_to_memory_buffer the_module asm_filetype target_machine
        in
        print_endline @@ Llvm.MemoryBuffer.as_string memory_buffer;
      else
        let filename =
          match String.split_on_char '.' !in_filename with
          | name::_ -> name
          | [] ->
            failwith "This should be unreachable. Splitting filename on '.' and getting empty list"
        in
        Llvm.print_module (filename ^ ".imm") the_module;
        Llvm_target.TargetMachine.emit_to_file the_module asm_filetype (filename ^ ".asm") target_machine;
        let cmd = Printf.sprintf "clang %s %s -o %s" (filename ^ ".asm") "lib.a" filename in
        let status = Sys.command cmd in
        if status != 0
        then
          print_endline "Failed to link object file with library. \
                         Clang is required for this. Maybe it needs to be installed.";
          exit 6
  with
  | Lexer.Lexical_error (msg, _filename, line_num, col_num) ->
    Printf.eprintf "Lexical error: %s\nLine: %d, Column: %d\n" msg line_num col_num;
    exit 2
  | Parsing.Parse_error | Parser.Error -> 
    Printf.eprintf "Syntax error: %s\n" (Lexing.lexeme lexbuf);
    exit 3
  | SemAnalysis.SemanticError msg ->
    Printf.eprintf "Semantic error: %s\n" msg;
    exit 4


let _ = main ()
