**Prerequsite:** 
Ocaml, Opam, Utop, dune

**Install libraries**
```bash
opam install bisect_ppx ppx_inline_test ounit2 
```

**Execute program**
At the scope of the project folder
```bash
dune exec bin/main.exe
```

**Note**
Terminal must support Unicode format (UTF-8)