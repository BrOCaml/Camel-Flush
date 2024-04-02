**Prerequsite:** 
Ocaml, Opam, Utop, dune, pip, python 3.10, Unix OS

**Install libraries**
```bash
pip install -r requirements.txt
opam install bisect_ppx ppx_inline_test ounit2 
```

**Execute program**
At the scope of the project folder
```bash
dune exec bin/main.exe
```

**Note**
Terminal must support Unicode format (UTF-8)