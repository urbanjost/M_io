## M_uuid Changelog

The intent of this log is to keep everyone in the loop about what's new
in the M_uuid  project. It is a curated, chronologically ordered list
of notifications of notable events such as bug fixes, new features,
and usage changes.

"Do unto others as you would have them do unto you", as they say. When I
find OS (Open Source) resources, I am hoping a lot of these boxes can be
checked ...
   - [x] git repository on WWW (github)
   - [x] annotated source files with an open license
   - [x] unit test
   - [x] make(1) build
   - [x] fpm(1) build
   - [x] user manual (on-line)
   - [x] man-page
   - [x] app program
   - [x] demo program for public procedures
   - [x] developer documents (ford(1))
   - [x] CI/CD(Continious Integration/Development) verification (github actions)
   - [x] registered in fpm(1) repository

---
**2023-01-16**  John S. Urban  <https://github.com/urbanjost>

### :green: ADD:
     + added IOSTAT option to GETLINE(3f)
### :red_circle: FIX:
     + READ_LINE(3f) said it replaced non-printable characters
       with a space but it did not. Now it does.
---
**2023-01-15**  John S. Urban  <https://github.com/urbanjost>

### :green: ADD:
     + added GENERATE_FILENAME(3f).
---
**2022-01-07**  John S. Urban  <https://github.com/urbanjost>

### :orange_circle: DIFF:
     + deprecated names SLURP and GULP for preferred FILEREAD and
       FILEBYTE names.
---
**2022-01-06**  John S. Urban  <https://github.com/urbanjost>

### :red_circle: FIX:
     + comment= option on read_table(3f) was not functioning, only removing
       the comment character itself.
---
**2020-02-04**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
     initial release on github
---
<!--
### :orange_circle: DIFF:
       + renamed ADVICE(3f) to ALERT(3f)
### :green_circle: ADD:
       + advice(3f) was added to provide a standardized message format simply.
### :red_circle: FIX:
       + </bo> did not work on several terminal types, changed it to a more
         universally accepted value.
-->
