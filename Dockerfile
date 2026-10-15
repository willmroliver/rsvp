FROM ocaml/opam:alpine

USER root
RUN mkdir -p /data/rsvp
RUN mkdir -p /usr/app/rsvp
RUN mkdir -p /usr/build/rsvp
RUN chown -R opam /usr/build/rsvp
RUN chown -R opam /data/rsvp

USER opam
WORKDIR /usr/app/rsvp
RUN opam install dune
RUN opam install dream tyxml petrol caqti-driver-sqlite3

CMD opam exec -- dune exec bin/main.exe
