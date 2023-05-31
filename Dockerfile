FROM haskell:9.2.7

WORKDIR /usr/src/app

COPY ./lib/haskell/ /usr/src/app

WORKDIR /usr/src/app/natural4

RUN stack build
RUN stack install --local-bin-path .

