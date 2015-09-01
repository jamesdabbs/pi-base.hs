FROM samdoshi/haskell-stack
MAINTAINER James Dabbs <jamesdabbs@gmail.com>

WORKDIR /app
ADD . /app

RUN stack setup
RUN stack build
