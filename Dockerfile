FROM heroku/cedar:14

# Install stack

ENV LANG C.UTF-8

RUN echo 'deb http://download.fpcomplete.com/debian jessie main' > /etc/apt/sources.list.d/fpco.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 && \
    apt-get update && \
    apt-get install -y --no-install-recommends stack \
            ca-certificates g++ && \
    rm -rf /var/lib/apt/lists/*


# Build and install

ENV HOME /opt
WORKDIR /opt


# Cache build dependencies, as they should change less often

COPY stack.yaml stack.yaml
COPY pi-base.cabal pi-base.cabal
RUN stack setup
RUN stack build --only-dependencies


# Build the app proper

COPY . /opt
RUN stack install


# And move it to a Heroku-compatible format

RUN useradd -d /app -m app
USER app

ENV HOME /app
ENV PORT 3000

WORKDIR /app
RUN mkdir -p /app/user
RUN cp /opt/.local/bin/pi-base /app/user/pi-base