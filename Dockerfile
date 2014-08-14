FROM divarvel/archlinux-haskell:latest

RUN /usr/bin/useradd -m deploy

ADD . /home/deploy/scotty-short

RUN chown -R deploy:deploy /home/deploy/scotty-short

ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

USER deploy
ENV HOME /home/deploy
WORKDIR /home/deploy/scotty-short

RUN cabal update
 && cabal sandbox init
 && cabal install -j4 --only-dependencies
 && cabal build

EXPOSE 8080

ENTRYPOINT ["/home/deploy/scotty-short/dist/build/scotty-short/scotty-short"]
