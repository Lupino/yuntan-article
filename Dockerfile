FROM alpine:3.8

RUN apk update && apk add ghc curl libc-dev pcre-dev git mariadb-connector-c-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /data

COPY . /data

RUN stack install --local-bin-path bin --system-ghc

FROM alpine:3.8

RUN apk update && apk add pcre gmp libffi mariadb-connector-c

COPY --from=0 /data/bin/* /usr/bin/
COPY config.sample.yaml /config.yaml

ENTRYPOINT ["/usr/bin/yuntan-article"]

CMD ["-c", "/config.yaml"]
