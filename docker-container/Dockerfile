FROM fedora:35

ENV CHICKEN_VERSION 5.3.0
ENV PLATFORM linux

RUN dnf install -y make gcc gcc-c++ diffutils && dnf clean all

RUN curl -sS https://code.call-cc.org/releases/${CHICKEN_VERSION}/chicken-${CHICKEN_VERSION}.tar.gz | tar xzv; \
    cd /chicken-${CHICKEN_VERSION}; \
    make PLATFORM=${PLATFORM}; \
    make PLATFORM=${PLATFORM} install; \
    make PLATFORM=${PLATFORM} check; \
    cd /; \
    rm -rf /chicken-${CHICKEN_VERSION}

WORKDIR /code

RUN dnf install -y openssl-devel && dnf clean all
RUN chicken-install openssl http-client intarweb uri-common json srfi-133 message-digest-utils sha2 iterators simple-loops srfi-69 srfi-135
