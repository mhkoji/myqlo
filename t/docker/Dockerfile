FROM ubuntu:18.04

RUN apt update && apt install -y \
    libmysqlclient-dev \
    sbcl \
    wget && \
    ##
    mkdir \
    /app \
    /build && \
    ##
    cd /build && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load /build/quicklisp.lisp \
         --eval "(quicklisp-quickstart:install)"

COPY . /root/quicklisp/local-projects/myqlo
RUN sbcl --noinform \
         --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load /root/quicklisp/setup.lisp \
         --load /root/quicklisp/local-projects/myqlo/t/docker/main.lisp \
         --eval "(sb-ext:save-lisp-and-die \
                  \"/app/test\" \
                  :executable t \
                  :toplevel #'myqlo-test.docker:main)"

ENTRYPOINT /app/test
