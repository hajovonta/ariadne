FROM debian:stable-slim

RUN apt-get update && apt-get install -y sbcl curl && rm -rf /var/lib/apt/lists/*

# Install Quicklisp
RUN curl -sL https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp && \
    sbcl --noinform --non-interactive \
      --load /tmp/quicklisp.lisp \
      --eval '(quicklisp-quickstart:install)' && \
    rm /tmp/quicklisp.lisp

COPY . /root/quicklisp/local-projects/ariadne/

# Pre-load dependencies
RUN sbcl --noinform --non-interactive \
      --load /root/quicklisp/setup.lisp \
      --eval '(ql:quickload :ariadne :silent t)'

EXPOSE 8080

CMD ["sbcl", "--noinform", "--non-interactive", \
     "--load", "/root/quicklisp/setup.lisp", \
     "--load", "/root/quicklisp/local-projects/ariadne/docker-entrypoint.lisp"]
