# syntax=docker/dockerfile:1.4
FROM haskell:slim
WORKDIR /workspace

COPY stack.yaml stack.yaml.lock ./
RUN <<EOF
  echo 'name: dummy' > package.yaml
  stack setup
EOF
