FROM ocaml/opam:ubuntu-22.04

# Install system deps as root
USER root
RUN apt-get update && apt-get install -y \
    pkg-config \
    libgmp-dev \
    build-essential \
    git \
    curl \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Switch to opam user to install Rust and OCaml packages
USER opam
WORKDIR /home/opam

# Install Rust for opam user
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/home/opam/.cargo/bin:${PATH}"

# Install OCaml dependencies
RUN opam update && opam install -y yojson

# Set up workspace
WORKDIR /home/opam/computor
COPY --chown=opam:opam . .
