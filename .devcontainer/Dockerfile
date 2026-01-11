# Use the OCaml opam image based on Ubuntu 22.04
FROM ocaml/opam:ubuntu-22.04

# Switch to root to install system packages
USER root

# Install system dependencies
# pkg-config and libgmp-dev are commonly needed for OCaml packages
# zathura and evince for PDF viewing
RUN apt-get update && apt-get install -y \
    pkg-config \
    libgmp-dev \
    build-essential \
    git \
    curl \
    unzip \
    zathura \
    evince \
    && rm -rf /var/lib/apt/lists/*

# Switch back to the opam user
USER opam

# Update opam and install development tools
RUN opam update && opam install -y \
    dune \
    ocaml-lsp-server \
    ocamlformat \
    utop

# Initialize opam environment in bashrc
RUN echo 'eval $(opam env)' >> /home/opam/.bashrc
