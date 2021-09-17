FROM ubuntu:20.04 as sccache-builder
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get -y update && apt-get -y install cargo openssl libssl-dev pkg-config
RUN cargo install sccache

# FROM swift:focal
FROM ubuntu:20.04

RUN groupadd -g 998 build-user && \
    useradd -m -r -u 998 -g build-user build-user

ENV DEBIAN_FRONTEND="noninteractive"

RUN apt-get -y update && apt-get -y install \
  build-essential       \
  clang                 \
  cmake                 \
  git                   \
  icu-devtools          \
  libcurl4-openssl-dev  \
  libedit-dev           \
  libicu-dev            \
  libncurses5-dev       \
  libpython3-dev        \
  libsqlite3-dev        \
  libxml2-dev           \
  ninja-build           \
  pkg-config            \
  python                \
  python-six            \
  python2-dev           \
  python3-six           \
  python3-distutils     \
  rsync                 \
  swig                  \
  systemtap-sdt-dev     \
  tzdata                \
  unzip                 \
  uuid-dev

RUN apt-get -y install clangd clang-format lldb
RUN mkdir -p /workspaces
RUN chmod a+w /workspaces

USER build-user

WORKDIR /home/build-user

RUN mkdir -p /home/build-user/.cache/sccache
COPY --from=sccache-builder /root/.cargo/bin/sccache /home/build-user/.cargo/bin/sccache
ENV PATH=/home/build-user/.cargo/bin/:$PATH
ENV SCCACHE_CACHE_SIZE="20G"
