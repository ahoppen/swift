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

RUN apt-get -y install clangd clang-format
RUN ln -s /usr/include /include

COPY ./cmake /home/build-user/cmake
RUN chmod -R a+xr /home/build-user/cmake

USER build-user

WORKDIR /home/build-user

RUN mkdir -p /home/build-user/build
RUN mkdir -p /home/build-user/.cache/sccache
RUN mkdir -p /home/build-user/cmake-build/bin


ENV PATH=/home/build-user/.cargo/bin/:/home/build-user/cmake/bin:$PATH
ENV SCCACHE_CACHE_SIZE="20G"

COPY --from=sccache-builder /root/.cargo/bin/sccache /home/build-user/.cargo/bin/sccache