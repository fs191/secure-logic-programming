apt-get install -y \
        build-essential cmake pkg-config \
        ncurses-dev libreadline-dev libedit-dev \
        libgoogle-perftools-dev \
        libunwind-dev \
        libgmp-dev \
        libssl-dev \
        unixodbc-dev \
        zlib1g-dev libarchive-dev \
        libossp-uuid-dev \
        libxext-dev libice-dev libjpeg-dev libxinerama-dev libxft-dev \
        libxpm-dev libxt-dev \
        libdb-dev \
        libpcre3-dev \
        libyaml-dev \
        default-jdk junit4

git clone https://github.com/SWI-Prolog/swipl.git
cd swipl
git fetch --tags
git checkout V8.2.2
git submodule update --init
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/usr/local ..
make -j 9
make install
