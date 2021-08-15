FROM erlang:24

# Install erlang-ls
RUN cd /tmp  \
        && git clone https://github.com/erlang-ls/erlang_ls.git \
        && cd erlang_ls \
        && make install
