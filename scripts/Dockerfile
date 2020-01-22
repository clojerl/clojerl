FROM erlang:latest

RUN git clone https://github.com/clojerl/clojerl && \
        cd clojerl                               && \
        make                                     && \
        apt-get update                           && \
        apt-get install rlwrap                   && \
        apt-get clean                            && \
        apt-get autoclean

CMD sleep 0.1; /clojerl/bin/clje
